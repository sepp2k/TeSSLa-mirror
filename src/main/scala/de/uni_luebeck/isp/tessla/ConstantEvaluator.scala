package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.LazyWithStack
import TesslaAST.{Core, Identifier, Typed}
import cats._
import cats.implicits._

import scala.collection.immutable.ArraySeq

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TranslationPhase[Typed.Specification, Core.Specification] {
  override def translate(spec: Typed.Specification) = {
    new ConstantEvaluatorWorker(spec, baseTimeUnit).translate()
  }
}

class ConstantEvaluatorWorker(spec: Typed.Specification, baseTimeUnit: Option[TimeUnit])
  extends TranslationPhase.Translator[Core.Specification] {

  type Env = Map[Identifier, TranslationResult[Any, Some]]
  type TypeEnv = Map[Identifier, Core.Type]

  class TranslatedExpressions {
    val expressions = mutable.Map[Identifier, Core.Expression]()
    val deferredQueue = mutable.Queue[() => Unit]()

    def complete(): Unit = {
      while (deferredQueue.nonEmpty) {
        deferredQueue.dequeue()()
      }
    }
  }

  type Stack = List[Location]

  val lazyWithStack = new LazyWithStack[Stack] {
    override def call[A](stack: Stack, rec: Boolean)(a: Stack => A) = {
      if (stack.size > 1000) {
        throw WithStackTrace(Errors.StackOverflow(stack.head), stack.tail)
      } else if (rec) {
        throw WithStackTrace(InfiniteRecursion(stack.head), stack.tail)
      } else {
        try {
          a(stack)
        } catch {
          case err: InternalError =>
            throw WithStackTrace(err, stack)
          case _: StackOverflowError =>
            throw WithStackTrace(Errors.StackOverflow(stack.head), stack.tail)
        }
      }
    }
  }

  import lazyWithStack._

  case class StrictOrLazy(translateStrict: StackLazy[Core.ExpressionArg], translateLazy: StackLazy[Core.ExpressionArg])

  // Expression that can be either an expression (left) or a reference to an expression or an literal (right).
  // A reference (or a literal) can be requested with strict or lazy translation.
  // When lazy translation is requested, the translation of a a referenced expression will be deferred.
  type ExpressionOrRef = Either[StackLazy[Core.Expression], StrictOrLazy]

  case class TranslationResult[+A, +B[+ _]](value: StackLazy[Option[A]], expression: StackLazy[B[ExpressionOrRef]])

  case class Translatable[+A, +B[+ _]](translate: TranslatedExpressions => TranslationResult[A, B])

  type TranslatableMonad[+A] = Translatable[A, Option]

  def getExpressionArgStrict(result: TranslationResult[Any, Some]) =
    result.expression.flatMap(_.value match {
      case Left(expression) => expression
      case Right(expresssionRef) => expresssionRef.translateStrict
    })

  def mkLiteralResult(value: Option[Any], expression: Core.Expression) =
    TranslationResult[Any, Some](Lazy(value), Lazy(Some(Right(StrictOrLazy(Lazy(expression), Lazy(expression))))))

  // TODO: reify values here
  def reified(result: TranslationResult[Any, Option], tpe: Core.Type): TranslationResult[Any, Some] =
    TranslationResult[Any, Some](result.value, result.expression.flatMap {
      case Some(e) => Lazy(Some(e))
      case None => result.value.map { v =>
        Some(Left(Lazy(v.map(reify(_, tpe)).getOrElse(throw InternalError("No value available for reification.")))))
      }
    })

  type Reifier = (Any, List[Core.Type]) => (List[(Any, Core.Type)], List[Core.ExpressionArg] => Core.Expression)

  val reifiers = Map[String, Reifier](
    "Int" -> ((value, _) => (Nil, _ => Core.IntLiteralExpression(value.asInstanceOf[BigInt])))
  )

  def reify(value: Any, tpe: Core.Type): Core.Expression = tpe match {
    case Core.InstatiatedType(name, typeArgs, _) =>
      val reifier = reifiers.getOrElse(name, throw InternalError(s"No reifier found for $name."))
      val (subValues, reification) = reifier(value, typeArgs)
      reification(subValues.map(x => reify(x._1, x._2)))
    case _ => throw InternalError(s"Could not reify value of type $tpe.")
  }

  /*type Reifier = (String, Any, Core.Type) => (List[Core.ExpressionArg] => Core.Expression, List[Any])

  val reifiers = Map[String, Reifier]()

  def reify(value: Any, tpe: Core.Type): (Core.Expression, Boolean) = {
    ???
  }*/

  type TypeApplicable = List[Core.Type] => Applicable
  type Applicable = ArraySeq[Translatable[Any, Option]] => Translatable[Any, Some]

  type TypeExtern = List[Core.Type] => Extern
  type Extern = ArraySeq[TranslatableMonad[Any]] => TranslatableMonad[Any]


  val errorExtern = Core.ExternExpression(
    List(Identifier("A")),
    List((Identifier("msg"), TesslaAST.LazyEvaluation, Core.StringType)),
    Core.TypeParam(Identifier("A")),
    "error"
  )

  def mkErrorResult(error: RuntimeEvaluator.RuntimeError, tpe: Core.Type) = {
    val expression = Core.ApplicationExpression(
      Core.TypeApplicationExpression(errorExtern, List(tpe)),
      ArraySeq(Core.StringLiteralExpression(error.msg)))
    TranslationResult[Any, Some](Lazy(Some(error)), Lazy(Some(Left(Lazy(expression)))))
  }

  val noExtern: TypeExtern = _ => _ => Translatable[Any, Option](_ => TranslationResult[Any, Option](Lazy(None), Lazy(None)))

  val streamExterns: Map[String, TypeExtern] = List("last", "slift", "default", "lift", "nil", "time",
    "delay", "defaultFrom", "merge").map(_ -> noExtern).toMap

  implicit val translatableMonad: Monad[TranslatableMonad] = new Monad[TranslatableMonad] {
    override def flatMap[A, B](fa: TranslatableMonad[A])(f: A => TranslatableMonad[B]) = Translatable { translatedExpressions =>
      val result = fa.translate(translatedExpressions).value.map(_.map(a => f(a).translate(translatedExpressions)))
      val value = result.flatMap(_.traverse(_.value)).map(_.flatten)
      val expression = result.flatMap(_.traverse(_.expression)).map(_.flatten)
      TranslationResult(value, expression)
    }

    override def tailRecM[A, B](a: A)(f: A => TranslatableMonad[Either[A, B]]): TranslatableMonad[B] =
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }

    override def pure[A](x: A): TranslatableMonad[A] =
      Translatable(_ => TranslationResult[A, Option](Lazy(Some(x)), Lazy(None)))
  }

  val valueExterns: Map[String, TypeExtern] = ValueExterns.commonExterns[TranslatableMonad].view.mapValues(f => (_: List[Core.Type]) => f).toMap

  val externs = streamExterns ++ valueExterns

  override def translateSpec(): Core.Specification = {
    val translatedExpressions = new TranslatedExpressions

    val inputs: Env = spec.in.map { i =>
      val ref = Lazy(Core.ExpressionRef(i._1, translateType(i._2._1)))
      i._1 -> TranslationResult[Any, Some](Lazy(None), Lazy(Some(Right(StrictOrLazy(ref, ref)))))
    }

    lazy val env: Env = inputs ++ spec.definitions.map { entry =>
      val result = translateExpressionArg(entry._2, env, Map(), extractNameOpt(entry._1)).translate(translatedExpressions)
      lazy val id = makeIdentifier(extractNameOpt(entry._1), entry._1.location)
      (entry._1, pushStack(TranslationResult[Any, Some](result.value, StackLazy { stack =>
        Some(result.expression.get(stack).value match {
          case Left(expression) =>
            Right(StrictOrLazy(StackLazy { stack =>
              translatedExpressions.expressions += id -> expression.get(stack)
              Core.ExpressionRef(id, translateType(entry._2.tpe))
            }, StackLazy { stack =>
              translatedExpressions.deferredQueue += (() => translatedExpressions.expressions += id -> expression.get(stack))
              Core.ExpressionRef(id, translateType(entry._2.tpe))
            }))
          case Right(expressionRef) => Right(expressionRef)
        })
      }), entry._1.location))
    }

    val outputs = spec.out.map { x =>
      (env(x._1).expression.get(List(x._1.location)).get match {
        case Left(expression) =>
          val id = makeIdentifier(extractNameOpt(x._1), x._1.location)
          translatedExpressions.expressions += id -> expression.get(List(x._1.location))
          id
        case Right(ref) => ref.translateStrict.get(List(x._1.location)) match {
          case Core.ExpressionRef(id, _, _) => id
          case expression: Core.Expression =>
            val id = makeIdentifier(extractNameOpt(x._1), x._1.location)
            translatedExpressions.expressions += id -> expression
            id
        }
      }, x._2, Nil)
    }

    translatedExpressions.complete()

    Core.Specification(spec.in.view.mapValues(x => (translateType(x._1), Nil)).toMap, translatedExpressions.expressions.toMap, outputs)
  }

  def extractNameOpt(id: Identifier) = {
    val namePos = id.id.indexOf("$")
    if (namePos > 0) Some(id.id.substring(0, namePos)) else None
  }

  def translateExpressionArg(expr: Typed.ExpressionArg, env: => Env, typeEnv: TypeEnv, nameOpt: Option[String]): Translatable[Any, Some] = Translatable { translatedExpressions =>
    expr match {
      case Typed.ExpressionRef(id, tpe, location) =>
        lazy val result = env(id)
        lazy val newId = makeIdentifier(nameOpt, id.location)
        pushStack(TranslationResult[Any, Some](StackLazy { stack =>
          result.value.get(stack)
        }, StackLazy { stack =>
          Some((result.expression.get(stack).value match {
            case Left(expression) =>
              Right(StrictOrLazy(StackLazy { stack =>
                translatedExpressions.expressions += newId -> expression.get(stack)
                Core.ExpressionRef(newId, translateType(tpe))
              }, StackLazy { stack =>
                translatedExpressions.deferredQueue += (() => translatedExpressions.expressions += newId -> expression.get(stack))
                Core.ExpressionRef(newId, translateType(tpe))
              }))
            case Right(id) => Right(id)
          }): ExpressionOrRef)
        }), location)
      case Typed.IntLiteralExpression(value, location) =>
        mkLiteralResult(Some(value), Core.IntLiteralExpression(value, location))
      case Typed.FloatLiteralExpression(value, location) =>
        mkLiteralResult(Some(value), Core.FloatLiteralExpression(value, location))
      case Typed.StringLiteralExpression(value, location) =>
        mkLiteralResult(Some(value), Core.StringLiteralExpression(value, location))
      case Typed.ExternExpression(typeParams, params, resultType, name, location) =>
        val paramTypes = params.map(x => (x._2, x._3))
        val expression = Core.ExternExpression(typeParams, params.map(x => (x._1, translateEvaluation(x._2), translateType(x._3).resolve(typeEnv))), translateType(resultType).resolve(typeEnv), name, location)
        val value: StackLazy[Option[Any]] = Lazy {
          val f: TypeApplicable = typeArgs => {
            args =>
              Translatable { translatedExpressions =>
                val extern = externs(name)
                val result = extern(typeArgs)(args).translate(translatedExpressions)
                TranslationResult[Any, Some](result.value, result.expression.map(e => Some(e.getOrElse(Left(StackLazy { stack =>
                  mkApplicationExpression(Core.TypeApplicationExpression(expression, typeArgs), paramTypes,
                    args.zip(params).map(a => reified(a._1.translate(translatedExpressions), translateType(a._2._3))),
                    stack, translatedExpressions, location)
                })))))
              }
          }
          Some(if (typeParams.isEmpty) f(Nil) else f)
        }
        TranslationResult[Any, Some](value, Lazy(Some(Right(StrictOrLazy(Lazy(expression), Lazy(expression))))))
      case Typed.FunctionExpression(typeParams, params, body, result, location) =>
        val ref = StackLazy { stack =>
          val id = makeIdentifier(nameOpt)
          translatedExpressions.deferredQueue += (() => {
            val innerTranslatedExpressions = new TranslatedExpressions
            val innerTypeEnv = typeEnv -- typeParams

            val translatedParams = params.map { param =>
              val paramId = makeIdentifier(extractNameOpt(param._1), param._1.location)
              val tpe = translateType(param._3).resolve(innerTypeEnv)
              val ref = Lazy(Core.ExpressionRef(paramId, tpe))
              param._1 -> (paramId, translateEvaluation(param._2), tpe,
                TranslationResult[Any, Some](Lazy(None), Lazy(Some(Right(StrictOrLazy(ref, ref))))))
            }

            lazy val innerEnv: Env = env ++ translatedParams.map(x => (x._1, x._2._4)) ++ translatedBody
            lazy val translatedBody = body.map { entry =>
              entry._1 -> translateExpressionArg(entry._2, innerEnv, innerTypeEnv, extractNameOpt(entry._1)).translate(innerTranslatedExpressions)
            }

            val translatedResult = getExpressionArgStrict(pushStack(translateExpressionArg(result, innerEnv, innerTypeEnv, None).translate(innerTranslatedExpressions), location)).get(stack)

            innerTranslatedExpressions.complete()

            val f = Core.FunctionExpression(typeParams, translatedParams.map(x => (x._2._1, x._2._2, x._2._3)),
              innerTranslatedExpressions.expressions.toMap, translatedResult, location)
            translatedExpressions.expressions += id -> f
          })

          Core.ExpressionRef(id, translateType(expr.tpe).resolve(typeEnv))
        }
        val value: StackLazy[Option[Any]] = Lazy {
          val f: TypeApplicable = typeArgs => {
            args =>
              Translatable { translatedExpressions =>
                val innerTypeEnv = typeEnv ++ typeParams.zip(typeArgs).map(x => (x._1, x._2))
                lazy val innerEnv: Env = env ++ params.map(_._1).zip(args.map(a => reified(a.translate(translatedExpressions), translateType(result.tpe)))) ++
                  body.map(e => (e._1,
                    translateExpressionArg(e._2, innerEnv, innerTypeEnv, extractNameOpt(e._1)).translate(translatedExpressions)
                  ))
                translateExpressionArg(result, innerEnv, innerTypeEnv, nameOpt).translate(translatedExpressions)
              }
          }
          Some(if (typeParams.isEmpty) f(Nil) else f)
        }
        pushStack(TranslationResult[Any, Some](value, Lazy(Some(Right(StrictOrLazy(ref, ref))))), location)
      case Typed.RecordConstructorExpression(entries, location) =>
        val translatedEntries = entries.view.mapValues(x => translateExpressionArg(x, env, typeEnv, nameOpt).translate(translatedExpressions)).toMap
        val value = Lazy(Some(RuntimeEvaluator.Record(translatedEntries.map(x => (x._1.id, x._2)))))
        val expression = StackLazy { stack =>
          Some(Left(StackLazy { stack =>
            Core.RecordConstructorExpression(translatedEntries.view.mapValues(x => getExpressionArgStrict(x).get(stack)).toMap, location)
          }))
        }
        pushStack(TranslationResult[Any, Some](value, expression), location)
      case Typed.RecordAccesorExpression(name, entries, location) =>
        lazy val translatedEntries = translateExpressionArg(entries, env, typeEnv, nameOpt).translate(translatedExpressions)
        val Typed.RecordType(entryTypes, _) = entries.tpe
        val value = StackLazy { stack =>
          translatedEntries.value.get(stack).map {
            case record: RuntimeEvaluator.Record => record.entries(name.id).asInstanceOf[TranslationResult[Any, Some]]
            case error: RuntimeEvaluator.RuntimeError => mkErrorResult(error, translateType(entryTypes(name)).resolve(typeEnv))
          }
        }
        val expression = StackLazy { stack =>
          value.get(stack).map(_.expression.get(stack)).getOrElse(
            Some(Left(StackLazy { stack =>
              Core.RecordAccesorExpression(name, getExpressionArgStrict(translatedEntries).get(stack), location)
            })))
        }
        pushStack(TranslationResult[Any, Some](value.flatMap(_.traverse(_.value).map(_.flatten)), expression), location)
      case Typed.ApplicationExpression(applicable, args, location) =>
        lazy val translatedApplicable = translateExpressionArg(applicable, env, typeEnv, nameOpt).translate(translatedExpressions)
        val Typed.FunctionType(_, paramTypes, resultType, _) = applicable.tpe
        if (paramTypes.size != args.size) {
          throw InternalError(s"Wrong number of arguments.", location)
        }
        lazy val translatedArgs = args.map(x => translateExpressionArg(x, env, typeEnv, nameOpt).translate(translatedExpressions))

        val value = StackLazy { stack =>
          if (translatedArgs.zip(paramTypes).forall { x =>
            x._2._1 != TesslaAST.StrictEvaluation || x._1.value.get(stack).nonEmpty
          }) {
            // TODO: handle strict arguments differently here?
            translatedApplicable.value.get(stack).map {
              case error: RuntimeEvaluator.RuntimeError => mkErrorResult(error, translateType(resultType).resolve(typeEnv))
              case f: Applicable => f(translatedArgs.map(r => Translatable[Any, Some](_ => r))).translate(translatedExpressions)
            }
          } else {
            None
          }
        }
        val expression = StackLazy { stack =>
          value.get(Nil).map(_.expression.get(stack)).getOrElse(Some(Left(StackLazy { stack =>
            mkApplicationExpression(getExpressionArgStrict(translatedApplicable).get(stack), paramTypes, translatedArgs,
              stack, translatedExpressions, location)
          })))
        }
        pushStack(TranslationResult[Any, Some](value.flatMap(_.traverse(_.value).map(_.flatten)), expression), location)
      case Typed.TypeApplicationExpression(applicable, typeArgs, location) =>
        val Typed.FunctionType(typeParams, _, _, _) = applicable.tpe
        if (typeParams.size != typeArgs.size) {
          throw InternalError(s"Wrong number of type arguments.", location)
        }
        lazy val translatedApplicable = pushStack(translateExpressionArg(applicable, env, typeEnv, nameOpt).translate(translatedExpressions), location)
        val value: StackLazy[Option[Any]] = StackLazy { stack =>
          translatedApplicable.value.get(location :: stack).map {
            case error: RuntimeEvaluator.RuntimeError => error
            case f => if (typeParams.isEmpty) f else f.asInstanceOf[TypeApplicable](typeArgs.map(translateType(_).resolve(typeEnv)))
          }
        }
        val expression = Lazy(Some(Left(StackLazy { stack =>
          Core.TypeApplicationExpression(getExpressionArgStrict(translatedApplicable).get(stack),
            typeArgs.map(translateType(_).resolve(typeEnv)), location)
        })))
        pushStack(TranslationResult[Any, Some](value, expression), location)
    }
  }

  def pushStack(result: TranslationResult[Any, Some], location: Location) =
    TranslationResult[Any, Some](result.value.push(location :: _), result.expression.push(location :: _).map(e => Some(e.value match {
      case Left(exp) => Left(exp.push(location :: _))
      case Right(StrictOrLazy(exp1, exp2)) => Right(StrictOrLazy(exp1.push(location :: _), exp2.push(location :: _)))
    })))

  def mkApplicationExpression(applicable: Core.ExpressionArg, paramTypes: List[(Typed.Evaluation, Typed.Type)], args: ArraySeq[TranslationResult[Any, Some]], stack: Stack, translatedExpressions: TranslatedExpressions, location: Location): Core.ApplicationExpression =
    Core.ApplicationExpression(
      applicable,
      args.zip(paramTypes).map { x =>
        if (x._2._1 != TesslaAST.LazyEvaluation) {
          getExpressionArgStrict(x._1).get(stack)
        } else x._1.expression.get(stack).value match {
          case Left(expression) =>
            val id = makeIdentifier(None)
            translatedExpressions.deferredQueue += (() => {
              translatedExpressions.expressions += id -> expression.get(stack)
            })
            Core.ExpressionRef(id, translateType(x._2._2))
          case Right(expressionRef) => expressionRef.translateLazy.get(stack)
        }
      }, location)

  def translateEvaluation(evaluation: Typed.Evaluation): Core.Evaluation = evaluation match {
    case TesslaAST.LazyEvaluation => TesslaAST.LazyEvaluation
    case _ => TesslaAST.StrictEvaluation
  }

  def translateType(tpe: Typed.Type): Core.Type = tpe match {
    case Typed.FunctionType(typeParams, paramTypes, resultType, location) =>
      Core.FunctionType(typeParams, paramTypes.map(x => (translateEvaluation(x._1), translateType(x._2))), translateType(resultType), location)
    case Typed.InstatiatedType(name, typeArgs, location) =>
      Core.InstatiatedType(name, typeArgs.map(translateType), location)
    case Typed.RecordType(entries, location) =>
      Core.RecordType(entries.view.mapValues(translateType).toMap, location)
    case Typed.TypeParam(name, location) =>
      Core.TypeParam(name, location)
  }

  private var counter = 0

  def makeIdentifier(nameOpt: Option[String], location: Location = Location.unknown): Identifier = {
    counter += 1
    new Identifier(nameOpt.getOrElse("") + "$" + counter, location)
  }

}

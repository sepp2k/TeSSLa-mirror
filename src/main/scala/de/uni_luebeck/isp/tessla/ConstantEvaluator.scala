package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.LazyWithStack
import TesslaAST.{Core, Typed}
import cats._
import cats.implicits._

import scala.collection.immutable.ArraySeq

object ConstantEvaluator {

  val PREFER_REIFIED_EXPRESSIONS = false

  type Env = Map[Typed.Identifier, TranslationResult[Any, Some]]
  type TypeEnv = Map[Typed.Identifier, Typed.Type]

  class TranslatedExpressions {
    val expressions = mutable.Map[Core.Identifier, Core.Expression]()
    val deferredQueue = mutable.Queue[() => Unit]()

    def complete(): Unit = {
      while (deferredQueue.nonEmpty) {
        deferredQueue.dequeue()()
      }
    }
  }

  val lazyWithStack = new LazyWithStack[Location]() {
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

  case class TranslationResult[+A, +B[+_]](value: StackLazy[Option[A]], expression: StackLazy[B[ExpressionOrRef]])

  case class Translatable[+A, +B[+_]](translate: TranslatedExpressions => TranslationResult[A, B])

  type TranslatableMonad[+A] = Translatable[A, Option]

  def getExpressionArgStrict(result: TranslationResult[Any, Some]) =
    result.expression.flatMap(_.value match {
      case Left(expression) => expression
      case Right(expresssionRef) => expresssionRef.translateStrict
    })

  def mkLiteralResult(value: Option[Any], expression: Core.Expression) =
    TranslationResult[Any, Some](Lazy(value), Lazy(Some(Right(StrictOrLazy(Lazy(expression), Lazy(expression))))))

  def reified(result: TranslationResult[Any, Option], tpe: Core.Type): TranslationResult[Any, Some] =
    TranslationResult[Any, Some](result.value, getReified(result, tpe).map {
      case Right(e) => Some(e)
      case Left(error) => throw error
    })

  def getReified(result: TranslationResult[Any, Option], tpe: Core.Type): StackLazy[Either[InternalError, ExpressionOrRef]] = {
    lazy val reified = result.value.flatMap { value =>
      value.map(Right(_)).getOrElse(Left(InternalError("No value available for reification."))).traverse { v =>
        for {
          reified <- CompiletimeExterns.reify(v, tpe)
        } yield for {
          r <- reified
        } yield if (
          r.isInstanceOf[Core.IntLiteralExpression] || r.isInstanceOf[Core.FloatLiteralExpression] ||
            r.isInstanceOf[Core.StringLiteralExpression] || r.isInstanceOf[Core.ExternExpression]
        ) {
          Right(StrictOrLazy(Lazy(r), Lazy(r)))
        } else {
          Left(Lazy(r))
        }
      }
    }.map(_.flatten)
    for {
      r <- reified
      e <- result.expression
      either = e.map(Right(_)).getOrElse(Left(InternalError("No value available for reification.")))
    } yield if (PREFER_REIFIED_EXPRESSIONS) {
      r.orElse(either)
    } else {
      either.orElse(r)
    }
  }


  def tryReified(result: TranslationResult[Any, Option], tpe: Core.Type): TranslationResult[Any, Option] =
    TranslationResult[Any, Option](result.value, getReified(result, tpe).map(_.toOption))

  type TypeApplicable = List[Typed.Type] => Applicable
  type Applicable = ArraySeq[Translatable[Any, Option]] => Translatable[Any, Some]

  type TypeExtern = List[Typed.Type] => Extern
  type Extern = ArraySeq[TranslatableMonad[Any]] => TranslatableMonad[Any]


  val errorExtern = Core.ExternExpression(
    List(Core.Identifier("A")),
    List((TesslaAST.LazyEvaluation, Core.StringType)),
    Core.TypeParam(Core.Identifier("A")),
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

  val doNotExpandExterns: Map[String, TypeExtern] = List("CTF_getInt", "CTF_getString").map(_ -> noExtern).toMap

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

  val valueExterns: Map[String, TypeExtern] = RuntimeExterns.commonExterns[TranslatableMonad].view.mapValues(f => (_: List[Typed.Type]) => f).toMap

  val externs = streamExterns ++ valueExterns

}

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TranslationPhase[Typed.Specification, Core.Specification] {
  override def translate(spec: Typed.Specification) = {
    new ConstantEvaluatorWorker(spec, baseTimeUnit).translate()
  }
}

class ConstantEvaluatorWorker(spec: Typed.Specification, baseTimeUnit: Option[TimeUnit])
  extends TranslationPhase.Translator[Core.Specification] {

  import ConstantEvaluator._

  import lazyWithStack._

  override def translateSpec(): Core.Specification = try {
    val translatedExpressions = new TranslatedExpressions

    val inputs: Env = spec.in.map { i =>
      val ref = Lazy(Core.ExpressionRef(translateIdentifier(i._1), translateType(i._2._1)))
      i._1 -> TranslationResult[Any, Some](Lazy(None), Lazy(Some(Right(StrictOrLazy(ref, ref)))))
    }

    lazy val env: Env = inputs ++ spec.definitions.map { entry =>
      val result = translateExpressionArg(entry._2, env, Map(), extractNameOpt(entry._1)).translate(translatedExpressions)
      lazy val id = Core.Identifier(entry._1.id, entry._1.location)
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
          val id = Core.Identifier(x._1.id, x._1.location)
          translatedExpressions.expressions += id -> expression.get(List(x._1.location))
          id
        case Right(ref) => ref.translateStrict.get(List(x._1.location)) match {
          case Core.ExpressionRef(id, _, _) => id
          case expression: Core.Expression =>
            val id = Core.Identifier(x._1.id, x._1.location)
            translatedExpressions.expressions += id -> expression
            id
        }
      }, x._2, Nil)
    }

    translatedExpressions.complete()

    Core.Specification(spec.in.map(x => (translateIdentifier(x._1), (translateType(x._2._1), Nil))), translatedExpressions.expressions.toMap, outputs, counter)
  } catch {
    case e: ClassCastException => throw InternalError(e.toString + "\n" + e.getStackTrace.mkString("\n"))
  }

  def extractNameOpt(id: Typed.Identifier) = {
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
        val paramTypes = params.map(x => (x._1, x._2))
        val expression = Core.ExternExpression(typeParams.map(translateIdentifier), params.map(x => (translateEvaluation(x._1), translateType(x._2.resolve(typeEnv)))), translateType(resultType.resolve(typeEnv)), name, location)
        val value: StackLazy[Option[Any]] = Lazy {
          val f: TypeApplicable = typeArgs => {
            val innerTypeEnv = typeEnv ++ typeParams.zip(typeArgs).map(x => (x._1, x._2))
            args =>
              Translatable { translatedExpressions =>
                val extern = externs(name)
                val result = tryReified(extern(typeArgs)(args).translate(translatedExpressions), translateType(resultType.resolve(innerTypeEnv)))
                TranslationResult[Any, Some](result.value, result.expression.map(e => Some(e.getOrElse(Left(StackLazy { stack =>
                  mkApplicationExpression(Core.TypeApplicationExpression(expression, typeArgs.map(translateType)), paramTypes,
                    args.zip(params).map(a => reified(a._1.translate(translatedExpressions), translateType(a._2._2))),
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
              val paramId = Core.Identifier(param._1.id, param._1.location)
              val tpe = translateType(param._3.resolve(innerTypeEnv))
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

            val f = Core.FunctionExpression(typeParams.map(translateIdentifier), translatedParams.map(x => (x._2._1, x._2._2, x._2._3)),
              innerTranslatedExpressions.expressions.toMap, translatedResult, location)
            translatedExpressions.expressions += id -> f
          })

          Core.ExpressionRef(id, translateType(expr.tpe.resolve(typeEnv)))
        }
        val value: StackLazy[Option[Any]] = Lazy {
          val f: TypeApplicable = typeArgs => {
            args =>
              Translatable { translatedExpressions =>
                val innerTypeEnv = typeEnv ++ typeParams.zip(typeArgs).map(x => (x._1, x._2))
                lazy val innerEnv: Env = env ++ params.zip(args).map(a => (a._1._1 -> reified(a._2.translate(translatedExpressions),
                  translateType(a._1._3.resolve(innerTypeEnv))))) ++
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
        val value = Lazy(Some(RuntimeEvaluator.Record(translatedEntries.map(x => (x._1.name, x._2)))))
        val expression = StackLazy { stack =>
          Some(Left(StackLazy { stack =>
            Core.RecordConstructorExpression(translatedEntries.map(x => (x._1, getExpressionArgStrict(x._2).get(stack))).toMap, location)
          }))
        }
        pushStack(TranslationResult[Any, Some](value, expression), location)
      case Typed.RecordAccesorExpression(name, entries, location) =>
        lazy val translatedEntries = translateExpressionArg(entries, env, typeEnv, nameOpt).translate(translatedExpressions)
        val Typed.RecordType(entryTypes, _) = entries.tpe
        val value = StackLazy { stack =>
          translatedEntries.value.get(stack).map {
            case record: RuntimeEvaluator.Record => record.entries(name.name).asInstanceOf[TranslationResult[Any, Some]]
            case error: RuntimeEvaluator.RuntimeError => mkErrorResult(error, translateType(entryTypes(name).resolve(typeEnv)))
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
              case error: RuntimeEvaluator.RuntimeError => mkErrorResult(error, translateType(resultType.resolve(typeEnv)))
              case f => f.asInstanceOf[Applicable](translatedArgs.map(r => Translatable[Any, Some](_ => r))).translate(translatedExpressions)
            }
          } else {
            None
          }
        }
        val expression = StackLazy { stack =>
          value.get(stack).map(_.expression.get(stack)).getOrElse(Some(Left(StackLazy { stack =>
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
            case f => if (typeParams.isEmpty) f else f.asInstanceOf[TypeApplicable](typeArgs.map(_.resolve(typeEnv)))
          }
        }
        val expression = Lazy(Some(Left(StackLazy { stack =>
          Core.TypeApplicationExpression(getExpressionArgStrict(translatedApplicable).get(stack),
            typeArgs.map(x => translateType(x.resolve(typeEnv))), location)
        })))
        pushStack(TranslationResult[Any, Some](value, expression), location)
    }
  }

  def pushStack(result: TranslationResult[Any, Some], location: Location) =
    TranslationResult[Any, Some](result.value.push(location), result.expression.push(location).map(e => Some(e.value match {
      case Left(exp) => Left(exp.push(location))
      case Right(StrictOrLazy(exp1, exp2)) => Right(StrictOrLazy(exp1.push(location), exp2.push(location)))
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
      Core.FunctionType(typeParams.map(translateIdentifier), paramTypes.map(x => (translateEvaluation(x._1), translateType(x._2))), translateType(resultType), location)
    case Typed.InstatiatedType(name, typeArgs, location) =>
      Core.InstatiatedType(name, typeArgs.map(translateType), location)
    case Typed.RecordType(entries, location) =>
      Core.RecordType(entries.map(x => (x._1, translateType(x._2))).toMap, location)
    case Typed.TypeParam(name, location) =>
      Core.TypeParam(translateIdentifier(name), location)
  }

  private var counter = spec.maxIdentifier

  def makeIdentifier(nameOpt: Option[String], location: Location = Location.unknown): Core.Identifier = {
    counter += 1
    new Core.Identifier(nameOpt.getOrElse("") + "$" + counter, location)
  }

  def translateIdentifier(identifier: Typed.Identifier) =
    Core.Identifier(identifier.id, identifier.location)
}

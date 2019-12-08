package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.{Lazy, LazyWithStack, ArraySeqMonad}
import ArraySeqMonad.instance
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

  println(spec)

  type Env = Map[Identifier, TranslationResult]
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

  import lazyWithStack.StackLazy
  import lazyWithStack.StackLazy.monadInstance

  type ExpressionOrRef = Either[StackLazy[Core.Expression], (StackLazy[Core.ExpressionArg], StackLazy[Core.ExpressionArg])]

  case class TranslationResult(value: StackLazy[Option[Any]], expression: StackLazy[ExpressionOrRef])

  def getExpressionArgStrict(result: TranslationResult, stack: Stack): Core.ExpressionArg =
    result.expression.get(stack) match {
      case Left(expression) => expression.get(stack)
      case Right(expresssionRef) => expresssionRef._1.get(stack)
    }

  def mkLiteralResult(value: Option[Any], expression: Core.Expression) =
    TranslationResult(StackLazy(_ => value), StackLazy(_ => Right((StackLazy(_ => expression), StackLazy(_ => expression)))))


  /*type Reifier = (String, Any, Core.Type) => (List[Core.ExpressionArg] => Core.Expression, List[Any])

  val reifiers = Map[String, Reifier]()

  def reify(value: Any, tpe: Core.Type): (Core.Expression, Boolean) = {
    ???
  }*/

  case class ExternResult(value: StackLazy[Option[Any]], expression: StackLazy[Option[ExpressionOrRef]])

  def mkExpressionResult(expression: StackLazy[Option[ExpressionOrRef]]) = ExternResult(StackLazy { _ => None }, expression)

  type TypeApplicable = List[Core.Type] => Applicable
  type Applicable = ArraySeq[TranslationResult] => TranslationResult

  type TypeExtern = List[Core.Type] => Extern
  type Extern = ArraySeq[TranslationResult] => ExternResult

  val errorExtern = Core.ExternExpression(
    List(Identifier("A")),
    List((Identifier("msg"), TesslaAST.LazyEvaluation, Core.StringType)),
    Core.TypeParam(Identifier("A")),
    "error"
  )

  def mkErrorResult(error: RuntimeEvaluator.RuntimeError, tpe: Core.Type): TranslationResult = {
    val expression = Core.ApplicationExpression(
      Core.TypeApplicationExpression(errorExtern, List(tpe)),
      ArraySeq(Core.StringLiteralExpression(error.msg)))
    TranslationResult(StackLazy(_ => Some(error)), StackLazy(_ => Left(StackLazy(_ => expression))))
  }

  val noExtern: TypeExtern = _ => _ => mkExpressionResult(StackLazy { _ => None })

  val streamExterns: Map[String, TypeExtern] = List("last", "slift", "default", "lift", "nil", "time", "false", "true",
    "delay", "defaultFrom", "merge", "String_format").map(_ -> noExtern).toMap

  // TODO: try to reify value
  def valueExtern(f: ArraySeq[Lazy[Any]] => Any): TypeExtern = _ => args => ExternResult(StackLazy { stack =>
    val valueArgs: Option[ArraySeq[Lazy[Any]]] = args.map(_.value.get(stack).map(Lazy(_))).sequence
    valueArgs.map(f)
  }, StackLazy(_ => None))

  val valueExterns: Map[String, TypeExtern] = RuntimeEvaluator.commonExterns {f =>
    args => f(args.map {x =>
      TranslationResult(StackLazy(_ => Some(x.get)), StackLazy(_ => Left(StackLazy(_ => Core.ApplicationExpression(
        Core.TypeApplicationExpression(errorExtern, List()),
        ArraySeq(Core.StringLiteralExpression("asd")))))))
    }).asInstanceOf[TranslationResult].value.get(Nil).get
  }.view.mapValues(valueExtern).toMap

  val staticIteExtern: TypeExtern = _ => args => {
    val value = StackLazy { stack =>
      args.head.value.get(stack).map {
        case _: RuntimeEvaluator.RuntimeError => args.head
        case value => if (value.asInstanceOf[Boolean]) args(1) else args(2)
      }
    }
    val expression = value.flatMap(_.map(_.expression).sequence)
    ExternResult(value.flatMap(_.map(_.value).sequence.map(_.flatten)), expression)
  }

  val extraExterns = Map(
    "staticite" -> staticIteExtern,
    "ite" -> staticIteExtern
  )
  val externs = streamExterns ++ valueExterns ++ extraExterns

  val translatedExpressions = new TranslatedExpressions

  override def translateSpec(): Core.Specification = {

    val inputs: Env = spec.in.map { i =>
      val ref = StackLazy(_ => Core.ExpressionRef(i._1, translateType(i._2._1)))
      i._1 -> TranslationResult(StackLazy(_ => None), StackLazy(_ => Right((ref, ref))))
    }

    lazy val env: Env = inputs ++ spec.definitions.map { entry =>
      val result = translateExpressionArg(entry._2, env, Map(), extractNameOpt(entry._1))
      lazy val id = makeIdentifier(extractNameOpt(entry._1))
      (entry._1, TranslationResult(result.value, StackLazy { stack =>
        result.expression.get(stack) match {
          case Left(expression) =>
            Right((StackLazy { stack =>
              translatedExpressions.expressions += id -> expression.get(stack)
              Core.ExpressionRef(id, translateType(entry._2.tpe))
            }, StackLazy { stack =>
              translatedExpressions.deferredQueue += (() => translatedExpressions.expressions += id -> expression.get(stack))
              Core.ExpressionRef(id, translateType(entry._2.tpe))
            }))
          case Right(expressionRef) => Right(expressionRef)
        }
      }))
    }

    val outputs = spec.out.map { x =>
      (env(x._1).expression.get(List(x._1.location)) match {
        case Left(expression) =>
          val id = makeIdentifier(extractNameOpt(x._1))
          translatedExpressions.expressions += id -> expression.get(List(x._1.location))
          id
        case Right(ref) => ref._1.get(List(x._1.location)) match {
          case Core.ExpressionRef(id, _, _) => id
          case expression: Core.Expression =>
            val id = makeIdentifier(extractNameOpt(x._1))
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

  def translateExpressionArg(expr: Typed.ExpressionArg, env: => Env, typeEnv: TypeEnv, nameOpt: Option[String]): TranslationResult = expr match {
    case Typed.ExpressionRef(id, tpe, _) =>
      lazy val result = env(id)
      lazy val newId = makeIdentifier(nameOpt)
      TranslationResult(StackLazy { stack =>
        result.value.get(stack)
      }, StackLazy { stack =>
        result.expression.get(stack) match {
          case Left(expression) =>
            Right((StackLazy { stack =>
              translatedExpressions.expressions += newId -> expression.get(stack)
              Core.ExpressionRef(newId, translateType(tpe))
            }, StackLazy { stack =>
              translatedExpressions.deferredQueue += (() => translatedExpressions.expressions += newId -> expression.get(stack))
              Core.ExpressionRef(newId, translateType(tpe))
            }))
          case Right(id) => Right(id)
        }
      })
    case Typed.IntLiteralExpression(value, location) =>
      mkLiteralResult(Some(value), Core.IntLiteralExpression(value, location))
    case Typed.FloatLiteralExpression(value, location) =>
      mkLiteralResult(Some(value), Core.FloatLiteralExpression(value, location))
    case Typed.StringLiteralExpression(value, location) =>
      mkLiteralResult(Some(value), Core.StringLiteralExpression(value, location))
    case Typed.ExternExpression(typeParams, params, resultType, name, location) =>
      val paramTypes = params.map(x => (x._2, x._3))
      val expression = Core.ExternExpression(typeParams, params.map(x => (x._1, translateEvaluation(x._2), translateType(x._3).resolve(typeEnv))), translateType(resultType).resolve(typeEnv), name, location)
      val value: StackLazy[Option[Any]] = StackLazy { _ =>
        val f: TypeApplicable = typeArgs => {
          args => {
            val extern = externs(name)
            val result = extern(typeArgs)(args)
            TranslationResult(result.value, result.expression.map(_.getOrElse(Left(StackLazy { stack =>
              mkApplicationExpression(Core.TypeApplicationExpression(expression, typeArgs),
                paramTypes, args, location :: stack, translatedExpressions)
            }))))
          }
        }
        Some(if (typeParams.isEmpty) f(Nil) else f)
      }
      TranslationResult(value, StackLazy(_ => Right(StackLazy(_ => expression), StackLazy(_ => expression))))
    case Typed.FunctionExpression(typeParams, params, body, result, location) =>
      val ref = StackLazy { stack =>
        val id = makeIdentifier(nameOpt)
        translatedExpressions.deferredQueue += (() => {
          val innerTypeEnv = typeEnv -- typeParams

          val translatedParams = params.map { param =>
            val paramId = makeIdentifier(extractNameOpt(param._1))
            val tpe = translateType(param._3).resolve(innerTypeEnv)
            val ref = StackLazy(_ => Core.ExpressionRef(paramId, tpe))
            param._1 -> (paramId, translateEvaluation(param._2), tpe,
              TranslationResult(StackLazy(_ => None), StackLazy(_ => Right((ref, ref)))))
          }

          lazy val innerEnv: Env = env ++ translatedParams.map(x => (x._1, x._2._4)) ++ translatedBody
          lazy val translatedBody = body.map { entry =>
            entry._1 -> translateExpressionArg(entry._2, innerEnv, innerTypeEnv, extractNameOpt(entry._1))
          }

          val translatedResult = getExpressionArgStrict(translateExpressionArg(result, innerEnv, innerTypeEnv, None), location :: stack)

          translatedExpressions.complete()

          val f = Core.FunctionExpression(typeParams, translatedParams.map(x => (x._2._1, x._2._2, x._2._3)),
            translatedExpressions.expressions.toMap, translatedResult, location)
          translatedExpressions.expressions += id -> f
        })

        Core.ExpressionRef(id, translateType(expr.tpe).resolve(typeEnv), location)
      }
      val value: StackLazy[Option[Any]] = StackLazy { _ =>
        val f: TypeApplicable = typeArgs => {
          args => {
            val innerTypeEnv = typeEnv ++ typeParams.zip(typeArgs).map(x => (x._1, x._2))
            lazy val innerEnv: Env = env ++ params.map(_._1).zip(args) ++
              body.map(e => (e._1, translateExpressionArg(e._2, innerEnv, innerTypeEnv, extractNameOpt(e._1))))
            translateExpressionArg(result, innerEnv, innerTypeEnv, nameOpt)
          }
        }
        Some(if (typeParams.isEmpty) f(Nil) else f)
      }
      TranslationResult(value, StackLazy(_ => Right((ref, ref))))
    case Typed.RecordConstructorExpression(entries, location) =>
      val translatedEntries = entries.view.mapValues(translateExpressionArg(_, env, typeEnv, nameOpt)).toMap
      val value = RuntimeEvaluator.Record(translatedEntries.map(x => (x._1.id, x._2)))
      val expression = StackLazy(stack => Core.RecordConstructorExpression(translatedEntries.map(x => (x._1, getExpressionArgStrict(x._2, location :: stack)))))
      TranslationResult(StackLazy(_ => Some(value)), StackLazy(_ => Left(expression)))
    case Typed.RecordAccesorExpression(name, entries, location) =>
      lazy val translatedEntries = translateExpressionArg(entries, env, typeEnv, nameOpt)
      val Typed.RecordType(entryTypes, _) = entries.tpe
      val value = StackLazy { stack =>
        translatedEntries.value.get(location :: stack).map {
          case record: RuntimeEvaluator.Record => record.entries(name.id).asInstanceOf[TranslationResult]
          case error: RuntimeEvaluator.RuntimeError => mkErrorResult(error, translateType(entryTypes(name)).resolve(typeEnv))
        }
      }
      val expression = StackLazy { stack =>
        value.get(stack).map(_.expression.get(location :: stack)).getOrElse(
          Left(StackLazy(stack => Core.RecordAccesorExpression(name, getExpressionArgStrict(translatedEntries, location :: stack), location))))
      }
      TranslationResult(value.flatMap(_.map(_.value).sequence.map(_.flatten)), expression)
    case Typed.ApplicationExpression(applicable, args, location) =>
      lazy val translatedApplicable = translateExpressionArg(applicable, env, typeEnv, nameOpt)
      val Typed.FunctionType(_, paramTypes, resultType, _) = applicable.tpe
      if (paramTypes.size != args.size) {
        throw InternalError(s"Wrong number of arguments.", location)
      }
      lazy val translatedArgs = args.map(translateExpressionArg(_, env, typeEnv, nameOpt))

      val value = StackLazy { stack =>
        if (translatedArgs.zip(paramTypes).forall { x =>
          x._2._1 != TesslaAST.StrictEvaluation || x._1.value.get(stack).nonEmpty
        }) {
          // TODO: handle strict arguments differently here?
          translatedApplicable.value.get(location :: stack).map {
            case error: RuntimeEvaluator.RuntimeError => mkErrorResult(error, translateType(resultType).resolve(typeEnv))
            case f: Applicable => f(translatedArgs)
          }
        } else {
          None
        }
      }
      val expression = StackLazy { stack =>
        value.get(Nil).
          map(_.expression.get(location :: stack)).
          getOrElse(Left(StackLazy(stack => mkApplicationExpression(
            getExpressionArgStrict(translatedApplicable, location :: stack), paramTypes, translatedArgs, location :: stack, translatedExpressions))))
      }
      TranslationResult(value.flatMap(_.map(_.value).sequence.map(_.flatten)), expression)
    case Typed.TypeApplicationExpression(applicable, typeArgs, location) =>
      val Typed.FunctionType(typeParams, _, _, _) = applicable.tpe // TODO: check that number of typeArgs conforms to number of typeParams once types are correct
      //if (typeParams.size != typeArgs.size) {
      //  throw InternalError(s"Wrong number of type arguments.", location)
      //}
      lazy val translatedApplicable = translateExpressionArg(applicable, env, typeEnv, nameOpt)
      val value: StackLazy[Option[Any]] = StackLazy { stack =>
        translatedApplicable.value.get(location :: stack).map {
          case error: RuntimeEvaluator.RuntimeError => error
          case f => if (typeParams.isEmpty) f else f.asInstanceOf[TypeApplicable](typeArgs.map(translateType(_).resolve(typeEnv)))
        }
      }
      val expression = StackLazy { stack =>
        Core.TypeApplicationExpression(getExpressionArgStrict(translatedApplicable, location :: stack), typeArgs.map(translateType(_).resolve(typeEnv)))
      }
      TranslationResult(value, StackLazy(_ => Left(expression)))
  }

  def mkApplicationExpression(applicable: Core.ExpressionArg, paramTypes: List[(Typed.Evaluation, Typed.Type)], args: ArraySeq[TranslationResult], stack: Stack, translatedExpressions: TranslatedExpressions): Core.ApplicationExpression =
    Core.ApplicationExpression(
      applicable,
      args.zip(paramTypes).map { x =>
        if (x._2._1 != TesslaAST.LazyEvaluation) {
          getExpressionArgStrict(x._1, stack)
        } else x._1.expression.get(stack) match {
          case Left(expression) =>
            val id = makeIdentifier(None)
            translatedExpressions.deferredQueue += (() => {
              translatedExpressions.expressions += id -> expression.get(stack)
            })
            Core.ExpressionRef(id, translateType(x._2._2))
          case Right(expressionRef) => expressionRef._2.get(stack)
        }
      })

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
    case null => Core.TypeParam(Identifier("NULL")) // FIXME: remove this once the typed AST has all types
  }

  private var counter = 0

  def makeIdentifier(nameOpt: Option[String], location: Location = Location.unknown): Identifier = {
    counter += 1
    new Identifier(nameOpt.getOrElse("") + "$" + counter, location)
  }

}

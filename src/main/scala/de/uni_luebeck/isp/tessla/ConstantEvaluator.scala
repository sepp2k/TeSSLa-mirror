package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.LazyWithStack

import TesslaAST.{Core, Identifier, Typed}

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit], evaluator: Evaluator) extends TranslationPhase[Typed.Specification, Core.Specification] {
  override def translate(spec: Typed.Specification) = {
    new ConstantEvaluatorWorker(spec, baseTimeUnit).translate()
  }
}

class ConstantEvaluatorWorker(spec: Typed.Specification, baseTimeUnit: Option[TimeUnit])
  extends TranslationPhase.Translator[Core.Specification] {
  type Env = Map[Identifier, TranslationResult]
  type TypeEnv = Map[Identifier, Core.Type]

  class TranslatedExpressions {
    val expressions = mutable.Map[Identifier, Core.ExpressionArg]()
    val deferredQueue = mutable.Queue[() => Unit]()

    def complete(): Unit = {
      while (deferredQueue.nonEmpty) {
        deferredQueue.dequeue()()
      }
    }
  }

  type Stack = List[Location]

  type Lazy[+A] = LazyWithStack[_ <: A, Stack]

  def Lazy[A](f: Stack => A): Lazy[A] = LazyWithStack[A, Stack] { (stack, rec) =>
    if (stack.size > 1000) {
      throw WithStackTrace(Errors.StackOverflow(stack.head), stack.tail)
    } else if (rec) {
      throw WithStackTrace(InfiniteRecursion(stack.head), stack.tail)
    } else {
      try {
        f(stack)
      } catch {
        case err: InternalError =>
          throw WithStackTrace(err, stack)
        case _: StackOverflowError =>
          throw WithStackTrace(Errors.StackOverflow(stack.head), stack.tail)
      }
    }
  }

  case class TranslationResult(value: Lazy[Option[Any]], expression: Lazy[Core.ExpressionArg], strictId: Lazy[Identifier], lazyId: Lazy[Identifier])

  def mkLiteralResult(value: Option[Any], expression: Core.ExpressionArg, nameOpt: Option[String], translatedExpressions: TranslatedExpressions) =
    mkResult(Lazy(_ => value), Lazy(_ => expression), nameOpt, translatedExpressions)

  def mkResult(value: Lazy[Option[Any]], expression: Lazy[Core.ExpressionArg], nameOpt: Option[String], translatedExpressions: TranslatedExpressions) = {
    lazy val id = makeIdentifier(nameOpt)
    TranslationResult(value, expression, Lazy { stack =>
      translatedExpressions.expressions += id -> expression.get(stack)
      id
    }, Lazy { stack =>
      translatedExpressions.deferredQueue += (() => {
        translatedExpressions.expressions += id -> expression.get(stack)
      })
      id
    })
  }

  type Reifier = (String, Any, Core.Type) => (List[Core.ExpressionArg] => Core.Expression, List[Any])

  val reifiers = Map[String, Reifier]()

  def reify(value: Any, tpe: Core.Type): (Core.Expression, Boolean) = {
    ???
  }

  case class ExternResult(value: Lazy[Option[Any]], expression: Lazy[Option[Core.ExpressionArg]])

  def mkExpressionResult(expression: Lazy[Option[Core.ExpressionArg]]) =
    ExternResult(Lazy { _ => None }, expression)

  type TypeApplicable = List[Core.Type] => Applicable
  type Applicable = List[TranslationResult] => TranslationResult

  type TypeExtern = List[Core.Type] => Extern
  type Extern = List[TranslationResult] => ExternResult

  val errorExtern = Core.ExternExpression(
    List(Identifier("A")),
    List((Identifier("msg"), TesslaAST.LazyEvaluation, Core.StringType)),
    Core.TypeParam(Identifier("A")),
    "error"
  )

  val noExtern: TypeExtern = (_ => _ => mkExpressionResult(Lazy { _ => None }))

  val externs: Map[String, TypeExtern] = List("last", "slift", "default").map(_ -> noExtern).toMap

  override def translateSpec(): Core.Specification = {
    val translatedExpressions = new TranslatedExpressions

    val inputs: Env = spec.in.map { i =>
      i._1 -> TranslationResult(Lazy(_ => None), Lazy(_ => Core.ExpressionRef(i._1, translateType(i._2._1))), Lazy(_ => i._1), Lazy(_ => i._1))
    }

    lazy val env: Env = inputs ++ spec.definitions.map { entry =>
      (entry._1, translateExpressionArg(entry._2, env, Map(), extractNameOpt(entry._1), translatedExpressions))
    }

    val outputs = spec.out.map { x =>
      (env(x._1).strictId.get(List(x._1.location)), x._2, Nil)
    }

    translatedExpressions.complete()

    Core.Specification(spec.in.mapValues(x => (translateType(x._1), Nil)), translatedExpressions.expressions.toMap, outputs)
  }

  def extractNameOpt(id: Identifier) = {
    val namePos = id.id.indexOf("$")
    if (namePos > 0) Some(id.id.substring(0, namePos)) else None
  }

  def translateExpressionArg(expr: Typed.ExpressionArg, env: => Env, typeEnv: TypeEnv, nameOpt: Option[String],
    translatedExpressions: TranslatedExpressions
  ): TranslationResult = expr match {
    case Typed.ExpressionRef(id, tpe, _) =>
      lazy val result = env(id)
      TranslationResult(Lazy { stack => result.value.get(stack) }, Lazy { stack =>
        Core.ExpressionRef(result.strictId.get(stack), translateType(tpe).resolve(typeEnv))
      }, Lazy { stack => result.strictId.get(stack) }, Lazy { stack => result.lazyId.get(stack) })
    case Typed.IntLiteralExpression(value, location) =>
      mkLiteralResult(Some(value), Core.IntLiteralExpression(value, location), nameOpt, translatedExpressions)
    case Typed.FloatLiteralExpression(value, location) =>
      mkLiteralResult(Some(value), Core.FloatLiteralExpression(value, location), nameOpt, translatedExpressions)
    case Typed.StringLiteralExpression(value, location) =>
      mkLiteralResult(Some(value), Core.StringLiteralExpression(value, location), nameOpt, translatedExpressions)
    case Typed.ExternExpression(typeParams, params, resultType, name, location) =>
      val paramTypes = params.map(x => (x._2, x._3))
      val expression = Core.ExternExpression(typeParams, params.map(x => (x._1, translateEvaluation(x._2), translateType(x._3).resolve(typeEnv))), translateType(resultType).resolve(typeEnv), name, location)
      val value: Lazy[Option[TypeApplicable]] = Lazy { stack =>
        Some(typeArgs => (args => {
          val extern = externs(name)
          try {
            val result = extern(typeArgs)(args)
            mkResult(result.value, result.expression.map { e =>
              e.getOrElse(mkApplicationExpression(
                Core.TypeApplicationExpression(expression, typeArgs), paramTypes, args, location :: stack))
            }, nameOpt, translatedExpressions)
          } catch {
            case e: RuntimeException => mkLiteralResult(Some(e), Core.ApplicationExpression(
              Core.TypeApplicationExpression(errorExtern, List(translateType(resultType))),
              List(Core.StringLiteralExpression(e.getMessage))), nameOpt, translatedExpressions) // TODO smarter error handling?
          }
        }))
      }
      mkResult(value, Lazy { stack => expression }, nameOpt, translatedExpressions)
    case Typed.FunctionExpression(typeParams, params, body, result, location) =>
      val expression = Lazy { stack =>
        val id = makeIdentifier(nameOpt)

        translatedExpressions.deferredQueue += (() => {
          val innerTranslatedExpressions = new TranslatedExpressions

          val innerTypeEnv = typeEnv -- typeParams

          val translatedParams = params.map { param =>
            val paramId = makeIdentifier(extractNameOpt(param._1))
            val tpe = translateType(param._3).resolve(innerTypeEnv)
            param._1 -> (paramId, translateEvaluation(param._2), tpe,
              TranslationResult(Lazy(_ => None), Lazy(_ => Core.ExpressionRef(paramId, tpe)), Lazy(_ => paramId), Lazy(_ => paramId)))
          }

          lazy val innerEnv: Env = env ++ translatedParams.map(x => (x._1, x._2._4)) ++ translatedBody
          lazy val translatedBody = body.map { entry =>
            entry._1 -> translateExpressionArg(entry._2, innerEnv, innerTypeEnv, extractNameOpt(entry._1), innerTranslatedExpressions)
          }

          val translatedResult = translateExpressionArg(result, innerEnv, innerTypeEnv, None, innerTranslatedExpressions)

          innerTranslatedExpressions.complete()

          val f = Core.FunctionExpression(typeParams, translatedParams.map(x => (x._2._1, x._2._2, x._2._3)), innerTranslatedExpressions.expressions.toMap, translatedResult.expression.get(location :: stack), location)
          translatedExpressions.expressions += id -> f
        })

        Core.ExpressionRef(id, translateType(expr.tpe).resolve(typeEnv), location)
      }
      val value: Lazy[Option[TypeApplicable]] = Lazy(_ => Some(typeArgs => (args => {
        lazy val innerEnv: Env = env ++ params.map(_._1).zip(args) ++
          body.map(e => (e._1, translateExpressionArg(e._2, innerEnv, typeEnv, extractNameOpt(e._1), translatedExpressions)))
        val innerTypeEnv = typeEnv ++ typeParams.zip(typeArgs).map(x => (x._1, x._2))
        translateExpressionArg(result, innerEnv, innerTypeEnv, nameOpt, translatedExpressions)
      })))
      TranslationResult(value, expression, expression.map(_.id), expression.map(_.id))
    case Typed.RecordConstructorExpression(entries, location) =>
      val translatedEntries = entries.mapValues(translateExpressionArg(_, env, typeEnv, nameOpt, translatedExpressions))
      val value = RuntimeEvaluator.Record(translatedEntries.map(x => (x._1.id, x._2)))
      val expression = Lazy(stack => Core.RecordConstructorExpression(translatedEntries.map(x => (x._1, x._2.expression.get(location :: stack)))))
      mkResult(Lazy(_ => Some(value)), expression, nameOpt, translatedExpressions)
    case Typed.RecordAccesorExpression(name, entries, location) =>
      lazy val translatedEntries = translateExpressionArg(entries, env, typeEnv, nameOpt, translatedExpressions)
      val value = Lazy { stack =>
        translatedEntries.value.get(location :: stack).map(_.asInstanceOf[RuntimeEvaluator.Record].entries(name.id).asInstanceOf[TranslationResult])
      }
      val expression = Lazy { stack =>
        value.get(stack).map(_.expression.get(location :: stack)).getOrElse(Core.RecordAccesorExpression(name, translatedEntries.expression.get(location :: stack), location))
      }
      mkResult(value.map(_.map(_.value)), expression, nameOpt, translatedExpressions)
    case Typed.ApplicationExpression(applicable, args, location) =>
      lazy val translatedApplicable = translateExpressionArg(applicable, env, typeEnv, nameOpt, translatedExpressions)
      val Typed.FunctionType(_, paramTypes, _, _) = applicable.tpe
      if (paramTypes.size != args.size) {
        throw InternalError(s"Wrong number of arguments.", location)
      }
      lazy val translatedArgs = args.map(translateExpressionArg(_, env, typeEnv, nameOpt, translatedExpressions))

      val value = Lazy { stack =>
        // TODO: handle strict arguments differently here?
        translatedApplicable.value.get(location :: stack).map(x => x.asInstanceOf[Applicable](translatedArgs))
      }
      val expression = Lazy { stack =>
        value.get(Nil).map(_.expression.get(location :: stack)).getOrElse(mkApplicationExpression(
          translatedApplicable.expression.get(location :: stack), paramTypes, translatedArgs, location :: stack))
      }
      mkResult(value, expression, nameOpt, translatedExpressions)
    case Typed.TypeApplicationExpression(applicable, typeArgs, location) =>
      val Typed.FunctionType(typeParams, _, _, _) = applicable.tpe
      lazy val translatedApplicable = translateExpressionArg(applicable, env, typeEnv, nameOpt, translatedExpressions)
      val value: Lazy[Option[Applicable]] = Lazy { stack =>
        translatedApplicable.value.get(location :: stack).map(x => x.asInstanceOf[TypeApplicable](typeArgs.map(translateType(_).resolve(typeEnv))))
      }
      val expression = Lazy { stack =>
        Core.TypeApplicationExpression(translatedApplicable.expression.get(location :: stack), typeArgs.map(translateType(_).resolve(typeEnv)))
      }
      mkResult(value, expression, nameOpt, translatedExpressions)
  }

  def mkApplicationExpression(applicable: Core.ExpressionArg, paramTypes: List[(Typed.Evaluation, Typed.Type)], args: List[TranslationResult], stack: Stack) =
    Core.ApplicationExpression(
      applicable,
      args.zip(paramTypes).map { x =>
        if (x._2._1 != TesslaAST.LazyEvaluation) {
          x._1.expression.get(stack)
        } else {
          Core.ExpressionRef(x._1.lazyId.get(stack), translateType(x._2._2))
        }
      })

  def translateEvaluation(evaluation: Typed.Evaluation): Core.Evaluation = evaluation match {
    case TesslaAST.LazyEvaluation => TesslaAST.LazyEvaluation
    case _ => TesslaAST.StrictEvaluation
  }

  def translateType(tpe: Typed.Type): Core.Type = tpe match {
    case Typed.FunctionType(typeParams, paramTypes, resultType, location) =>
      Core.FunctionType(typeParams, paramTypes.map(x => (translateEvaluation(x._1), translateType(x._2))), translateType(resultType))
    case Typed.InstatiatedType(name, typeArgs, location) =>
      Core.InstatiatedType(name, typeArgs.map(translateType), location)
    case Typed.RecordType(entries, location) =>
      Core.RecordType(entries.mapValues(translateType), location)
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

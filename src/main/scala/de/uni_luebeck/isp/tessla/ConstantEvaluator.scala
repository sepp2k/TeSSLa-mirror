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
    val identifiers = mutable.Map[Identifier, Identifier]()
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
    f(stack)
  }

  case class TranslationResult(value: Lazy[Option[Any]], expression: Lazy[Core.ExpressionArg])

  def mkResult(value: Option[Any], expression: Core.ExpressionArg) = TranslationResult(Lazy(_ => value), Lazy(_ => expression))

  def mkExpressionResult(expression: Lazy[Core.ExpressionArg]) = TranslationResult(Lazy(_ => None), expression)

  type Reifier = (String, Any, Core.Type) => (List[Core.ExpressionArg] => Core.Expression, List[Any])

  val reifiers = Map[String, Reifier]()

  def reify(value: Any, tpe: Core.Type): (Core.Expression, Boolean) = {
    ???
  }


  type TypeApplicable = List[Core.Type] => Applicable
  type Applicable = List[TranslationResult] => TranslationResult

  val externs = Map[String, TypeApplicable]()

  val lastExtern = Core.ExternExpression(
    List(Identifier("A"), Identifier("B")),
    List(
      (Identifier("a"), TesslaAST.LazyEvaluation, Core.TypeParam(Identifier("A"))),
      (Identifier("b"), TesslaAST.StrictEvaluation, Core.TypeParam(Identifier("B")))),
    Core.TypeParam(Identifier("A")),
    "last"
  )

  val streamExterns: Map[String, TypeApplicable] = Map(
    "last" -> (typeArguments => (arguments => {
      mkExpressionResult(Lazy { stack => Core.ApplicationExpression(Core.TypeApplicationExpression(lastExtern, typeArguments), arguments.map(_.expression.get(stack))) })
    }))
  )

  override def translateSpec(): Core.Specification = {
    val translatedExpressions = new TranslatedExpressions

    val inputs: Env = spec.in.map { i =>
      i._1 -> mkResult(None, Core.ExpressionRef(i._1, translateType(i._2._1)))
    }

    lazy val env: Env = inputs ++ spec.definitions.map { entry =>
      (entry._1, translateExpressionArg(entry._2, env, Map(), extractNameOpt(entry._1), translatedExpressions))
    }

    translatedExpressions.complete()

    val outputs = spec.out.map(x => (translatedExpressions.identifiers(x._1), x._2, Nil))

    Core.Specification(spec.in.mapValues(x => (translateType(x._1), Nil)), translatedExpressions.expressions.toMap, outputs)
  }

  def createEnvForDefs(defs: Map[Identifier, Typed.ExpressionArg], parent: Env, typeEnv: TypeEnv, translatedFunctions: TranslatedExpressions): Env = {
    lazy val entries: Env = parent ++ defs.map { entry =>
      (entry._1, translateExpressionArg(entry._2, entries, typeEnv, extractNameOpt(entry._1), translatedFunctions))
    }
    entries
  }

  def extractNameOpt(id: Identifier) = {
    val namePos = id.id.indexOf("$")
    if (namePos > 0) Some(id.id.substring(0, namePos)) else None
  }

  def translateExpressionArg(expr: Typed.ExpressionArg, env: Env, typeEnv: TypeEnv, nameOpt: Option[String],
    translatedExpressions: TranslatedExpressions
  ): TranslationResult = expr match {
    case Typed.ExpressionRef(id, tpe, location) =>
      val newId = makeIdentifier(extractNameOpt(id), id.location)
      translatedExpressions.identifiers += id -> newId
      val result = env(id)
      TranslationResult(result.value, Lazy { stack =>
        translatedExpressions.expressions += newId -> result.expression.get(location :: stack)
        Core.ExpressionRef(newId, translateType(tpe).resolve(typeEnv), location)
      })
    case Typed.IntLiteralExpression(value, location) =>
      mkResult(Some(value), Core.IntLiteralExpression(value, location))
    case Typed.FloatLiteralExpression(value, location) =>
      mkResult(Some(value), Core.FloatLiteralExpression(value, location))
    case Typed.StringLiteralExpression(value, location) =>
      mkResult(Some(value), Core.StringLiteralExpression(value, location))
    case Typed.ExternExpression(typeParams, params, resultType, name, location) =>
      val expression = Core.ExternExpression(typeParams, params.map(x => (x._1, translateEvaluation(x._2), translateType(x._3).resolve(typeEnv))), translateType(resultType).resolve(typeEnv), name, location)
      val value: Lazy[Option[TypeApplicable]] = Lazy { stack =>
        Some(typeArgs => (args =>
          try {
            externs(name)(typeArgs)(args)
          } catch {
            case e: RuntimeException => mkResult(Some(e), Core.ApplicationExpression(expression, args.map(_.expression.get(stack)))) // TODO smarter error handling?
          }))
      }
      TranslationResult(value, Lazy { stack => expression })
    case Typed.FunctionExpression(typeParams, params, body, result, location) =>
      val expression = Lazy { stack =>
        val id = makeIdentifier(nameOpt)

        translatedExpressions.deferredQueue += (() => {
          val innerTranslatedExpressions = new TranslatedExpressions

          val innerTypeEnv = typeEnv -- typeParams

          val translatedParams = params.map { param =>
            val paramId = makeIdentifier(extractNameOpt(param._1))
            val tpe = translateType(param._3).resolve(innerTypeEnv)
            param._1 -> (paramId, translateEvaluation(param._2), tpe, mkResult(None, Core.ExpressionRef(paramId, tpe)))
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
      TranslationResult(value, expression)
    case Typed.RecordConstructorExpression(entries, location) =>
      val translatedEntries = entries.mapValues(translateExpressionArg(_, env, typeEnv, nameOpt, translatedExpressions))
      val value = RuntimeEvaluator.Record(translatedEntries.map(x => (x._1.id, x._2)))
      val expression = Lazy(stack => Core.RecordConstructorExpression(translatedEntries.map(x => (x._1, x._2.expression.get(location :: stack)))))
      TranslationResult(Lazy(_ => Some(value)), expression)
    case Typed.RecordAccesorExpression(name, entries, location) =>
      val translatedEntries = translateExpressionArg(entries, env, typeEnv, nameOpt, translatedExpressions)
      val value = Lazy { stack =>
        translatedEntries.value.get(location :: stack).map(_.asInstanceOf[RuntimeEvaluator.Record].entries(name.id).asInstanceOf[TranslationResult])
      }
      val expression = Lazy { stack =>
        value.get(stack).map(_.expression.get(location :: stack)).getOrElse(Core.RecordAccesorExpression(name, translatedEntries.expression.get(location :: stack), location))
      }
      TranslationResult(value.map(_.map(_.value)), expression)
    case Typed.ApplicationExpression(applicable, args, location) =>
      val translatedApplicable = translateExpressionArg(applicable, env, typeEnv, nameOpt, translatedExpressions)
      val Typed.FunctionType(_, paramTypes, _, _) = applicable.tpe
      if (paramTypes.size != args.size) {
        throw InternalError(s"Wrong number of arguments.", location)
      }
      val translatedArgs = args.map(translateExpressionArg(_, env, typeEnv, nameOpt, translatedExpressions)).zip(paramTypes)

      val value = Lazy { stack =>
        // TODO: handle strict arguments differently here?
        translatedApplicable.value.get(location :: stack).map(x => x.asInstanceOf[Applicable](translatedArgs.map(_._1)))
      }
      val id = makeIdentifier(nameOpt)
      translatedExpressions.deferredQueue += (() => {
        translatedExpressions.expressions += id ->
          value.get(Nil).map(_.expression.get(List(location))).getOrElse(Core.ApplicationExpression(
            translatedApplicable.expression.get(List(location)),
            translatedArgs.map(_._1.expression.get(List(location)))))

      })
      TranslationResult(value.map(_.map(_.value)), Lazy { stack =>
        translatedArgs.foreach { arg =>
          if (arg._2._1 != TesslaAST.LazyEvaluation) {
            arg._1.expression.get(location :: stack)
          }
        }
        Core.ExpressionRef(id, translateType(expr.tpe).resolve(typeEnv))
      })
    case Typed.TypeApplicationExpression(applicable, typeArgs, location) =>
      val Typed.FunctionType(typeParams, _, _, _) = applicable.tpe
      val translatedApplicable = translateExpressionArg(applicable, env, typeEnv, nameOpt, translatedExpressions)
      val value: Lazy[Option[Applicable]] = Lazy { stack =>
        translatedApplicable.value.get(location :: stack).map(x => x.asInstanceOf[TypeApplicable](typeArgs.map(translateType(_).resolve(typeEnv))))
      }
      val expression = Lazy { stack =>
        Core.TypeApplicationExpression(translatedApplicable.expression.get(location :: stack), typeArgs.map(translateType(_).resolve(typeEnv)))
      }
      TranslationResult(value, expression)
  }

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
  }

  private var counter = 0

  def makeIdentifier(nameOpt: Option[String], location: Location = Location.unknown): Identifier = {
    counter += 1
    new Identifier(nameOpt.getOrElse("") + "$" + counter, location)
  }

}

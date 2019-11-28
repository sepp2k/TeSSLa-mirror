package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util._

import scala.util.Try
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

  def Lazy[A](f: Stack => A): Lazy[A] = LazyWithStack[A, Stack](f)

  case class TranslationResult(value: Lazy[Option[Any]], expression: Lazy[Core.ExpressionArg])

  def mkResult(value: Option[Any], expression: Core.ExpressionArg) = TranslationResult(LazyWithStack(_ => value), LazyWithStack(_ => expression))

  type Reifier = (String, Any, Core.Type) => (List[Core.ExpressionArg] => Core.Expression, List[Any])

  val reifiers = Map[String, Reifier]()

  def reify(value: Any, tpe: Core.Type): (Core.Expression, Boolean) = {
    ???
  }

  type Applicable = List[TranslationResult] => TranslationResult

  val externs = Map[String, Applicable]()

  sealed abstract class XTranslationResult

  case class XStreamEntry(streamId: Identifier) extends XTranslationResult

  case class XInputStreamEntry(name: String) extends XTranslationResult

  case class XNilEntry(nil: Core.Expression) extends XTranslationResult

  case class XValueEntry(value: TesslaCore.ValueOrError) extends XTranslationResult

  case class XObjectEntry(members: Map[String, XTranslationResult], loc: Location) extends XTranslationResult

  case class XFunctionEntry(f: Core.FunctionExpression, id: Identifier) extends XTranslationResult

  case class XMacroEntry(mac: TypedTessla.Macro, closure: Env, functionValue: Option[Core.ExpressionRef], translatedExpressions: TranslatedExpressions) extends XTranslationResult {
    override def toString = s"MacroEntry($mac, ...)"
  }

  case class XBuiltInEntry(builtIn: TypedTessla.BuiltInOperator, typ: TypedTessla.Type) extends XTranslationResult {
    def name = builtIn.name

    def parameters = builtIn.parameters
  }

  sealed abstract class XExpressionEntry extends XTranslationResult

  case class XFunctionParameterEntry(id: Identifier) extends XExpressionEntry

  case class VXalueExpressionEntry(exp: Core.Expression, id: Identifier) extends XExpressionEntry

  override def translateSpec(): Core.Specification = {
    val translatedExpressions = new TranslatedExpressions

    val inputs: Env = spec.in.map {i =>
      i._1 -> mkResult(None, Core.ExpressionRef(i._1, translateType(i._2._1, Map())))
    }

    lazy val env: Env = inputs ++ spec.definitions.map { entry =>
      (entry._1, translateExpressionArg(entry._2, env, Map(), extractNameOpt(entry._1), translatedExpressions))
    }

    translatedExpressions.complete()

    val outputs = spec.out.map(x => (translatedExpressions.identifiers(x._1), x._2, Nil))

    Core.Specification(spec.in.mapValues(x => (translateType(x._1, Map()), Nil)), translatedExpressions.expressions.toMap, outputs)
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

  // TODO: move to LazyWithStack
  //  val entry = env(id)
  //  if (stack.size > 1000) {
  //    throw WithStackTrace(Errors.StackOverflow(location), stack)
  //  } else if (entry.isComputing) {
  //    throw WithStackTrace(InfiniteRecursion(location), stack)
  //  } else {
  //    try {
  //      entry.get(location :: stack)
  //    } catch {
  //      case err: InternalError =>
  //        throw WithStackTrace(err, location :: stack)
  //      case _: StackOverflowError =>
  //        throw WithStackTrace(Errors.StackOverflow(location), stack)
  //    }
  //  }


  def translateExpressionArg(expr: Typed.ExpressionArg, env: Env, typeEnv: TypeEnv, nameOpt: Option[String],
    translatedExpressions: TranslatedExpressions
  ): TranslationResult = expr match {
    case Typed.ExpressionRef(id, tpe, location) => {
      val newId = makeIdentifier(extractNameOpt(id), id.location)
      translatedExpressions.identifiers += id -> newId
      val result = env(id)
      TranslationResult(result.value, Lazy { stack =>
        translatedExpressions.expressions += newId -> result.expression.get(location :: stack)
        Core.ExpressionRef(newId, translateType(tpe, typeEnv), location)
      })
    }
    case Typed.IntLiteralExpression(value, location) =>
      mkResult(Some(value), Core.IntLiteralExpression(value, location))
    case Typed.FloatLiteralExpression(value, location) =>
      mkResult(Some(value), Core.FloatLiteralExpression(value, location))
    case Typed.StringLiteralExpression(value, location) =>
      mkResult(Some(value), Core.StringLiteralExpression(value, location))
    case Typed.ExternExpression(typeParams, params, resultType, name, location) =>
      val expression = Core.ExternExpression(typeParams, params.map(x => (x._1, translateEvaluation(x._2), translateType(x._3, typeEnv))), translateType(resultType, typeEnv), name, location)
      val value: Lazy[Option[Applicable]] = Lazy { stack =>
        Some(args =>
          try {
            externs(name)(args)
          } catch {
            case e: RuntimeException => mkResult(Some(e), Core.ApplicationExpression(expression, args.map(_.expression.get(stack)))) // TODO smarter error handling?
          })
      }
      TranslationResult(value, Lazy { stack => expression })
    case Typed.FunctionExpression(typeParams, params, body, result, location) =>
      val expression = Lazy { stack =>
        val id = makeIdentifier(nameOpt)

        translatedExpressions.deferredQueue += (() => {
          val innerTranslatedExpressions = new TranslatedExpressions

          val translatedParams = params.map { param =>
            val paramId = makeIdentifier(extractNameOpt(param._1))
            val tpe = translateType(param._3, typeEnv)
            param._1 -> (paramId, translateEvaluation(param._2), tpe, mkResult(None, Core.ExpressionRef(paramId, tpe)))
          }

          lazy val innerEnv: Env = env ++ translatedParams.map(x => (x._1, x._2._4)) ++ translatedBody
          lazy val translatedBody = body.map { entry =>
            entry._1 -> translateExpressionArg(entry._2, innerEnv, typeEnv, extractNameOpt(entry._1), innerTranslatedExpressions)
          }

          val translatedResult = translateExpressionArg(result, innerEnv, typeEnv, None, innerTranslatedExpressions)

          innerTranslatedExpressions.complete()

          val f = Core.FunctionExpression(List(), translatedParams.map(x => (x._2._1, x._2._2, x._2._3)), innerTranslatedExpressions.expressions.toMap, translatedResult.expression.get(location :: stack), location)
          translatedExpressions.expressions += id -> f
        })

        Core.ExpressionRef(id, translateType(expr.tpe, typeEnv), location)
      }
      val value: Lazy[Option[Applicable]] = Lazy(_ => Some(args => {
        lazy val innerEnv: Env = env ++ params.map(_._1).zip(args) ++
          body.map(e => (e._1, translateExpressionArg(e._2, innerEnv, typeEnv, extractNameOpt(e._1), translatedExpressions)))
        translateExpressionArg(result, innerEnv, typeEnv, nameOpt, translatedExpressions)
      }))
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
        translatedArgs.foreach {arg =>
          if (arg._2._1 != TesslaAST.LazyEvaluation) {
            arg._1.expression.get(location :: stack)
          }
        }
        Core.ExpressionRef(id, translateType(expr.tpe, typeEnv))
      })
    case Typed.TypeApplicationExpression(applicable, typeArgs, location) => ???
  }

  def translateEvaluation(evaluation: Typed.Evaluation): Core.Evaluation = evaluation match {
    case TesslaAST.LazyEvaluation => TesslaAST.LazyEvaluation
    case _ => TesslaAST.StrictEvaluation
  }

  def translateType(tpe: Typed.Type, typeEnv: TypeEnv): Core.Type = tpe match {
    case Typed.FunctionType(typeParams, paramTypes, resultType, location) =>
      Core.FunctionType(typeParams, paramTypes.map(x => (translateEvaluation(x._1), translateType(x._2, typeEnv))), translateType(resultType, typeEnv))
    case Typed.InstatiatedType(name, typeArgs, location) =>
      Core.InstatiatedType(name, typeArgs.map(translateType(_, typeEnv)), location)
    case Typed.RecordType(entries, location) =>
      Core.RecordType(entries.mapValues(translateType(_, typeEnv)), location)
    case Typed.TypeParam(name, location) =>
      Core.TypeParam(name, location)
  }

  private var counter = 0

  def makeIdentifier(nameOpt: Option[String], location: Location = Location.unknown): Identifier = {
    counter += 1
    new Identifier(nameOpt.getOrElse("") + "$" + counter, location)
  }

}

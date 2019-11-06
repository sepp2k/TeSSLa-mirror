package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.InternalError
import de.uni_luebeck.isp.tessla.TesslaCore._

import scala.collection.mutable.{Set => MutableSet}

class RemoveUnusedDefinitions(spec: TesslaCore.Specification)
  extends TranslationPhase.Translator[TesslaCore.Specification] {

  def removeUnusedFunction(function: Function): Function = {
    val used = MutableSet.empty[Long]

    def usageExp(exp: ValueExpression): Unit = exp match {
      case Function(_, body, result, _) =>
        body.values.map(_.exp).foreach(usageExp)
        usageValueArg(result)
      case IfThenElse(cond, thenCase, elseCase, _) =>
        usageValueArg(cond)
        usageValueArg(thenCase.get)
        usageValueArg(elseCase.get)
      case Application(f, args, _) =>
        usageValueArg(f.get)
        args.foreach(usageValueArg)
      case MemberAccess(obj, _, _) =>
        usageValueArg(obj)
    }

    def usageValueArg(valueArg: ValueArg): Unit = valueArg match {
      case ValueExpressionRef(id) if function.body.contains(id) =>
        used.add(id.uid)
        usageExp(function.body(id).exp)
      case ObjectCreation(members, _) => members.values.foreach(usageValueArg)
      case _ => // no usage at all
    }

    usageValueArg(function.result)

    val newBody = function.body.collect{
      case (id, desc) if used(id.uid) => (id, desc.copy(exp = removeUnusedValueExpression(desc.exp)))
    }

    Function(function.parameters, newBody, function.result, function.loc)
  }

  def removeUnusedValueExpression(e: ValueExpression): ValueExpression = e match {
    case f: Function => removeUnusedFunction(f)
    case x => x
  }

  override def translateSpec(): TesslaCore.Specification = {
    val streams = spec.streams.map { stream => stream.id -> stream }.toMap

    val used = MutableSet.empty[Long]
    def usageRef(ref: StreamRef): Unit = {
      ref match {
        case s: Stream =>
          if (!used(s.id.uid)) {
            used.add(s.id.uid)
            usageExp(streams(s.id).expression)
          }
        case _ => // ignore nil and input streams
      }
    }
    def usageOut(s: OutStreamDescription): Unit = s match {
      case OutStreamDescription(_, ref, _) => usageRef(ref)
    }
    def usageExp(exp: Expression): Unit = exp match {
      case TesslaCore.Last(values, clock, _) =>
        usageRef(values)
        usageRef(clock)
      case TesslaCore.Delay(delays, resets, _) =>
        usageRef(delays)
        usageRef(resets)
      case c: TesslaCore.CustomBuiltInCall =>
        c.streamArgs.foreach(usageRef)
    }
    spec.outStreams.foreach(usageOut)

    val updatedStreams = spec.streams.collect {
      case sd if used(sd.id.uid) =>
        val newExpression = sd.expression match {
          case call: CustomBuiltInCall if call.name == "lift" =>
            val closure = call.args.last match {
              case c: TesslaCore.Closure =>
                c.copy(function = removeUnusedFunction(c.function))
              case arg => throw InternalError(s"Expected closure argument, but got ${arg}", call.loc)
            }
            call.copy(args = call.args.init :+ closure)
          case _ => sd.expression
        }
        sd.copy(expression = newExpression)
    }

    val updatedOutStreams = spec.outStreams.filter {
      case TesslaCore.OutStreamDescription(_, ref, _) => ref match {
        case _: Nil => false
        case _ => true
      }
    }

    spec.copy(streams = updatedStreams, outStreams = updatedOutStreams)
  }
}

object RemoveUnusedDefinitions extends TranslationPhase[TesslaCore.Specification, TesslaCore.Specification] {
  override def translate(spec: TesslaCore.Specification) = {
    new RemoveUnusedDefinitions(spec).translate()
  }
}
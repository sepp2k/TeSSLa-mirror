package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TesslaCore._
import scala.collection.mutable.{Set => MutableSet}

class RemoveUnusedDefinitions(spec: TesslaCore.Specification)
  extends TranslationPhase.Translator[TesslaCore.Specification] {

  def removeUnusedFunction(function: Function): Function = {
    val used = MutableSet.empty[Long]

    def usageExp(exp: ValueExpression): Unit = exp match {
      case Function(_, body, result, _) =>
        body.values.foreach(usageExp)
        usageValueArg(result)
      case IfThenElse(cond, thenCase, elseCase, _) =>
        usageValueArg(cond)
        usageValueArg(thenCase.get)
        usageValueArg(elseCase.get)
      case Application(f, args, _) =>
        usageValueArg(f.get)
        args.map(_.get).foreach(usageValueArg)
      case MemberAccess(obj, _, _) =>
        usageValueArg(obj)
    }

    def usageValueArg(valueArg: ValueArg): Unit = valueArg match {
      case ValueExpressionRef(id) if function.body.contains(id) =>
        used.add(id.uid)
        usageExp(function.body(id))
      case ObjectCreation(members, _) => members.values.foreach(usageValueArg)
      case _ => // no usage at all
    }

    usageValueArg(function.result)

    val newBody = function.body.collect{
      case (id, e) if used(id.uid) => (id, removeUnusedValueExpression(e))
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
        case Stream(id, _) =>
          if (!used(id.uid)) {
            used.add(id.uid)
            usageExp(streams(id).expression)
          }
        case _ => // ignore nil and input streams
      }
    }
    def usageOut(s: OutStreamDescription): Unit = s match {
      case OutStreamDescription(_, ref, _) => usageRef(ref)
    }
    def usageExp(exp: Expression): Unit = exp match {
      case TesslaCore.SignalLift(op, argStreams, _)  =>
        argStreams.foreach(usageRef)
      case TesslaCore.Lift(f, argStreams, _) =>
        argStreams.foreach(usageRef)
      case TesslaCore.Default(values, _, _) =>
        usageRef(values)
      case TesslaCore.DefaultFrom(values, defaults, _) =>
        usageRef(values)
        usageRef(defaults)
      case TesslaCore.Last(values, clock, _) =>
        usageRef(values)
        usageRef(clock)
      case TesslaCore.Delay(delays, resets, _) =>
        usageRef(delays)
        usageRef(resets)
      case TesslaCore.Time(values, _) =>
        usageRef(values)
      case c: TesslaCore.CustomBuiltInCall =>
        c.streamArgs.foreach(usageRef)
    }
    spec.outStreams.foreach(usageOut)

    val updatedStreams = spec.streams.collect{
      case StreamDescription(id, expression, typ) if used(id.uid) =>
        val newExpression = expression match {
          case Lift(f, args, loc) =>
            Lift(removeUnusedFunction(f), args, loc)
          case _ => expression
        }
        StreamDescription(id, newExpression, typ)
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
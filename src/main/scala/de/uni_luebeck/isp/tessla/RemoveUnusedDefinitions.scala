package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TesslaCore._
import scala.collection.mutable.{Set => MutableSet}

class RemoveUnusedDefinitions extends TranslationPhase[TesslaCore.Specification, TesslaCore.Specification] {

  def removeUnusedFunction(function: Function): Function = {
    val used = MutableSet.empty[Long]

    def usageExp(exp: ValueExpression): Unit = exp match {
      case Function(_, scope, body, _) =>
        scope.values.foreach(usageExp)
        usageValueArg(body)
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
      case ValueExpressionRef(id) if function.scope.contains(id) =>
        used.add(id.uid)
        usageExp(function.scope(id))
      case ObjectCreation(members, _) => members.values.foreach(usageValueArg)
      case _ => // no usage at all
    }

    usageValueArg(function.body)

    // TODO handle nested functions
    val newScope = function.scope.collect{
      case (id, e) if used(id.uid) => (id, removeUnusedValueExpression(e))
    }

    Function(function.parameters, newScope, function.body, function.loc)
  }

  def removeUnusedValueExpression(e: ValueExpression): ValueExpression = e match {
    case f: Function => removeUnusedFunction(f)
    case x => x
  }

  def removeUnusedValueArg(valueArg: ValueArg): ValueArg = valueArg match {
    case Closure(f, env, loc) => Closure(removeUnusedFunction(f), env, loc)
    case x => x
  }

  override def translateSpec(spec: TesslaCore.Specification): TesslaCore.Specification = {
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
      case TesslaCore.Const(_, stream, _) =>
        usageRef(stream)
      case TesslaCore.Last(values, clock, _) =>
        usageRef(values)
        usageRef(clock)
      case TesslaCore.DelayedLast(values, delays, _) =>
        usageRef(values)
        usageRef(delays)
      case TesslaCore.Delay(delays, resets, _) =>
        usageRef(delays)
        usageRef(resets)
      case TesslaCore.Time(values, _) =>
        usageRef(values)
      case TesslaCore.StdLibCount(values, _) =>
        usageRef(values)
      case TesslaCore.Merge(arg1, arg2, _) =>
        usageRef(arg1)
        usageRef(arg2)
      case TesslaCore.Filter(events, condition, _) =>
        usageRef(events)
        usageRef(condition)
    }
    spec.outStreams.foreach(usageOut)

    val updatedStreams = spec.streams.collect{
      case StreamDescription(id, expression, typ) if used(id.uid) =>
        val newExpression = expression match {
          case Lift(f, args, loc) =>
            Lift(removeUnusedValueArg(f), args, loc)
          case _ => expression
        }
        StreamDescription(id, newExpression, typ)
    }

    TesslaCore.Specification(updatedStreams, spec.inStreams, spec.outStreams)
  }
}

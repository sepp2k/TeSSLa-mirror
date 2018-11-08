package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition

object Evaluator {
  def getInt(voe: TesslaCore.ValueOrError): BigInt = voe.forceValue match {
    case intLit: TesslaCore.IntValue => intLit.value
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: Int, got: $v", v.loc)
  }

  def getBool(voe: TesslaCore.ValueOrError): Boolean = voe.forceValue match {
    case boolLit: TesslaCore.BoolValue => boolLit.value
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: Bool, got: $v", v.loc)
  }

  def getString(voe: TesslaCore.ValueOrError): String = voe.forceValue match {
    case stringLit: TesslaCore.StringValue => stringLit.value
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: String, got: $v", v.loc)
  }

  def getOption(voe: TesslaCore.ValueOrError): TesslaCore.TesslaOption = voe.forceValue match {
    case optionValue: TesslaCore.TesslaOption => optionValue
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: Option ${v.loc}, got: $v", v.loc)
  }

  def getCtf(voe: TesslaCore.ValueOrError): ICompositeDefinition = voe.forceValue match {
    case ctfLit: TesslaCore.Ctf => ctfLit.value
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: CTF, got: $v", v.loc)
  }

  def getMap(voe: TesslaCore.ValueOrError): TesslaCore.TesslaMap = voe.forceValue match {
    case mapLit: TesslaCore.TesslaMap => mapLit
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: Map, got: $v", v.loc)
  }

  def getSet(voe: TesslaCore.ValueOrError): TesslaCore.TesslaSet = voe.forceValue match {
    case setLit: TesslaCore.TesslaSet => setLit
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: Set, got: $v", v.loc)
  }

  def evalPrimitiveOperator(op: BuiltIn.PrimitiveOperator,
                            arguments: Seq[TesslaCore.ValueOrError],
                            loc: Location): Option[TesslaCore.ValueOrError] = {
    def binIntOp(op: (BigInt, BigInt) => BigInt) = {
      Some(TesslaCore.IntValue(op(getInt(arguments(0)), getInt(arguments(1))), loc))
    }

    def binIntComp(op: (BigInt, BigInt) => Boolean) = {
      Some(TesslaCore.BoolValue(op(getInt(arguments(0)), getInt(arguments(1))), loc))
    }
    // TODO: Replace try-catch using ValueOrError.mapValue (or add ValueOrError.flatMapValue if needed)
    try {
      op match {
        case BuiltIn.Add => binIntOp(_ + _)
        case BuiltIn.Sub => binIntOp(_ - _)
        case BuiltIn.Mul => binIntOp(_ * _)
        case BuiltIn.Div =>
          val x = getInt(arguments(0))
          val y = getInt(arguments(1))
          if (y == 0) Some(TesslaCore.Error(DivideByZero(arguments(1).forceValue.loc)))
          else Some(TesslaCore.IntValue(x / y, loc))
        case BuiltIn.LeftShift => binIntOp(_ << _.toInt)
        case BuiltIn.RightShift => binIntOp(_ >> _.toInt)
        case BuiltIn.BitAnd => binIntOp(_ & _)
        case BuiltIn.BitOr => binIntOp(_ | _)
        case BuiltIn.BitXor => binIntOp(_ ^ _)
        case BuiltIn.BitFlip => Some(TesslaCore.IntValue(~getInt(arguments(0)), loc))
        case BuiltIn.Negate => Some(TesslaCore.IntValue(-getInt(arguments(0)), loc))
        case BuiltIn.Eq => Some(TesslaCore.BoolValue(arguments(0).forceValue == arguments(1).forceValue, loc))
        case BuiltIn.Neq => Some(TesslaCore.BoolValue(arguments(0).forceValue != arguments(1).forceValue, loc))
        case BuiltIn.Lt => binIntComp(_ < _)
        case BuiltIn.Lte => binIntComp(_ <= _)
        case BuiltIn.Gt => binIntComp(_ > _)
        case BuiltIn.Gte => binIntComp(_ >= _)
        case BuiltIn.And => Some(TesslaCore.BoolValue(getBool(arguments(0)) && getBool(arguments(1)), loc))
        case BuiltIn.Or => Some(TesslaCore.BoolValue(getBool(arguments(0)) || getBool(arguments(1)), loc))
        case BuiltIn.Not => Some(TesslaCore.BoolValue(!getBool(arguments(0)), loc))
        case BuiltIn.IfThen =>
          if (getBool(arguments(0))) Some(arguments(1))
          else None
        case BuiltIn.IfThenElse =>
          if (getBool(arguments(0))) Some(arguments(1))
          else Some(arguments(2))
        case BuiltIn.First =>
          Some(arguments(0))
        case BuiltIn.None =>
          Some(TesslaCore.TesslaOption(None, loc))
        case BuiltIn.Some =>
          Some(TesslaCore.TesslaOption(Some(arguments(0).forceValue), loc))
        case BuiltIn.IsNone =>
          Some(TesslaCore.BoolValue(getOption(arguments(0)).value.isEmpty, loc))
        case BuiltIn.GetSome =>
          getOption(arguments(0)).value match {
            case None => throw CannotGetValueOfNone(loc)
            case some => some
          }
        case BuiltIn.MapEmpty =>
          Some(TesslaCore.TesslaMap(Map(), loc))
        case BuiltIn.MapAdd =>
          val map = getMap(arguments(0))
          Some(TesslaCore.TesslaMap(map.value + (arguments(1).forceValue -> arguments(2).forceValue), loc))
        case BuiltIn.MapGet =>
          val map = getMap(arguments(0))
          val key = arguments(1).forceValue
          try {
            Some(map.value(key).withLoc(loc))
          } catch {
            case _: NoSuchElementException =>
              throw KeyNotFound(key.forceValue, map.value, loc)
          }
        case BuiltIn.MapContains =>
          Some(TesslaCore.BoolValue(getMap(arguments(0)).value.contains(arguments(1).forceValue), loc))
        case BuiltIn.MapRemove =>
          val map = getMap(arguments(0))
          Some(TesslaCore.TesslaMap(map.value - arguments(1).forceValue, loc))
        case BuiltIn.MapSize =>
          val map = getMap(arguments(0))
          Some(TesslaCore.IntValue(map.value.size, loc))
        case BuiltIn.SetEmpty =>
          Some(TesslaCore.TesslaSet(Set(), loc))
        case BuiltIn.SetAdd =>
          val set = getSet(arguments(0))
          Some(TesslaCore.TesslaSet(set.value + arguments(1).forceValue, loc))
        case BuiltIn.SetContains =>
          Some(TesslaCore.BoolValue(getSet(arguments(0)).value.contains(arguments(1).forceValue), loc))
        case BuiltIn.SetRemove =>
          val set = getSet(arguments(0))
          Some(TesslaCore.TesslaSet(set.value - arguments(1).forceValue, loc))
        case BuiltIn.SetSize =>
          val set = getSet(arguments(0))
          Some(TesslaCore.IntValue(set.value.size, loc))
        case BuiltIn.SetUnion =>
          val set1 = getSet(arguments(0))
          val set2 = getSet(arguments(1))
          Some(TesslaCore.TesslaSet(set1.value | set2.value, loc))
        case BuiltIn.SetIntersection =>
          val set1 = getSet(arguments(0))
          val set2 = getSet(arguments(1))
          Some(TesslaCore.TesslaSet(set1.value & set2.value, loc))
        case BuiltIn.CtfGetInt =>
          val composite = getCtf(arguments(0))
          val key = getString(arguments(1))
          Some(TesslaCore.IntValue(Ctf.getInt(composite, key), loc))
        case BuiltIn.CtfGetString =>
          val composite = getCtf(arguments(0))
          val key = getString(arguments(1))
          Some(TesslaCore.StringValue(Ctf.getString(composite, key), loc))
      }
    } catch {
      case e: TesslaError =>
        Some(TesslaCore.Error(e))
    }
  }

  // The laziness here (and by extension in TesslaCore.Closure) is necessary, so that the
  // recursive definition of the inner environment when evaluating a closure application
  // does not chase its own tail into an infinite recursion. This laziness can't be contained
  // locally as this will resolve the laziness too soon and still run into the same problem -
  // at least in the case of nested functions.
  type Env = Map[TesslaCore.Identifier, Lazy[TesslaCore.ValueOrError]]

  def evalApplication(f: TesslaCore.ValueArg,
                      args: Seq[TesslaCore.ValueOrError],
                      loc: Location,
                      env: Env): TesslaCore.ValueOrError = {
    evalArg(f, env) match {
      case b: TesslaCore.BuiltInOperator =>
        evalPrimitiveOperator(b.value, args, loc).get
      case c: TesslaCore.Closure =>
        val env = c.capturedEnvironment
        val argEnv = c.function.parameters.zip(args.map(arg => Lazy(arg))).toMap
        lazy val innerEnv: Env =  env ++ argEnv ++ c.function.scope.map {
          case (id, e) => id -> Lazy(evalExpression(e, innerEnv))
        }
        evalArg(c.function.body, innerEnv)
      case _ =>
        throw InternalError("Application of non-function should have been caught by the type checker")
    }
  }

  def evalArg(arg: TesslaCore.ValueArg, env: Env): TesslaCore.ValueOrError = arg match {
    case value: TesslaCore.ValueOrError =>
      value
    case obj: TesslaCore.ObjectCreation =>
      TesslaCore.TesslaObject(obj.members.mapValues(evalArg(_, env).forceValue), obj.loc)
    case ref: TesslaCore.ValueExpressionRef =>
      env(ref.id).get
  }

  def evalExpression(exp: TesslaCore.ValueExpression, env: Env): TesslaCore.ValueOrError = exp match {
    case f: TesslaCore.Function =>
      TesslaCore.Closure(f, env, exp.loc)
    case a: TesslaCore.Application =>
      evalApplication(a.f, a.args.map(evalArg(_, env)), a.loc, env)
    case ma: TesslaCore.MemberAccess =>
      evalArg(ma.obj, env).mapValue {
        case obj: TesslaCore.TesslaObject =>
          obj.value(ma.member)
        case other =>
          throw InternalError(s"Member access on non-object ($other) should have been caught by type checker", ma.loc)
      }
  }
}
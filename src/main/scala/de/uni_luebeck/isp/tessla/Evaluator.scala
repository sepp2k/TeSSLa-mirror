package de.uni_luebeck.isp.tessla

import java.util.Locale

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition
import util.mapValues

object Evaluator {
  def getInt(voe: TesslaCore.ValueOrError): BigInt = voe.forceValue match {
    case intLit: TesslaCore.IntValue => intLit.value
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: Int, got: $v", v.loc)
  }

  def getFloat(voe: TesslaCore.ValueOrError): Double = voe.forceValue match {
    case floatLit: TesslaCore.FloatValue => floatLit.value
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

  def getList(voe: TesslaCore.ValueOrError): TesslaCore.TesslaList = voe.forceValue match {
    case listLit: TesslaCore.TesslaList => listLit
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: List, got: $v", v.loc)
  }

  def evalToString(arg: TesslaCore.ValueOrError): String = {
    arg.forceValue match {
      case s: TesslaCore.StringValue => s.value
      case _ => arg.toString
    }
  }

  def evalPrimitiveOperator(op: TesslaCore.CurriedPrimitiveOperator,
                            arguments: Seq[TesslaCore.ValueOrError],
                            loc: Location): TesslaCore.ValueOrError =
    if (op.args.isEmpty) {
      evalPrimitiveOperator(op.op, arguments, loc)
    } else {
      def f(curried: Int, arg: Int, acc: Seq[TesslaCore.ValueOrError]): Seq[TesslaCore.ValueOrError] =
        if (op.args.contains(curried)) {
          f(curried + 1, arg, acc :+ op.args(curried))
        } else if (arg < arguments.size) {
          f(curried + 1, arg + 1, acc :+ arguments(arg))
        } else {
          acc
        }
      evalPrimitiveOperator(op.op, f(0, 0, Seq()), loc)
    }

  def evalPrimitiveOperator(op: BuiltIn.PrimitiveOperator,
                            arguments: Seq[TesslaCore.ValueOrError],
                            loc: Location): TesslaCore.ValueOrError = {
    def binIntOp(op: (BigInt, BigInt) => BigInt) = {
      TesslaCore.IntValue(op(getInt(arguments(0)), getInt(arguments(1))), loc)
    }

    def binIntComp(op: (BigInt, BigInt) => Boolean) = {
      TesslaCore.BoolValue(op(getInt(arguments(0)), getInt(arguments(1))), loc)
    }

    def binFloatOp(op: (Double, Double) => Double) = {
      TesslaCore.FloatValue(op(getFloat(arguments(0)), getFloat(arguments(1))), loc)
    }

    def binFloatComp(op: (Double, Double) => Boolean) = {
      TesslaCore.BoolValue(op(getFloat(arguments(0)), getFloat(arguments(1))), loc)
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
          if (y == 0) TesslaCore.Error(DivideByZero(arguments(1).forceValue.loc))
          else TesslaCore.IntValue(x / y, loc)
        case BuiltIn.FAdd => binFloatOp(_ + _)
        case BuiltIn.FSub => binFloatOp(_ - _)
        case BuiltIn.FMul => binFloatOp(_ * _)
        case BuiltIn.FDiv => binFloatOp(_ / _)
        case BuiltIn.Mod =>
          val x = getInt(arguments(0))
          val y = getInt(arguments(1))
          if (y == 0) TesslaCore.Error(DivideByZero(arguments(1).forceValue.loc))
          else TesslaCore.IntValue(x % y, loc)
        case BuiltIn.LeftShift => binIntOp(_ << _.toInt)
        case BuiltIn.RightShift => binIntOp(_ >> _.toInt)
        case BuiltIn.BitAnd => binIntOp(_ & _)
        case BuiltIn.BitOr => binIntOp(_ | _)
        case BuiltIn.BitXor => binIntOp(_ ^ _)
        case BuiltIn.BitFlip => TesslaCore.IntValue(~getInt(arguments(0)), loc)
        case BuiltIn.Negate => TesslaCore.IntValue(-getInt(arguments(0)), loc)
        case BuiltIn.FNegate => TesslaCore.FloatValue(-getFloat(arguments(0)), loc)
        case BuiltIn.Eq => TesslaCore.BoolValue(arguments(0).forceValue == arguments(1).forceValue, loc)
        case BuiltIn.Neq => TesslaCore.BoolValue(arguments(0).forceValue != arguments(1).forceValue, loc)
        case BuiltIn.Lt => binIntComp(_ < _)
        case BuiltIn.Lte => binIntComp(_ <= _)
        case BuiltIn.Gt => binIntComp(_ > _)
        case BuiltIn.Gte => binIntComp(_ >= _)
        case BuiltIn.FLt => binFloatComp(_ < _)
        case BuiltIn.FLte => binFloatComp(_ <= _)
        case BuiltIn.FGt => binFloatComp(_ > _)
        case BuiltIn.FGte => binFloatComp(_ >= _)
        case BuiltIn.And => TesslaCore.BoolValue(getBool(arguments(0)) && getBool(arguments(1)), loc)
        case BuiltIn.Or => TesslaCore.BoolValue(getBool(arguments(0)) || getBool(arguments(1)), loc)
        case BuiltIn.Not => TesslaCore.BoolValue(!getBool(arguments(0)), loc)
        case BuiltIn.IfThenElse =>
          if (getBool(arguments(0))) arguments(1)
          else arguments(2)
        case BuiltIn.First =>
          arguments(0)
        case BuiltIn.Min =>
          binIntOp(_ min _)
        case BuiltIn.Max =>
          binIntOp(_ max _)
        case BuiltIn.Pow =>
          binFloatOp(math.pow)
        case BuiltIn.Log =>
          binFloatOp((x, base) => math.log(x) / math.log(base))
        case BuiltIn.Sin =>
          TesslaCore.FloatValue(Math.sin(getFloat(arguments(0))), loc)
        case BuiltIn.Cos =>
          TesslaCore.FloatValue(Math.cos(getFloat(arguments(0))), loc)
        case BuiltIn.Tan =>
          TesslaCore.FloatValue(Math.tan(getFloat(arguments(0))), loc)
        case BuiltIn.Atan =>
          TesslaCore.FloatValue(Math.atan(getFloat(arguments(0))), loc)
        case BuiltIn.IntToFloat =>
          TesslaCore.FloatValue(getInt(arguments(0)).toDouble, loc)
        case BuiltIn.FloatToInt =>
          TesslaCore.IntValue(BigDecimal(getFloat(arguments(0))).toBigInt, loc)
        case BuiltIn.None =>
          TesslaCore.TesslaOption(None, loc)
        case BuiltIn.Some =>
          TesslaCore.TesslaOption(Some(arguments(0).forceValue), loc)
        case BuiltIn.IsNone =>
          TesslaCore.BoolValue(getOption(arguments(0)).value.isEmpty, loc)
        case BuiltIn.GetSome =>
          getOption(arguments(0)).value match {
            case None => throw CannotGetValueOfNone(loc)
            case Some(value) => value
          }
        case BuiltIn.MapEmpty =>
          TesslaCore.TesslaMap(Map(), loc)
        case BuiltIn.MapAdd =>
          val map = getMap(arguments(0))
          TesslaCore.TesslaMap(map.value + (arguments(1).forceValue -> arguments(2).forceValue), loc)
        case BuiltIn.MapGet =>
          val map = getMap(arguments(0))
          val key = arguments(1).forceValue
          try {
            map.value(key).withLoc(loc)
          } catch {
            case _: NoSuchElementException =>
              throw KeyNotFound(key.forceValue, map.value, loc)
          }
        case BuiltIn.MapContains =>
          TesslaCore.BoolValue(getMap(arguments(0)).value.contains(arguments(1).forceValue), loc)
        case BuiltIn.MapRemove =>
          val map = getMap(arguments(0))
          TesslaCore.TesslaMap(map.value - arguments(1).forceValue, loc)
        case BuiltIn.MapSize =>
          val map = getMap(arguments(0))
          TesslaCore.IntValue(map.value.size, loc)
        case BuiltIn.SetEmpty =>
          TesslaCore.TesslaSet(Set(), loc)
        case BuiltIn.SetAdd =>
          val set = getSet(arguments(0))
          TesslaCore.TesslaSet(set.value + arguments(1).forceValue, loc)
        case BuiltIn.SetContains =>
          TesslaCore.BoolValue(getSet(arguments(0)).value.contains(arguments(1).forceValue), loc)
        case BuiltIn.SetRemove =>
          val set = getSet(arguments(0))
          TesslaCore.TesslaSet(set.value - arguments(1).forceValue, loc)
        case BuiltIn.SetSize =>
          val set = getSet(arguments(0))
          TesslaCore.IntValue(set.value.size, loc)
        case BuiltIn.SetUnion =>
          val set1 = getSet(arguments(0))
          val set2 = getSet(arguments(1))
          TesslaCore.TesslaSet(set1.value | set2.value, loc)
        case BuiltIn.SetIntersection =>
          val set1 = getSet(arguments(0))
          val set2 = getSet(arguments(1))
          TesslaCore.TesslaSet(set1.value & set2.value, loc)
        case BuiltIn.SetFold =>
          val set = getSet(arguments(0)).value
          val z = arguments(1)
          val f = arguments(2)
          set.foldLeft(z)((acc, value) => evalApplication(f, Seq(acc, value), loc))
        case BuiltIn.ListEmpty =>
          TesslaCore.TesslaList(List(), loc)
        case BuiltIn.ListSize =>
          val list = getList(arguments(0))
          TesslaCore.IntValue(list.value.size, loc)
        case BuiltIn.ListAppend =>
          val list = getList(arguments(0))
          TesslaCore.TesslaList(list.value :+ arguments(1).forceValue, loc)
        case BuiltIn.ListPrepend =>
          val list = getList(arguments(0))
          TesslaCore.TesslaList(arguments(1).forceValue +: list.value, loc)
        case BuiltIn.ListTail =>
          val list = getList(arguments(0))
          if (list.value.isEmpty) {
            list
          } else {
            TesslaCore.TesslaList(list.value.tail, loc)
          }
        case BuiltIn.ListInit =>
          val list = getList(arguments(0))
          if (list.value.isEmpty) {
            list
          } else {
            TesslaCore.TesslaList(list.value.init, loc)
          }
        case BuiltIn.ListHead =>
          val list = getList(arguments(0))
          if (list.value.isEmpty) {
            throw HeadOfEmptyList(loc)
          } else {
            list.value.head.withLoc(loc)
          }
        case BuiltIn.ListLast =>
          val list = getList(arguments(0))
          if (list.value.isEmpty) {
            throw LastOfEmptyList(loc)
          } else {
            list.value.last.withLoc(loc)
          }
        case BuiltIn.ListFold =>
          val list = getList(arguments(0)).value
          val z = arguments(1)
          val f = arguments(2)
          list.foldLeft(z)((acc, value) => evalApplication(f, Seq(acc, value), loc))
        case BuiltIn.String_concat =>
          val s1 = getString(arguments(0))
          val s2 = getString(arguments(1))
          TesslaCore.StringValue(s1 + s2, loc)
        case BuiltIn.ToString =>
          TesslaCore.StringValue(evalToString(arguments(0)), loc)
        case BuiltIn.Format =>
          TesslaCore.StringValue(String.format(Locale.ROOT, getString(arguments(0)), getFormatArg(arguments(1))), loc)
        case BuiltIn.FormatInt =>
          TesslaCore.StringValue(String.format(Locale.ROOT, getString(arguments(0)), getFormatArg(arguments(1))), loc)
        case BuiltIn.FormatFloat =>
          TesslaCore.StringValue(String.format(Locale.ROOT, getString(arguments(0)), getFormatArg(arguments(1))), loc)
        case BuiltIn.CtfGetInt =>
          val composite = getCtf(arguments(0))
          val key = getString(arguments(1))
          TesslaCore.IntValue(Ctf.getInt(composite, key), loc)
        case BuiltIn.CtfGetString =>
          val composite = getCtf(arguments(0))
          val key = getString(arguments(1))
          TesslaCore.StringValue(Ctf.getString(composite, key), loc)
      }
    } catch {
      case e: TesslaError =>
        TesslaCore.Error(e)
    }
  }

  def getFormatArg(arg: TesslaCore.ValueOrError): AnyRef = arg.forceValue match {
    case i: TesslaCore.IntValue => i.value.bigInteger
    case p: TesslaCore.PrimitiveValue => p.value.asInstanceOf[AnyRef]
    case other => other
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
                      env: Env = Map()): TesslaCore.ValueOrError = {
    evalArg(f, env) match {
      case b: TesslaCore.BuiltInOperator =>
        evalPrimitiveOperator(b.value, args, loc)
      case c: TesslaCore.Closure =>
        val env = c.capturedEnvironment
        val argEnv = c.function.parameters.zip(args.map(arg => Lazy(arg))).toMap
        lazy val innerEnv: Env =  env ++ argEnv ++ c.function.body.map {
          case (id, e) => id -> Lazy(evalExpression(e, innerEnv))
        }
        evalArg(c.function.result, innerEnv)
      case _ =>
        throw InternalError("Application of non-function should have been caught by the type checker")
    }
  }

  def evalArg(arg: TesslaCore.ValueArg, env: Env): TesslaCore.ValueOrError = arg match {
    case value: TesslaCore.ValueOrError =>
      value
    case obj: TesslaCore.ObjectCreation =>
      try {
        TesslaCore.TesslaObject(mapValues(obj.members)(evalArg(_, env).forceValue), obj.loc)
      } catch {
        case e: TesslaError => TesslaCore.Error(e)
      }
    case ref: TesslaCore.ValueExpressionRef =>
      env(ref.id).get
  }

  def evalIfThenElse(cond: TesslaCore.ValueArg,
                     thenCase: Lazy[TesslaCore.ValueArg],
                     elseCase: Lazy[TesslaCore.ValueArg],
                     env: Env, loc: Location): TesslaCore.ValueOrError = {
    evalArg(cond, env).mapValue {cond =>
      if (getBool(cond)) {
        evalArg(thenCase.get, env)
      } else {
        evalArg(elseCase.get, env)
      }
    }
  }

  def evalExpression(exp: TesslaCore.ValueExpression, env: Env): TesslaCore.ValueOrError = exp match {
    case f: TesslaCore.Function =>
      TesslaCore.Closure(f, env, exp.loc)
    case ite: TesslaCore.IfThenElse =>
      evalIfThenElse(ite.cond, ite.thenCase, ite.elseCase, env, ite.loc)
    case a: TesslaCore.Application =>
      evalApplication(a.f.get, a.args.map(arg => evalArg(arg.get, env)), a.loc, env)
    case ma: TesslaCore.MemberAccess =>
      evalArg(ma.obj, env).mapValue {
        case obj: TesslaCore.TesslaObject =>
          obj.value(ma.member)
        case other =>
          throw InternalError(s"Member access on non-object ($other) should have been caught by type checker", ma.loc)
      }
  }
}
package de.uni_luebeck.isp.tessla

import java.util.Locale

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.Evaluator._
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
    case v => throw InternalError(s"Type error should've been caught by type checker: Expected: Float, got: $v", v.loc)
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

  def evalArg(arg: TesslaCore.ValueArg, env: Env): TesslaCore.ValueOrError = arg match {
    case value: TesslaCore.ValueOrError =>
      value
    case obj: TesslaCore.ObjectCreation =>
      TesslaCore.TesslaObject(mapValues(obj.members)(evalArg(_, env)), obj.loc)
    case ref: TesslaCore.ValueExpressionRef =>
      env(ref.id).get
  }

  def evalIfThenElse(cond: TesslaCore.ValueArg,
    thenCase: Lazy[TesslaCore.ValueArg],
    elseCase: Lazy[TesslaCore.ValueArg],
    env: Env, loc: Location
  ): TesslaCore.ValueOrError = {
    evalArg(cond, env).mapValue { cond =>
      if (getBool(cond)) {
        evalArg(thenCase.get, env)
      } else {
        evalArg(elseCase.get, env)
      }
    }
  }
}

class Evaluator(customBuiltIns: Map[String, Seq[TesslaCore.ValueOrError] => TesslaCore.ValueOrError]) {

  def evalExpression(desc: TesslaCore.ValueExpressionDescription, env: Env): TesslaCore.ValueOrError = desc.exp match {
    case f: TesslaCore.Function =>
      TesslaCore.Closure(f, env, f.loc)
    case ite: TesslaCore.IfThenElse =>
      evalIfThenElse(ite.cond, ite.thenCase, ite.elseCase, env, ite.loc)
    case a: TesslaCore.Application =>
      evalApplication(a.f.get, a.args.map(arg => evalArg(arg, env)), desc.typ, a.loc, env)
    case ma: TesslaCore.MemberAccess =>
      evalArg(ma.obj, env).mapValue {
        case obj: TesslaCore.TesslaObject =>
          obj.value(ma.member)
        case other =>
          throw InternalError(s"Member access on non-object ($other) should have been caught by type checker", ma.loc)
      }
  }

  def evalApplication(f: TesslaCore.ValueArg,
    args: Seq[TesslaCore.ValueOrError],
    resultType: TesslaCore.ValueType,
    loc: Location,
    env: Env = Map()
  ): TesslaCore.ValueOrError = {
    evalArg(f, env).forceValue match {
      case b: TesslaCore.BuiltInOperator =>
        evalPrimitiveOperator(b.value, args, resultType, loc)
      case c: TesslaCore.Closure =>
        val env = c.capturedEnvironment
        val argEnv = c.function.parameters.zip(args.map(arg => Lazy(arg))).toMap
        lazy val innerEnv: Env = env ++ argEnv ++ c.function.body.map {
          case (id, e) => id -> Lazy(evalExpression(e, innerEnv))
        }
        evalArg(c.function.result, innerEnv).withLoc(loc)
      case _ =>
        throw InternalError("Application of non-function should have been caught by the type checker")
    }
  }

  def evalPrimitiveOperator(name: String,
    arguments: Seq[TesslaCore.ValueOrError],
    resultType: TesslaCore.ValueType,
    loc: Location
  ): TesslaCore.ValueOrError = {
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
      name match {
        case "add" => binIntOp(_ + _)
        case "sub" => binIntOp(_ - _)
        case "mul" => binIntOp(_ * _)
        case "div" =>
          val x = getInt(arguments(0))
          val y = getInt(arguments(1))
          if (y == 0) TesslaCore.Error(DivideByZero(arguments(1).forceValue.loc))
          else TesslaCore.IntValue(x / y, loc)
        case "mod" =>
          val x = getInt(arguments(0))
          val y = getInt(arguments(1))
          if (y == 0) TesslaCore.Error(DivideByZero(arguments(1).forceValue.loc))
          else TesslaCore.IntValue(x % y, loc)
        case "fadd" => binFloatOp(_ + _)
        case "fsub" => binFloatOp(_ - _)
        case "fmul" => binFloatOp(_ * _)
        case "fdiv" => binFloatOp(_ / _)
        case "leftshift" => binIntOp(_ << _.toInt)
        case "rightshift" => binIntOp(_ >> _.toInt)
        case "bitand" => binIntOp(_ & _)
        case "bitor" => binIntOp(_ | _)
        case "bitxor" => binIntOp(_ ^ _)
        case "bitflip" => TesslaCore.IntValue(~getInt(arguments(0)), loc)
        case "negate" => TesslaCore.IntValue(-getInt(arguments(0)), loc)
        case "fnegate" => TesslaCore.FloatValue(-getFloat(arguments(0)), loc)
        case "eq" => TesslaCore.BoolValue(arguments(0).forceValue == arguments(1).forceValue, loc)
        case "neq" => TesslaCore.BoolValue(arguments(0).forceValue != arguments(1).forceValue, loc)
        case "lt" => binIntComp(_ < _)
        case "leq" => binIntComp(_ <= _)
        case "gt" => binIntComp(_ > _)
        case "geq" => binIntComp(_ >= _)
        case "flt" => binFloatComp(_ < _)
        case "fleq" => binFloatComp(_ <= _)
        case "fgt" => binFloatComp(_ > _)
        case "fgeq" => binFloatComp(_ >= _)
        case "and" => TesslaCore.BoolValue(getBool(arguments(0)) && getBool(arguments(1)), loc)
        case "or" => TesslaCore.BoolValue(getBool(arguments(0)) || getBool(arguments(1)), loc)
        case "not" => TesslaCore.BoolValue(!getBool(arguments(0)), loc)
        case "ite" =>
          if (getBool(arguments(0))) arguments(1)
          else arguments(2)
        case "pow" =>
          binFloatOp(math.pow)
        case "log" =>
          binFloatOp((x, base) => math.log(x) / math.log(base))
        case "sin" =>
          TesslaCore.FloatValue(Math.sin(getFloat(arguments(0))), loc)
        case "cos" =>
          TesslaCore.FloatValue(Math.cos(getFloat(arguments(0))), loc)
        case "tan" =>
          TesslaCore.FloatValue(Math.tan(getFloat(arguments(0))), loc)
        case "atan" =>
          TesslaCore.FloatValue(Math.atan(getFloat(arguments(0))), loc)
        case "intToFloat" =>
          TesslaCore.FloatValue(getInt(arguments(0)).toDouble, loc)
        case "floatToInt" =>
          val value = getFloat(arguments(0))
          try {
            TesslaCore.IntValue(BigDecimal(value).toBigInt, loc)
          } catch {
            case _: NumberFormatException =>
              TesslaCore.Error(FloatConversionError(value, loc))
          }
        case "None" =>
          TesslaCore.TesslaOption(None, resultType, loc)
        case "Some" =>
          TesslaCore.TesslaOption(Some(arguments(0)), resultType, loc)
        case "isNone" =>
          TesslaCore.BoolValue(getOption(arguments(0)).value.isEmpty, loc)
        case "getSome" =>
          getOption(arguments(0)).value match {
            case None => throw CannotGetValueOfNone(loc)
            case Some(value) => value.withLoc(loc)
          }
        case "Map_empty" =>
          TesslaCore.TesslaMap(Map(), resultType, loc)
        case "Map_add" =>
          val map = getMap(arguments(0))
          TesslaCore.TesslaMap(map.value + (arguments(1).forceValue -> arguments(2).forceValue), resultType, loc)
        case "Map_get" =>
          val map = getMap(arguments(0))
          val key = arguments(1).forceValue
          try {
            map.value(key).withLoc(loc)
          } catch {
            case _: NoSuchElementException =>
              throw KeyNotFound(key.forceValue, map.value, loc)
          }
        case "Map_contains" =>
          TesslaCore.BoolValue(getMap(arguments(0)).value.contains(arguments(1).forceValue), loc)
        case "Map_remove" =>
          val map = getMap(arguments(0))
          TesslaCore.TesslaMap(map.value - arguments(1).forceValue, resultType, loc)
        case "Map_size" =>
          val map = getMap(arguments(0))
          TesslaCore.IntValue(map.value.size, loc)
        case "Map_keys" =>
          val map = getMap(arguments(0))
          TesslaCore.TesslaList(map.value.keys.toIndexedSeq, resultType, loc)
        case "Map_fold" =>
          val map = getMap(arguments(0)).value
          val z = arguments(1)
          val f = arguments(2)
          map.foldLeft(z){case (acc, (key, value)) => evalApplication(f, Seq(acc, key, value), resultType, loc)}
        case "Set_empty" =>
          TesslaCore.TesslaSet(Set(), resultType, loc)
        case "Set_add" =>
          val set = getSet(arguments(0))
          TesslaCore.TesslaSet(set.value + arguments(1).forceValue, resultType, loc)
        case "Set_contains" =>
          TesslaCore.BoolValue(getSet(arguments(0)).value.contains(arguments(1).forceValue), loc)
        case "Set_remove" =>
          val set = getSet(arguments(0))
          TesslaCore.TesslaSet(set.value - arguments(1).forceValue, resultType, loc)
        case "Set_size" =>
          val set = getSet(arguments(0))
          TesslaCore.IntValue(set.value.size, loc)
        case "Set_union" =>
          val set1 = getSet(arguments(0))
          val set2 = getSet(arguments(1))
          TesslaCore.TesslaSet(set1.value | set2.value, resultType, loc)
        case "Set_intersection" =>
          val set1 = getSet(arguments(0))
          val set2 = getSet(arguments(1))
          TesslaCore.TesslaSet(set1.value & set2.value, resultType, loc)
        case "Set_minus" =>
          val set1 = getSet(arguments(0))
          val set2 = getSet(arguments(1))
          TesslaCore.TesslaSet(set1.value -- set2.value, resultType, loc)
        case "Set_fold" =>
          val set = getSet(arguments(0)).value
          val z = arguments(1)
          val f = arguments(2)
          set.foldLeft(z)((acc, value) => evalApplication(f, Seq(acc, value), resultType, loc))
        case "List_empty" =>
          TesslaCore.TesslaList(Vector(), resultType, loc)
        case "List_size" =>
          val list = getList(arguments(0))
          TesslaCore.IntValue(list.value.size, loc)
        case "List_append" =>
          val list = getList(arguments(0))
          TesslaCore.TesslaList(list.value :+ arguments(1).forceValue, resultType, loc)
        case "List_prepend" =>
          val list = getList(arguments(1))
          TesslaCore.TesslaList(arguments(0).forceValue +: list.value, resultType, loc)
        case "List_tail" =>
          val list = getList(arguments(0))
          if (list.value.isEmpty) {
            list
          } else {
            TesslaCore.TesslaList(list.value.tail, resultType, loc)
          }
        case "List_init" =>
          val list = getList(arguments(0))
          if (list.value.isEmpty) {
            list
          } else {
            TesslaCore.TesslaList(list.value.init, resultType, loc)
          }
        case "List_fold" =>
          val list = getList(arguments(0)).value
          val z = arguments(1)
          val f = arguments(2)
          list.foldLeft(z)((acc, value) => evalApplication(f, Seq(acc, value), resultType, loc))
        case "List_get" =>
          val list = getList(arguments(0))
          val index = getInt(arguments(1)).toInt
          try {
            list.value(index).withLoc(loc)
          } catch {
            case _: IndexOutOfBoundsException =>
              throw IndexOutOfRange(index, list.value, loc)
          }
        case "List_set" =>
          val list = getList(arguments(0))
          val index = getInt(arguments(1)).toInt
          val value = arguments(2)
          try {
            TesslaCore.TesslaList(list.value.updated(index, value.forceValue), list.typ, loc)
          } catch {
            case _: IndexOutOfBoundsException =>
              throw IndexOutOfRange(index, list.value, loc)
          }
        case "String_concat" =>
          val s1 = getString(arguments(0))
          val s2 = getString(arguments(1))
          TesslaCore.StringValue(s1 + s2, loc)
        case "toString" =>
          TesslaCore.StringValue(evalToString(arguments(0)), loc)
        case "String_format" =>
          TesslaCore.StringValue(String.format(Locale.ROOT, getString(arguments(0)), getFormatArg(arguments(1))), loc)
        case "CTF_getInt" =>
          val composite = getCtf(arguments(0))
          val key = getString(arguments(1))
          TesslaCore.IntValue(Ctf.getInt(composite, key), loc)
        case "CTF_getString" =>
          val composite = getCtf(arguments(0))
          val key = getString(arguments(1))
          TesslaCore.StringValue(Ctf.getString(composite, key), loc)
        case other =>
          customBuiltIns.get(other) match {
            case Some(f) => f(arguments)
            case None => throw InternalError(s"Unknown built-in in constant folder: $other", loc)
          }
      }
    } catch {
      case e: TesslaError if !e.isInstanceOf[InternalError] =>
        TesslaCore.Error(e)
    }
  }
}


package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{CannotGetValueOfNone, DivideByZero, InternalError, KeyNotFound}
import de.uni_luebeck.isp.tessla.util.Lazy
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition

object Evaluator {
  def getInt(v: TesslaCore.Value): BigInt = v match {
    case intLit: TesslaCore.IntValue => intLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Int, got: $v", v.loc)
  }

  def getBool(v: TesslaCore.Value): Boolean = v match {
    case boolLit: TesslaCore.BoolValue => boolLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Bool, got: $v", v.loc)
  }

  def getString(v: TesslaCore.Value): String = v match {
    case stringLit: TesslaCore.StringValue => stringLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: String, got: $v", v.loc)
  }

  def getOption(v: TesslaCore.Value): TesslaCore.TesslaOption = v match {
    case optionValue: TesslaCore.TesslaOption => optionValue
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Option ${v.loc}, got: $v", v.loc)
  }

  def getCtf(v: TesslaCore.Value): ICompositeDefinition = v match {
    case ctfLit: TesslaCore.Ctf => ctfLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: CTF, got: $v", v.loc)
  }

  def getMap(v: TesslaCore.Value): TesslaCore.TesslaMap = v match {
    case mapLit: TesslaCore.TesslaMap => mapLit
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Map, got: $v", v.loc)
  }

  def getSet(v: TesslaCore.Value): TesslaCore.TesslaSet = v match {
    case setLit: TesslaCore.TesslaSet => setLit
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Set, got: $v", v.loc)
  }

  def evalPrimitiveOperator(op: BuiltIn.PrimitiveOperator,
                            arguments: Seq[Lazy[TesslaCore.Value]],
                            loc: Location): Lazy[Option[TesslaCore.Value]] = Lazy {
    def binIntOp(op: (BigInt, BigInt) => BigInt) = {
      Some(TesslaCore.IntValue(op(getInt(arguments(0).get), getInt(arguments(1).get)), loc))
    }

    def binIntComp(op: (BigInt, BigInt) => Boolean) = {
      Some(TesslaCore.BoolValue(op(getInt(arguments(0).get), getInt(arguments(1).get)), loc))
    }

    def div(x: BigInt, y: BigInt): BigInt = {
      // This is a bit dirty because we hard-code the fact that y corresponds to arguments(1),
      // but since this is only a local function, it should be fine.
      if (y == 0) throw DivideByZero(arguments(1).get.loc)
      else x/y
    }

    op match {
      case BuiltIn.Add => binIntOp(_ + _)
      case BuiltIn.Sub => binIntOp(_ - _)
      case BuiltIn.Mul => binIntOp(_ * _)
      case BuiltIn.Div => binIntOp(div)
      case BuiltIn.LeftShift => binIntOp(_ << _.toInt)
      case BuiltIn.RightShift => binIntOp(_ >> _.toInt)
      case BuiltIn.BitAnd => binIntOp(_ & _)
      case BuiltIn.BitOr => binIntOp(_ | _)
      case BuiltIn.BitXor => binIntOp(_ ^ _)
      case BuiltIn.BitFlip => Some(TesslaCore.IntValue(~getInt(arguments(0).get), loc))
      case BuiltIn.Negate => Some(TesslaCore.IntValue(-getInt(arguments(0).get), loc))
      case BuiltIn.Eq => Some(TesslaCore.BoolValue(arguments(0).get == arguments(1).get, loc))
      case BuiltIn.Neq => Some(TesslaCore.BoolValue(arguments(0).get != arguments(1).get, loc))
      case BuiltIn.Lt => binIntComp(_ < _)
      case BuiltIn.Lte => binIntComp(_ <= _)
      case BuiltIn.Gt => binIntComp(_ > _)
      case BuiltIn.Gte => binIntComp(_ >= _)
      case BuiltIn.And => Some(TesslaCore.BoolValue(getBool(arguments(0).get) && getBool(arguments(1).get), loc))
      case BuiltIn.Or => Some(TesslaCore.BoolValue(getBool(arguments(0).get) || getBool(arguments(1).get), loc))
      case BuiltIn.Not => Some(TesslaCore.BoolValue(!getBool(arguments(0).get), loc))
      case BuiltIn.IfThen =>
        if (getBool(arguments(0).get)) Some(arguments(1).get)
        else None
      case BuiltIn.IfThenElse =>
        if (getBool(arguments(0).get)) Some(arguments(1).get)
        else Some(arguments(2).get)
      case BuiltIn.First =>
        Some(arguments(0).get)
      case BuiltIn.None =>
        Some(TesslaCore.TesslaOption(None, loc))
      case BuiltIn.Some =>
        Some(TesslaCore.TesslaOption(Some(arguments(0).get), loc))
      case BuiltIn.IsNone =>
        Some(TesslaCore.BoolValue(getOption(arguments(0).get).value.isEmpty, loc))
      case BuiltIn.GetSome =>
        getOption(arguments(0).get).value match {
          case None => throw CannotGetValueOfNone(loc)
          case some => some
        }
      case BuiltIn.MapEmpty =>
        Some(TesslaCore.TesslaMap(Map(), loc))
      case BuiltIn.MapAdd =>
        val map = getMap(arguments(0).get)
        Some(TesslaCore.TesslaMap(map.value + (arguments(1).get -> arguments(2).get), loc))
      case BuiltIn.MapGet =>
        val map = getMap(arguments(0).get)
        val key = arguments(1).get
        try {
          Some(map.value(key).withLoc(loc))
        } catch {
          case _: NoSuchElementException =>
            throw KeyNotFound(key, map.value, loc)
        }
      case BuiltIn.MapContains =>
        Some(TesslaCore.BoolValue(getMap(arguments(0).get).value.contains(arguments(1).get), loc))
      case BuiltIn.MapRemove =>
        val map = getMap(arguments(0).get)
        Some(TesslaCore.TesslaMap(map.value - arguments(1).get, loc))
      case BuiltIn.MapSize =>
        val map = getMap(arguments(0).get)
        Some(TesslaCore.IntValue(map.value.size, loc))
      case BuiltIn.SetEmpty =>
        Some(TesslaCore.TesslaSet(Set(), loc))
      case BuiltIn.SetAdd =>
        val set = getSet(arguments(0).get)
        Some(TesslaCore.TesslaSet(set.value + arguments(1).get, loc))
      case BuiltIn.SetContains =>
        Some(TesslaCore.BoolValue(getSet(arguments(0).get).value.contains(arguments(1).get), loc))
      case BuiltIn.SetRemove =>
        val set = getSet(arguments(0).get)
        Some(TesslaCore.TesslaSet(set.value - arguments(1).get, loc))
      case BuiltIn.SetSize =>
        val set = getSet(arguments(0).get)
        Some(TesslaCore.IntValue(set.value.size, loc))
      case BuiltIn.SetUnion =>
        val set1 = getSet(arguments(0).get)
        val set2 = getSet(arguments(1).get)
        Some(TesslaCore.TesslaSet(set1.value | set2.value, loc))
      case BuiltIn.SetIntersection =>
        val set1 = getSet(arguments(0).get)
        val set2 = getSet(arguments(1).get)
        Some(TesslaCore.TesslaSet(set1.value & set2.value, loc))
      case BuiltIn.CtfGetInt =>
        val composite = getCtf(arguments(0).get)
        val key = getString(arguments(1).get)
        Some(TesslaCore.IntValue(Ctf.getInt(composite, key), loc))
      case BuiltIn.CtfGetString =>
        val composite = getCtf(arguments(0).get)
        val key = getString(arguments(1).get)
        Some(TesslaCore.StringValue(Ctf.getString(composite, key), loc))
    }
  }
}
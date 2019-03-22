package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.{InputTypeMismatch, InternalError}
import de.uni_luebeck.isp.tessla.TesslaCore

object ValueTypeChecker {
  def check(value: TesslaCore.Value,
            elementType: TesslaCore.ValueType,
            name: String): Unit = value match {
    case _: TesslaCore.IntValue =>
      if (elementType != TesslaCore.IntType) {
        throw InputTypeMismatch(value, "Int", name, elementType, value.loc)
      }
    case _: TesslaCore.FloatValue =>
      if (elementType != TesslaCore.FloatType) {
        throw InputTypeMismatch(value, "Float", name, elementType, value.loc)
      }
    case _: TesslaCore.StringValue =>
      if (elementType != TesslaCore.StringType) {
        throw InputTypeMismatch(value, "String", name, elementType, value.loc)
      }
    case _: TesslaCore.BoolValue =>
      if (elementType != TesslaCore.BoolType) {
        throw InputTypeMismatch(value, "Bool", name, elementType, value.loc)
      }
    case o: TesslaCore.TesslaOption =>
      elementType match {
        case ot: TesslaCore.OptionType =>
          o.value.foreach(check(_, ot.elementType, name))
        case _ =>
          throw InputTypeMismatch(value, "Option[?]", name, elementType, value.loc)
      }
    case s: TesslaCore.TesslaSet =>
      elementType match {
        case st: TesslaCore.SetType =>
          s.value.foreach(check(_, st.elementType, name))
        case _ =>
          throw InputTypeMismatch(value, "Set[?]", name, elementType, value.loc)
      }
    case m: TesslaCore.TesslaMap =>
      elementType match {
        case mt: TesslaCore.MapType =>
          m.value.foreach {
            case (k, v) =>
              check(k, mt.keyType, name)
              check(v, mt.valueType, name)
          }
        case _ =>
          throw InputTypeMismatch(value, "Map[?, ?]", name, elementType, value.loc)
      }
    case s: TesslaCore.TesslaList =>
      elementType match {
        case st: TesslaCore.ListType =>
          s.value.foreach(check(_, st.elementType, name))
        case _ =>
          throw InputTypeMismatch(value, "List[?]", name, elementType, value.loc)
      }
    case o: TesslaCore.TesslaObject =>
      val actual = o.value.keys.map {n => s"$n: ?"}.mkString("{", ", ", "}")
      elementType match {
        case ot: TesslaCore.ObjectType =>
          if (ot.memberTypes.keySet != o.value.keySet) {
            throw InputTypeMismatch(value, actual, name, elementType, value.loc)
          }
          o.value.foreach {
            case (n, v) => check(v, ot.memberTypes(n), s"$name.$n")
          }
        case _ =>
          throw InputTypeMismatch(value, actual, name, elementType, value.loc)
      }
    case _: TesslaCore.Closure | _: TesslaCore.BuiltInOperator =>
      throw InternalError("Functions should not currently be able to appear in input streams")
    case _: TesslaCore.Ctf =>
      if (elementType != TesslaCore.CtfType) {
        throw InputTypeMismatch(value, "CTF", name, elementType, value.loc)
      }
  }
}
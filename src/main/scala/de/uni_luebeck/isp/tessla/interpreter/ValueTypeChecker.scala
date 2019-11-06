package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.{InputTypeMismatch, InternalError}
import de.uni_luebeck.isp.tessla.TesslaCore

object ValueTypeChecker {
  def isType(typ: TesslaCore.ValueType, name: String) = typ match {
    case b: TesslaCore.BuiltInType => b.name == name
    case _ => false
  }

  def check(value: TesslaCore.Value,
            elementType: TesslaCore.ValueType,
            name: String): Unit = value match {
    case _: TesslaCore.IntValue =>
      if (!isType(elementType, "Int")) {
        throw InputTypeMismatch(value, "Int", name, elementType, value.loc)
      }
    case _: TesslaCore.FloatValue =>
      if (!isType(elementType, "Float")) {
        throw InputTypeMismatch(value, "Float", name, elementType, value.loc)
      }
    case _: TesslaCore.StringValue =>
      if (!isType(elementType, "String")) {
        throw InputTypeMismatch(value, "String", name, elementType, value.loc)
      }
    case _: TesslaCore.BoolValue =>
      if (!isType(elementType, "Bool")) {
        throw InputTypeMismatch(value, "Bool", name, elementType, value.loc)
      }
    case o: TesslaCore.TesslaOption =>
      elementType match {
        case ot: TesslaCore.BuiltInType if ot.name == "Option" =>
          o.value.foreach(v => check(v.forceValue, ot.typeArgs.head, name))
        case _ =>
          throw InputTypeMismatch(value, "Option[?]", name, elementType, value.loc)
      }
    case s: TesslaCore.TesslaSet =>
      elementType match {
        case st: TesslaCore.BuiltInType if st.name == "Set" =>
          s.value.foreach(check(_, st.typeArgs.head, name))
        case _ =>
          throw InputTypeMismatch(value, "Set[?]", name, elementType, value.loc)
      }
    case m: TesslaCore.TesslaMap =>
      elementType match {
        case mt: TesslaCore.BuiltInType if mt.name == "Map" =>
          m.value.foreach {
            case (k, v) =>
              check(k, mt.typeArgs.head, name)
              check(v, mt.typeArgs(1), name)
          }
        case _ =>
          throw InputTypeMismatch(value, "Map[?, ?]", name, elementType, value.loc)
      }
    case l: TesslaCore.TesslaList =>
      elementType match {
        case lt: TesslaCore.BuiltInType if lt.name == "List" =>
          l.value.foreach(check(_, lt.typeArgs.head, name))
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
            case (n, v) => check(v.forceValue, ot.memberTypes(n), s"$name.$n")
          }
        case _ =>
          throw InputTypeMismatch(value, actual, name, elementType, value.loc)
      }
    case _: TesslaCore.Closure | _: TesslaCore.BuiltInOperator =>
      throw InternalError("Functions should not currently be able to appear in input streams")
    case _: TesslaCore.ExternalValue =>
      throw InternalError("Values of externally defined types should not currently be able to appear in input streams")
    case _: TesslaCore.Ctf =>
      if (!isType(elementType, "CTF")) {
        throw InputTypeMismatch(value, "CTF", name, elementType, value.loc)
      }
  }
}
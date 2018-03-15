package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{StreamOfStreams, TypeMismatch, UnknownType}

object Types {
  sealed abstract class Type
  sealed abstract class ValueType extends Type
  case object Int extends ValueType
  case object String extends ValueType
  case object Bool extends ValueType
  case object Unit extends ValueType
  case object Nothing extends ValueType {
    override def toString = "?"
  }
  final case class Stream(elementType: ValueType) extends Type {
    override def toString = s"Events[$elementType]"
  }

  def fromAst(ast: Tessla.Type): Type = ast match {
    case Tessla.SimpleType(Tessla.Identifier("Int", _)) => Int
    case Tessla.SimpleType(Tessla.Identifier("String", _)) => String
    case Tessla.SimpleType(Tessla.Identifier("Bool", _)) => Bool
    case Tessla.SimpleType(Tessla.Identifier("Unit", _)) => Unit
    case Tessla.SimpleType(Tessla.Identifier(name, loc)) => throw UnknownType(name, loc)
    case Tessla.GenericType(Tessla.Identifier("Events", _), Seq(elementType), loc) =>
      fromAst(elementType) match {
        case Stream(_) => throw StreamOfStreams(loc)
        case t: ValueType => Stream(t)
      }
    case Tessla.GenericType(name, _, _) => throw UnknownType(name.name, name.loc)
  }

  private def requireTypeOption[T <: Type](expected: T, actual: T): Option[T] = (expected, actual) match {
    case (Nothing, _: ValueType) => Some(actual)
    case (_: ValueType, Nothing) => Some(expected)
    case (Stream(t1), Stream(t2)) =>
      requireTypeOption(t1, t2).map(Stream).asInstanceOf[Option[T]]
    case _ =>
      if (expected == actual) Some(actual)
      else None
  }

  def requireType[T <: Type](expected: T, actual: T, loc: Location): T = {
    requireTypeOption(expected, actual).getOrElse {
      throw TypeMismatch(expected, actual, loc)
    }
  }

  abstract class CustomType extends ValueType
}

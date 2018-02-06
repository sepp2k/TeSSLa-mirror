package de.uni_luebeck.isp.tessla

object TypedTessla extends FlatTessla {
  override type TypeAnnotation = Type

  override def typeAnnotationToString(typ: Type) = s" : $typ"

  override type Identifier = FlatTessla.Identifier

  sealed abstract class Type

  case object IntType extends Type

  case object TimeType extends Type

  case object StringType extends Type

  case object BoolType extends Type

  case object UnitType extends Type

  case class StreamType(elementType: Type) extends Type

  case class FunctionType(parameterTypes: Seq[Type], returnType: Seq[Type]) extends Type
}

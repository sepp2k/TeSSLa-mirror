package de.uni_luebeck.isp.tessla

object TypedTessla extends FlatTessla {
  override type TypeAnnotation = Type

  override def typeAnnotationToString(typ: Type) = s" : $typ"

  override type Identifier = FlatTessla.Identifier

  sealed abstract class Type
  case object UnknownType extends Type
}

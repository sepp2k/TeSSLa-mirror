package de.uni_luebeck.isp.tessla

object TypedTessla extends FlatTessla {
  type Type = FlatTessla.Type

  override type TypeAnnotation = Type

  override def typeAnnotationToString(typ: Type) = s" : $typ"

  override type Identifier = FlatTessla.Identifier
}

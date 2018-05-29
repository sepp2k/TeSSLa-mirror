package de.uni_luebeck.isp.tessla

object TypedTessla extends FlatTessla {
  override type TypeAnnotation = Type

  override def typeAnnotationToString(typ: Type) = s" : $typ"
}

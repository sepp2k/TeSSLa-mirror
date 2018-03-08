package de.uni_luebeck.isp.tessla

object TypedTessla extends FlatTessla {
  override type TypeAnnotation = Type

  override def typeAnnotationToString(typ: Type) = s" : $typ"

  override type Identifier = FlatTessla.Identifier

  sealed abstract class Type {
    def isValueType: Boolean
  }

  case object IntType extends Type {
    override def isValueType = true
  }

  case object TimeSpanType extends Type {
    override def isValueType = true
  }

  case object StringType extends Type {
    override def isValueType = true
  }

  case object BoolType extends Type {
    override def isValueType = true
  }

  case object UnitType extends Type {
    override def isValueType = true
  }

  case class StreamType(elementType: Type) extends Type {
    override def isValueType = false
  }

  case class FunctionType(typeParameters: Seq[Identifier], parameterTypes: Seq[Type], returnType: Type) extends Type {
    override def isValueType = false
  }
}

package de.uni_luebeck.isp.tessla

object TypedTessla extends FlatTessla {
  case class TypedSpecification(globalDefs: Definitions, outStreams: Seq[OutStream], outAllLocation: Option[Location]) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"$globalDefs\n${outStreams.mkString("\n")}$outAllString"
    }

    def outAll = outAllLocation.isDefined
  }

  override type TypeAnnotation = Type

  override def typeAnnotationToString(typ: Type) = s" : $typ"
}

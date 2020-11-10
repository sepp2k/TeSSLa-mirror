/*

 */

package de.uni_luebeck.isp.tessla.core

/**
 * An extension of the flattened Tessla AST, with all expressions annotated with their respective type.
 */
object TypedTessla extends FlatTessla {

  case class TypedSpecification(
    annotations: Seq[Annotation],
    globalDefs: Definitions,
    outStreams: Seq[OutStream],
    outAllLocation: Option[Location]
  ) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"${annotations.mkString("\n")}\n$globalDefs\n${outStreams.mkString("\n")}$outAllString"
    }

    def outAll = outAllLocation.isDefined
  }

  override type TypeAnnotation = Type

  override def typeAnnotationToString(typ: Type) = s" : $typ"
}

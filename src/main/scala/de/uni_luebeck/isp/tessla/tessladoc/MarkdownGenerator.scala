package de.uni_luebeck.isp.tessla.tessladoc

class MarkdownGenerator(docs: TesslaDoc.Docs) {
  def generateMarkdown: String =
    docs.items.map(itemToMarkdown).mkString("\n\n\n")

  def seqToString(seq: Seq[String], opening: String, closing: String) = {
    if (seq.isEmpty) ""
    else seq.mkString(opening, ", ", closing)
  }

  def toAnchor(str: String) = str.replaceAll("[^a-zA-Z]", "").toLowerCase

  def typeToMarkdown(typ: String): String = {
    val hasDoc = docs.items.exists {
      case td: TesslaDoc.TypeDoc => td.name == typ
      case _ => false
    }
    if (hasDoc) {
      s"""<a href="#${toAnchor(typ)}">${htmlEscape(typ)}</a>"""
    } else {
      htmlEscape(typ)
    }
  }

  def typeParamsToMarkdown(typeParams: Seq[String]) = {
    seqToString(typeParams.map(p => s"""$p"""), "[", "]")
  }

  def parametersToMarkdown(params: Seq[TesslaDoc.Param]): String = {
    seqToString(params.map(parameterToMarkdown), "(", ")")
  }

  def parameterToMarkdown(param: TesslaDoc.Param): String = {
    s"""${htmlEscape(param.name)}: ${typeToMarkdown(param.typ.toString)}"""
  }

  def annotationToMarkdown(annotation: String) = {
    val hasDoc = docs.items.exists {
      case ad: TesslaDoc.AnnotationDoc => ad.name == annotation
      case _ => false
    }
    if (hasDoc) {
      s"""<a href="#${toAnchor(annotation)}">@${htmlEscape(annotation)}</a>"""
    } else {
      "@" + htmlEscape(annotation)
    }
  }

  def annotationsToMarkdown(annotations: Seq[String]) = {
    annotations.map(annotationToMarkdown).map(_ + "<br>\n").mkString
  }

  def markdownEscape(s: String) = s.replaceAll("_", "\\\\_").replaceAll("\\*", "\\\\*")

  def htmlEscape(s: String) = {
    val substitutions = Map(
      '<' -> "&lt;",
      '>' -> "&gt;",
      '&' -> "&amp;"
    )
    s.flatMap(c => substitutions.getOrElse(c, c.toString))
  }

  def itemToMarkdown(doc: TesslaDoc): String = doc match {
    case typ: TesslaDoc.TypeDoc =>
      val typeParams = typeParamsToMarkdown(typ.typeParameters)
      s"""## <a id="${toAnchor(typ.name)}">ANCHOR</a>Type ${markdownEscape(typ.name)}
         |{: .mt-5.anchor}
         |
         |`${typ.name}$typeParams`
         |
         |
         |${typ.doc}
         |""".stripMargin
    case annotation: TesslaDoc.AnnotationDoc =>
      val params = parametersToMarkdown(annotation.parameters)
      s"""## <a id="${toAnchor(annotation.name)}">ANCHOR</a>Annotation @${markdownEscape(annotation.name)}
         |{: .mt-5.anchor}
         |
         |<code>@${htmlEscape(annotation.name)}$params</code>
         |
         |${annotation.doc}
         |""".stripMargin
    case definition: TesslaDoc.DefDoc =>
      val typeParams = typeParamsToMarkdown(definition.typeParameters)
      val params = parametersToMarkdown(definition.parameters)
      val annotations = annotationsToMarkdown(definition.annotations)
      val returnType = definition.returnType match {
        case Some(typ) => ": " + typeToMarkdown(typ.toString)
        case None => ""
      }
      s"""##  ${markdownEscape(definition.name)}
         |{: .mt-5}
         |
         |<code>$annotations${htmlEscape(definition.name)}${htmlEscape(typeParams)}$params${returnType}</code>
         |
         |${definition.doc}
         |""".stripMargin
  }
}

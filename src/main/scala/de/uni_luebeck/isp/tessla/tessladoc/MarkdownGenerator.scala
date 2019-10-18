package de.uni_luebeck.isp.tessla.tessladoc

class MarkdownGenerator(docs: TesslaDoc.Docs) {
  def generateMarkdown: String =
    itemsToMarkdown(docs.items)

  def itemsToMarkdown(items: Seq[TesslaDoc]) = items.map(itemToMarkdown).mkString("\n\n\n")

  def seqToString(seq: Seq[String], opening: String, closing: String) = {
    if (seq.isEmpty) ""
    else seq.mkString(opening, ", ", closing)
  }

  def toAnchor(str: String) = str.replaceAll("[^a-zA-Z]", "").toLowerCase

  def typeToMarkdown(typ: TesslaDoc.Type): String = typ match {
    case t: TesslaDoc.SimpleType =>
      val hasDoc = docs.items.exists {
        case td: TesslaDoc.TypeDoc => td.name == t.name
        case _ => false
      }
      if (hasDoc) {
        s"""<a href="#${toAnchor(t.name)}">${htmlEscape(t.name)}</a>"""
      } else {
        htmlEscape(t.name)
      }
    case t: TesslaDoc.TypeApplication =>
      s"${typeToMarkdown(t.constructor)}${t.arguments.map(typeToMarkdown).mkString("[", ", ", "]")}"
    case t: TesslaDoc.FunctionType =>
      s"(${t.parameters.map(typeToMarkdown).mkString("(", ", ", ")")} => ${typeToMarkdown(t.result)}"
    case t: TesslaDoc.ObjectType =>
      t.members.map{case (name, typ) => s"$name: ${typeToMarkdown(typ)}"}.mkString("{", ", ", "}")
    case t: TesslaDoc.TupleType =>
      t.members.map(typeToMarkdown).mkString("(", ", ", ")")
  }

  def typeParamsToMarkdown(typeParams: Seq[String]) = {
    seqToString(typeParams.map(p => s"""$p"""), "[", "]")
  }

  def parametersToMarkdown(params: Seq[TesslaDoc.Param]): String = {
    seqToString(params.map(parameterToMarkdown), "(", ")")
  }

  def parameterToMarkdown(param: TesslaDoc.Param): String = {
    s"""${htmlEscape(param.name)}: ${typeToMarkdown(param.typ)}"""
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
      s"""<h2 class="mt-5 anchor"><a id="${toAnchor(typ.name)}">ANCHOR</a>Type ${htmlEscape(typ.name)}</h2>
         |
         |`${typ.name}$typeParams`
         |
         |
         |${typ.doc}
         |""".stripMargin
    case annotation: TesslaDoc.AnnotationDoc =>
      val params = parametersToMarkdown(annotation.parameters)
      s"""<h2 class="mt-5 anchor"><a id="${toAnchor(annotation.name)}">ANCHOR</a>Annotation @${htmlEscape(annotation.name)}</h2>
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
        case Some(typ) => ": " + typeToMarkdown(typ)
        case None => ""
      }
      s"""<h2 class="mt-5 anchor"><a id="${toAnchor(definition.name)}">ANCHOR</a>${htmlEscape(definition.name)}</h2>
         |
         |<code>$annotations${htmlEscape(definition.name)}${htmlEscape(typeParams)}$params${returnType}</code>
         |
         |${definition.doc}
         |""".stripMargin
    case module: TesslaDoc.ModuleDoc =>
      s"""## Module ${markdownEscape(module.name)}
         |{: .mt-5}
         |
         |${module.doc}
         |
         |<div class="module" id="module-${module.name}">
         |${itemsToMarkdown(module.members)}
         |</div>
         |""".stripMargin
  }
}

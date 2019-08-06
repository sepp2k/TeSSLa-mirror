package de.uni_luebeck.isp.tessla.tessladoc

import com.github.rjeschke.txtmark

object HtmlGenerator {
  def generateHTML(doc: TesslaDoc): String = {
    s"""<!DOCTYPE html>
       |<html>
       |  <head>
       |    <title>TeSSLa Documentation</title>
       |    <style>
       |      .doc-entry {
       |        border: 1px solid black;
       |        margin-bottom: 0.5em;
       |        padding: 3px;
       |      }
       |
       |      .parameter-type, .type-parameter {
       |        font-style: italic;
       |      }
       |
       |      .name {
       |        font-weight: bold;
       |      }
       |
       |      header {
       |        font-size: 110%;
       |      }
       |    </style>
       |  </head>
       |  <body>
       |    ${itemToHtml(doc)}
       |  </body>
       |</html>
       |""".stripMargin
  }

  def seqToString(seq: Seq[String], opening: String, closing: String) = {
    if (seq.isEmpty) ""
    else seq.mkString(opening, ", ", closing)
  }

  def typeParamsToHtml(typeParams: Seq[String]) = {
    seqToString(typeParams.map(p => s"""<span class="type-parameter">$p</span>"""), "[", "]")
  }

  def parametersToHtml(params: Seq[TesslaDoc.Param]): String = {
    seqToString(params.map(parameterToHtml), "(", ")")
  }

  def parameterToHtml(param: TesslaDoc.Param): String = {
    s"""<span class="parameter-name">${param.name}</span>: <span class="parameter-type">${param.typ}</span>"""
  }

  def itemToHtml(doc: TesslaDoc): String = doc match {
    case module: TesslaDoc.ModuleDoc =>
      module.items.map(itemToHtml).mkString
    case typ: TesslaDoc.TypeDoc =>
      val typeParams = typeParamsToHtml(typ.typeParameters)
      s"""    <div class="doc-entry type" id="doc-typ-${typ.name}">
         |      <header><span class="type-name name">${typ.name}</span>$typeParams</header>
         |      ${txtmark.Processor.process(typ.doc)}
         |    </div>
         |""".stripMargin
    case definition: TesslaDoc.DefDoc =>
      val typeParams = typeParamsToHtml(definition.typeParameters)
      val params = parametersToHtml(definition.parameters)
      s"""    <div class="doc-entry def" id="doc-def-${definition.name}">
         |      <header><span class="def-name name">${definition.name}</span>$typeParams$params</header>
         |      ${txtmark.Processor.process(definition.doc)}
         |    </div>
         |""".stripMargin
  }
}

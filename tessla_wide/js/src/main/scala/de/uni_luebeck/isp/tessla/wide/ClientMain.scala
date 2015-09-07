package de.uni_luebeck.isp.tessla.wide

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.jquery.jQuery
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.dom
import org.scalajs.dom.raw.BeforeUnloadEvent
import de.uni_luebeck.isp.tessla.Parser._
import de.uni_luebeck.isp.tessla.MacroResolution._
import de.uni_luebeck.isp.tessla.ASTText._
import scala.io.Source
import codemirror._

object ClientMain extends JSApp {
  var tesslaEditor: codemirror.CodeMirror = null
  var sourceEditor: codemirror.CodeMirror = null

  var errorMarks: List[js.Dynamic] = List()

  def setupUI {
    dom.onbeforeunload = (_: BeforeUnloadEvent) => "Are you sure you want to leave this page?"

    val modeDef = js.Dynamic.literal(
      meta = js.Dynamic.literal(
        lineComment = "--"),
      start = js.Array(
        js.Dynamic.literal(
          regex = new js.RegExp(raw"(?:define|out)\b"),
          token = "keyword"),
        js.Dynamic.literal(
          regex = new js.RegExp(raw"--.*"),
          token = "comment"),
        js.Dynamic.literal(
          regex = new js.RegExp(raw"\b\d+"),
          token = "number"),
        js.Dynamic.literal(
          regex = new js.RegExp(raw":=?"),
          token = "operator")))
    CodeMirror.defineSimpleMode("tessla", modeDef)

    jQuery("body").empty()
    jQuery("body").append(Templates.navigation)
    jQuery("body").append(Templates.editors)

    var textArea = jQuery("#tessla-editor").get(0) match {
      case ok: HTMLTextAreaElement => ok
      case _                       => throw new Exception("codemirror not found")
    }
    var opts = new Options()
    opts.mode = "tessla"
    opts.lineNumbers = true

    opts.extraKeys = js.Dictionary("Ctrl-Space" -> ((cm: CodeMirror) => {
      cm.showHint()
    }))

    opts.lint = true
    opts.gutters = js.Array("CodeMirror-lint-markers")

    CodeMirror.registerHelper("lint", "tessla", lint _)
    CodeMirror.registerHelper("hint", "tessla", hint _)

    tesslaEditor = CodeMirror.fromTextArea(textArea, opts)

    textArea = jQuery("#target-editor").get(0) match {
      case ok: HTMLTextAreaElement => ok
      case _                       => throw new Exception("codemirror not found")
    }
    opts = new codemirror.Options()
    opts.mode = "text/x-csrc"
    opts.lineNumbers = true
    sourceEditor = CodeMirror.fromTextArea(textArea, opts)

  }

  def lint(content: String, options: js.Dynamic, cm: CodeMirror): js.Array[js.Dynamic] = {
    if (content.length() == 0) {
      return js.Array()
    }
    parseAll(spec(), Source.fromString(content)) match {
      case Success(_, result, _, _) =>
        val parseResult = jQuery("#parse-result")
        parseResult.empty()
        parseResult.append(result.toString)
        parseResult.append("<br />\n")
        parseResult.append(resolveMacros(result) match {
          case Left(CyclicDefinitionError(macroDef)) => "Cyclic macro definition in macro " + macroDef.name
          case Left(e)                               => e.toString()
          case Right(spec) =>
            "After macro resolution: <br />\n" +
              spec.statements.map { _.toText }.mkString("<br />\n")
        })

        js.Array()
      case failure@Failure(pos, _, _, _) =>
        val failureStart = Place(pos.from.line - 1, pos.from.column - 1)
        val failureEnd = Place(pos.to.line - 1, pos.to.column - 1)

        js.Array(
          js.Dynamic.literal(
            message = failure.message.split(": ", 2)(1),
            severity = "error",
            from = failureStart.asInstanceOf,
            to = failureEnd.asInstanceOf))
    }
  }

  def hint(cm: CodeMirror, options: js.Dynamic): Completions = {
    val prefix = cm.getRange(Place(0, 0), cm.getCursor())
    val result = parseAll(spec(), Source.fromString(prefix))
    println(prefix)
    println(result.completions)

    val completions = for (comp <- result.completions.to[js.Array])
      yield js.Dynamic.literal(text = comp + " "): js.Any

    Completions(cm.getCursor(), cm.getCursor(), completions)
  }

  def main(): Unit = {
    jQuery(() => setupUI)
  }
}

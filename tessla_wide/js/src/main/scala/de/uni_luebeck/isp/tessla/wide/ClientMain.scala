package de.uni_luebeck.isp.tessla.wide

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.jquery.jQuery
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.dom
import org.scalajs.dom.raw.BeforeUnloadEvent
import de.uni_luebeck.isp.tessla.Parser._
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
            lineComment = "--"
        ),
        start = js.Array(
            js.Dynamic.literal(
                regex = new js.RegExp(raw"(?:define|out)\b"),
                token = "keyword"
            ),
            js.Dynamic.literal(
                regex = new js.RegExp(raw"--.*"),
                token = "comment"
            ),
            js.Dynamic.literal(
                regex = new js.RegExp(raw"\b\d+"),
                token = "number"
            ),
            js.Dynamic.literal(
                regex = new js.RegExp(raw":=?"),
                token = "operator"
            )
        )
    )
    CodeMirror.defineSimpleMode("tessla", modeDef)
    
    jQuery("body").empty()
    jQuery("body").append(Templates.navigation)
    jQuery("body").append(Templates.editors)
    
    
    var textArea = jQuery("#tessla-editor").get(0) match {
      case ok: HTMLTextAreaElement => ok
      case _ => throw new Exception("codemirror not found")
    }
    var opts = new Options()
    opts.mode = "tessla"
    opts.lineNumbers = true
    tesslaEditor = CodeMirror.fromTextArea(textArea, opts)
    
    textArea = jQuery("#target-editor").get(0) match {
      case ok: HTMLTextAreaElement => ok
      case _ => throw new Exception("codemirror not found")
    }
    opts = new codemirror.Options()
    opts.mode = "text/x-csrc"
    opts.lineNumbers = true
    sourceEditor = CodeMirror.fromTextArea(textArea, opts)
    
    tesslaEditor.on("changes", (changeObject: js.Dynamic) => {
      for (mark <- errorMarks) {
        mark.clear()
      }
      errorMarks = List()
      val changeGen = tesslaEditor.changeGeneration(true)
      dom.window.setTimeout(() => performUpdate(changeGen), 600.0)
    })
    
  }
  
  def performUpdate(changeGen: Integer) {
    if (changeGen == tesslaEditor.changeGeneration(true)) {
      
      
      parseAll(spec(), Source.fromString(tesslaEditor.getValue())) match {
        case Success(_, _, _, _) => {}
        case failure @ Failure(pos, _, _, _) =>
          val failureStart = Place(pos.from.line - 1, pos.from.column - 1)
          val failureEnd = Place(pos.to.line - 1, pos.to.column - 1)
          
          val bookmark = jQuery("""<div class="error-bookmark"><div></div></div>""")
          
          bookmark.find("div").append(failure.message.split(": ", 2)(1))
          
          errorMarks = List(
            tesslaEditor.setBookmark(
              failureStart, js.Dynamic.literal(
                widget = bookmark.get(0))))
            
          if (pos.from != pos.to) {
            errorMarks ++= List(
              tesslaEditor.markText(
                failureStart, failureEnd, js.Dynamic.literal(
                  className = "tessla-error", title = failure.message)))
          }
      }
    }
  }
  
  def main(): Unit = {
    jQuery(() => setupUI)
  }
}

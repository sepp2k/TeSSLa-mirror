package de.uni_luebeck.isp.tessla.wide.codemirror

import scala.scalajs.js
import js.annotation.JSName
import org.scalajs.dom.raw.HTMLTextAreaElement
import scala.annotation.meta.field

class Place {
  def dynamic: js.Dynamic = this.asInstanceOf[js.Dynamic]
  
  def line = dynamic.line.asInstanceOf[Int]
  def line_=(v: Int) {dynamic.line = v}
  
  def ch = dynamic.ch.asInstanceOf[Int]
  def ch_=(v: Int) {dynamic.ch = v}
}

object Place {
  def apply(line: Int, ch: Int): Place = {
    val result = new Place
    result.line = line
    result.ch = ch
    result
  }
}

class Completions {
  def dynamic: js.Dynamic = this.asInstanceOf[js.Dynamic]
  
  def from = dynamic.from.asInstanceOf[Place]
  def from_=(v: Place) {dynamic.from = v.asInstanceOf}
  
  def to = dynamic.to.asInstanceOf[Place]
  def to_=(v: Place) {dynamic.to = v.asInstanceOf}
  
  def list = dynamic.list.asInstanceOf[js.Array[js.Any]]
  def list_=(v: js.Array[js.Any]) {dynamic.list = v.asInstanceOf}
}

object Completions {
  def apply(from: Place, to: Place, list: js.Array[js.Any]): Completions = {
    val result = new Completions
    result.from = from
    result.to = to
    result.list = list
    result
  }
}

class Document extends js.Object {
  def changeGeneration(closeEvent: Boolean): Int = js.native
  
  def getValue(separator: String = "\n"): String = js.native
  def setValue(content: String): Unit = js.native
  def getRange(from: Place, to: Place, separator: String = "\n"): String = js.native
  
  def markText(from: Place, to: Place, options: js.Dynamic): js.Dynamic = js.native
  def setBookmark(pos: Place, options: js.Dynamic): js.Dynamic = js.native
  
  def getCursor(start: String = "head"): Place = js.native
}

class CodeMirror extends Document {
  def on(event: String, callback: js.Function): Unit = js.native
  def off(event: String, callback: js.Function): Unit = js.native
  
  def showHint(options: js.Dynamic = null): Unit = js.native
}

class Options() {
  def dynamic: js.Dynamic = this.asInstanceOf[js.Dynamic]
  
  def mode = dynamic.mode
  def mode_=(v: String) {dynamic.mode = v}
  
  def lint = dynamic.lint
  def lint_=(v: Boolean) {dynamic.lint = v}
  
  def gutters = dynamic.gutters.asInstanceOf[js.Array[String]]
  def gutters_=(v: js.Array[String]) {dynamic.gutters = v}
  
  def lineNumbers = (dynamic.lineNumbers).asInstanceOf[Boolean]
  def lineNumbers_=(v: Boolean) {dynamic.lineNumbers = v}
  
  def extraKeys = dynamic.extraKeys.asInstanceOf[js.Dictionary[js.Function1[CodeMirror, Unit]]]
  def extraKeys_=(v: js.Dictionary[js.Function1[CodeMirror, Unit]])
    {dynamic.extraKeys = v}
}


object CodeMirror extends js.Object {

  def fromTextArea(place: HTMLTextAreaElement, options: Options): CodeMirror = js.native
  def defineSimpleMode(name: String, definition: js.Dynamic): Unit = js.native
  
  def registerHelper(typ: String, name: String, value: js.Any): Unit = js.native
}


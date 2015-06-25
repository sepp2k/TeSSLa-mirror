package de.uni_luebeck.isp.tessla.wide.codemirror

import scala.scalajs.js
import js.annotation.JSName
import org.scalajs.dom.raw.HTMLTextAreaElement
import scala.annotation.meta.field

class Place() {
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

class Document extends js.Object {
  def changeGeneration(closeEvent: Boolean): Int = js.native
  
  def getValue(separator: String = "\n"): String = js.native
  def setValue(content: String): Unit = js.native
  
  def markText(from: Place, to: Place, options: js.Dynamic): js.Dynamic = js.native
  def setBookmark(pos: Place, options: js.Dynamic): js.Dynamic = js.native
}

class CodeMirror extends Document {
  def on(event: String, callback: js.Function): Unit = js.native
  def off(event: String, callback: js.Function): Unit = js.native
}

class Options() {
  def dynamic: js.Dynamic = this.asInstanceOf[js.Dynamic]
  
  def mode = dynamic.mode
  def mode_=(v: String) {dynamic.mode = v}
  
  def lineNumbers = (dynamic.lineNumbers).asInstanceOf[Boolean]
  def lineNumbers_=(v: Boolean) {dynamic.lineNumbers = v}
}


object CodeMirror extends js.Object {

  def fromTextArea(place: HTMLTextAreaElement, options: Options): CodeMirror = js.native
  def defineSimpleMode(name: String, definition: js.Dynamic): Unit = js.native
}


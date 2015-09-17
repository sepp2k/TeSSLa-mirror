package de.uni_luebeck.isp.tessla

import scala.scalajs.js.JSApp
import de.uni_luebeck.isp.tessla.Parser._
import scala.io.Source

object TestMain extends JSApp {
  def main() {
    
      val result = parseAll(spec(), Source.fromString("define foo(x) := bar(y, x) define y : int := 123"))
      println(result)
  }
}
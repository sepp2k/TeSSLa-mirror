package de.uni_luebeck.isp.tessla

import java.io.File

import scala.io.Source
import scala.util.Success

object CompilerApp extends App {

  case class Config(
                     debug: Boolean = false,
                     file: Source = Source.stdin)

  val parser = new scopt.OptionParser[Config]("tesslac") {
    head("tesslac", "0.2")
    (opt[Unit]('d', "debug") text "enable verbose debug output"
      action { (_, c) => c.copy(debug = true) })
    (arg[File]("<source>") text "input tessla source (default: stdin)"
      optional() action { (f, c) =>
      c.copy(file = Source.fromFile(f))
    })
    help("help") text "prints this usage text"
  }

  var dbg = false

  val result = parser.parse(args, Config()) match {
    case Some(config) =>
      dbg = config.debug
      val compiler = new Compiler(debug = config.debug)
      compiler.applyPasses(new TesslaSource(config.file))
    case None =>
  }

  if (!dbg) { //debug output will print result
    result match {
      case Some(result) => {
        println(result)
      }
      case _ =>
    }
  }
}
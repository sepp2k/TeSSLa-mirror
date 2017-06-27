package de.uni_luebeck.isp.tessla

import java.io.File

import scala.io.Source
import scala.util.Success

object CompilerApp extends App {

  case class Config(file: Source = Source.stdin)

  val parser = new scopt.OptionParser[Config]("tesslac") {
    head("tesslac", "0.2")
    (arg[File]("<source>") text "input tessla source (default: stdin)"
      optional() action { (f, c) =>
      c.copy(file = Source.fromFile(f))
    })
    help("help") text "prints this usage text"
  }

  val result = parser.parse(args, Config()) match {
    case Some(config) =>
      val compiler = new Compiler
      compiler.applyPasses(new TesslaSource(config.file))
    case None =>
  }

  result match {
    case TranslationPhase.Success(value, warnings) =>
      println(value)
      if (warnings.isEmpty) println("Compilation succeeded without warnings")
      else {
        println(s"Compilation succeeded with ${warnings.length} warnings:")
        warnings.foreach(w => println(s"Warning: $w"))
      }
    case TranslationPhase.Failure(errors, warnings) =>
      println(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors:")
      warnings.foreach(w => println(s"Warning: $w"))
      errors.foreach(e => println(s"Error: $e"))
  }
}
package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val (tesslaFile, traceSource) = args match {
      case Array(tesslaFile, traceFile) => (tesslaFile, Source.fromFile(traceFile))
      case Array(tesslaFile) => (tesslaFile, Source.stdin)
      case _ =>
        System.err.println("Usage: tessla-interpreter tessla-file [trace-file]")
        sys.exit(1)
    }
    val tesslaSpec = Interpreter.fromFile(tesslaFile) match {
      case Success(spec, warnings) =>
        warnings.foreach(w => println(s"Warning: $w"))
        spec
      case Failure(errors, warnings) =>
        println(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors:")
        warnings.foreach(w => println(s"Warning: $w"))
        errors.foreach(e => println(s"Error: $e"))
        sys.exit(1)
    }
    tesslaSpec.outStreams.foreach { case (name, stream) => tesslaSpec.printStream(stream, name) }
    Traces.feedInput(tesslaSpec, traceSource)
  }
}

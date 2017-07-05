package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import sexyopt.SexyOpt
import scala.io.Source

object Main extends SexyOpt {
  val programName = "tessla-interpreter"
  val programDescription = "Evaluate the given Tessla specification on the input streams provided by the given trace file."

  val tesslaFile = posArg("tessla-file", "The file containing the Tessla specification")
  val traceFile = optionalPosArg("trace-file", "The file containing the trace data used as input for the specification." +
                                               " If this is not provided, input is read from stdin")
  val verifyOnly = flag("verify-only", "Only check the Tessla spec for errors and don't execute it")
  val noDiagnostics = flag("no-diagnostics", "Don't print error messages and warnings")
  def diagnostics = !noDiagnostics.value
  val printCore = flag("print-core", "Print the Tessla Core representation generated from the Tessla specification")

  def main(args: Array[String]): Unit = {
    parse(args)
    val traceSource = traceFile.value.map(Source.fromFile).getOrElse(Source.stdin)
    val tesslaSpec = Interpreter.fromFile(tesslaFile.value) match {
      case Success(spec, warnings) =>
        if (diagnostics) warnings.foreach(w => System.err.println(s"Warning: $w"))
        if (printCore.value) println(spec.spec)
        spec
      case Failure(errors, warnings) =>
        if (diagnostics) {
          warnings.foreach(w => System.err.println(s"Warning: $w"))
          errors.foreach(e => System.err.println(s"Error: $e"))
          System.err.println(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors:")
        }
        sys.exit(1)
    }
    if (verifyOnly.value) return
    tesslaSpec.outStreams.foreach { case (name, stream) => tesslaSpec.printStream(stream, name) }
    Traces.feedInput(tesslaSpec, traceSource)
  }
}

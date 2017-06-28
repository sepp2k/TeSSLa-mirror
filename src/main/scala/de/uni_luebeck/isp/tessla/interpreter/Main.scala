package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import sexyopt.SexyOpt
import scala.io.Source

object Main extends SexyOpt {
  val programName = "tessla-interpreter"
  val programDescription = "Evaluate the given Tessla specification on the input streams provided by the given trace file."

  val tesslaFile = posArg("tessla-file", "The file containing the Tessla specification")
  val traceFile = optionalPosArg("trace-file", "The file containing the trace data used as input for the specification." +
                                               "If this is not provided, input is read froms stdin")

  def main(args: Array[String]): Unit = {
    parse(args)
    val traceSource = traceFile.value.map(Source.fromFile).getOrElse(Source.stdin)
    val tesslaSpec = Interpreter.fromFile(tesslaFile.value) match {
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

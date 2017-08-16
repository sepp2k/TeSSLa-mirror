package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{CompilationError, TimeUnit}
import de.uni_luebeck.isp.tessla.TimeUnit.Nanos
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
  val debug = flag("debug", "Print stack traces for runtime errors")
  val threshold = option("threshold", "Allowed maximal difference between decreasing timestamps (default: 100,000)", "100000")
  val stopOn = option("stop-on", "Stop when the output stream with the given name generates its first event")
  val listOutStreams = flag("list-out-streams", "Print a list of the output streams defined in the given tessla spec and then exit")
  val listInStreams = flag("list-in-streams", "Print a list of the input streams defined in the given tessla spec and then exit")
  val printVersion = flag("version", "Print the version number of the tessla interpreter and then exit")

  def main(args: Array[String]): Unit = {
    def tesslaSpec(timeUnit: Option[TimeUnit.TimeUnit]) = Interpreter.fromFile(tesslaFile, timeUnit) match {
      case Success(spec, warnings) =>
        if (diagnostics) warnings.foreach(w => System.err.println(s"Warning: $w"))
        if (printCore) println(spec.spec)
        spec
      case Failure(errors, warnings) =>
        if (diagnostics) {
          warnings.foreach(w => System.err.println(s"Warning: $w"))
          errors.foreach(e => System.err.println(s"Error: $e"))
          System.err.println(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors")
        }
        sys.exit(1)
    }

    parse(args)
    if(printVersion) {
      println(BuildInfo.version)
      return
    }
    if (verifyOnly || listInStreams || listOutStreams) {
      val spec = tesslaSpec(None)
      if (listInStreams) {
        spec.inStreams.foreach { case (name, _) => println(name) }
        return
      }
      if (listOutStreams) {
        spec.outStreams.foreach { case (name, _) => println(name) }
        return
      }
    } else {
      try {
        val traces = Traces.read(traceFile.map(Source.fromFile).getOrElse(Source.stdin))
        traces.timeStampUnit.foreach(unit => println("$timeunit = \"" + unit + "\""))
        val spec = tesslaSpec(traces.timeStampUnit)
        traces.feedInput(spec, BigInt(threshold)) {
          case (Some(ts), name, value) =>
            println(s"$ts: $name = $value")
            if (stopOn.contains(name)) return
          case (None, name, value) => println(s"$name = $value")
        }
      } catch {
        case ex: CompilationError =>
          System.err.println(s"Runtime error: $ex")
          if (debug) ex.printStackTrace()
      }
    }
  }
}

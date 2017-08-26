package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.{CommandLineLoc, TesslaSource, TimeUnit}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import sexyopt.SexyOpt

import scala.io.Source

object Main extends SexyOpt {
  override val programName = "tessla"
  override val version = Some(BuildInfo.version)
  override val programDescription = "Evaluate the given Tessla specification on the input streams provided by the given trace file."

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
  val timeunit = option("timeunit", "Use the given unit as the unit for timestamps in the input")

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
    try {
      val tu = timeunit.map{
        s => val arg = args.mkString(" ").indexOfSlice(timeunit.get)
          TimeUnit.fromString(s, CommandLineLoc(arg-11, arg + timeunit.get.size, timeunit.get))
      }
      if (verifyOnly || listInStreams || listOutStreams) {
        val spec = tesslaSpec(tu)
        if (listInStreams) {
          spec.inStreams.foreach { case (name, _) => println(name) }
          return
        }
        if (listOutStreams) {
          spec.outStreams.foreach { case (name, _) => println(name) }
          return
        }
      } else {
        val traces: Traces = TracesParser.parseTraces(traceFile.map(TesslaSource.fromFile).getOrElse(new TesslaSource(Source.stdin)))
        val tu2 = tu.orElse(traces.timeStampUnit.map(_.timeUnit))
        tu2.foreach(unit => println("$timeunit = \"" + unit + "\""))
        val spec = tesslaSpec(tu2)
        traces.feedInput(spec, BigInt(threshold)) {
          case (ts, name, value) =>
            println(s"$ts: $name = $value")
            if (stopOn.contains(name)) return
        }
      }
    } catch {
      case ex: TesslaError =>
        System.err.println(s"Runtime error: $ex")
        if (debug) ex.printStackTrace()
    }
  }
}

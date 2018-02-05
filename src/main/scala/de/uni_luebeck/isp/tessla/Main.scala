package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{InputTypeMismatch, TesslaError, UndeclaredInputStreamError}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.interpreter._
import sexyopt.SexyOpt

import scala.collection.mutable

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
  val stopOn = option("stop-on", "Stop when the output stream with the given name generates its first event")
  val listOutStreams =
    flag("list-out-streams", "Print a list of the output streams defined in the given tessla spec and then exit")
  val listInStreams =
    flag("list-in-streams", "Print a list of the input streams defined in the given tessla spec and then exit")
  val timeUnit = option("timeunit", "Use the given unit as the unit for timestamps in the input")
  val abortAt = option("abort-at", "Stop the interpreter after a given amount of events.")
  val flattenInput = flag("flatten-input", "Print the input trace in a flattened form.")
  val computationDepth = flag("print-computation-depth", "Print the length of the longest path a propagation message travels")

  def main(args: Array[String]): Unit = {
    def unwrapResult[T](result: Result[T]): T = result match {
      case Success(res, warnings) =>
        if (diagnostics) warnings.foreach(w => System.err.println(s"Warning: $w"))
        res
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
      val specSource = TesslaSource.fromFile(tesslaFile)
      val timeUnitSource = timeUnit.map(TesslaSource.fromString(_, "--timeunit"))
      if (verifyOnly || listInStreams || listOutStreams || computationDepth) {
        val spec = unwrapResult(new Compiler().compile(specSource, timeUnitSource))
        if (listInStreams) {
          spec.inStreams.foreach { case (name, _, _) => println(name) }
          return
        }
        if (listOutStreams) {
          spec.outStreams.foreach { case (name, _) => println(name) }
          return
        }
        if (computationDepth) {
          println(DepthChecker.nestingDepth(spec))
          return
        }
      } else {
        val abortAtValue = abortAt.map(BigInt(_))
        val traceSource = traceFile.map(TesslaSource.fromFile).getOrElse(TesslaSource.stdin)
        if (flattenInput) {
          Interpreter.flattenInput(traceSource, timeUnitSource, abortAtValue).foreach(println)
        } else {
          val output = unwrapResult {
            Interpreter.runSpec(specSource, traceSource, timeUnit = timeUnitSource, stopOn = stopOn, printCore = printCore, abortAt = abortAtValue)
          }
          output.foreach(println)
        }
      }
    } catch {
      case ex: TesslaError =>
        System.err.println(s"Runtime error: $ex")
        if (debug) ex.printStackTrace()
    }
  }
}

package de.uni_luebeck.isp.tessla

import java.io.IOException

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.analyses._
import de.uni_luebeck.isp.tessla.interpreter._
import org.antlr.v4.runtime.CharStreams
import sexyopt.SexyOpt

import scala.io.Source

object Main extends SexyOpt {
  override val programName = BuildInfo.name
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

  val observations = flag("observations", "Generate observation specification file from the corresponding annotations")

  val abortAt = option("abort-at", "Stop the interpreter after a given amount of events.")

  val flattenInput = flag("flatten-input", "Print the input trace in a flattened form.")

  val computationDepth = flag("print-computation-depth", "Print the length of the longest path a propagation message travels")

  val recursionDepth = flag("print-recursion-depth", "Print the length of the longest recursion")

  val nodeCount = flag("print-node-count", "Print the number of nodes in the TeSSLaCore graph")

  val ctfTrace = flag("ctf", "The trace-file with the input data is in CTF format. With this option you must specify " +
    "a trace-file. stdin is not supported.")

  val csvTrace = flag("csv", "The trace-file or the input stream is in CSV format.")

  def main(args: Array[String]): Unit = {
    def unwrapResult[T](result: Result[T]): T = result match {
      case Success(res, warnings) =>
        if (diagnostics) warnings.foreach(w => System.err.println(s"Warning: $w"))
        res
      case Failure(errors, warnings) =>
        if (diagnostics) {
          warnings.foreach(w => System.err.println(s"Warning: $w"))
          errors.foreach { e =>
            System.err.println(s"Error: $e")
            if (debug) e.printStackTrace()
          }
          System.err.println(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors")
        }
        sys.exit(1)
    }

    parse(args)

    val evaluator = new Evaluator(Map())
    val compiler = new Compiler(evaluator)
    try {
      val specSource = CharStreams.fromFileName(tesslaFile)
      val compilerOptions = Compiler.Options(
        timeUnitString = timeUnit,
        includeResolver = IncludeResolvers.fromFile,
        stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
        stdlibPath = "stdlib.tessla"
      )
      val core = unwrapResult(compiler.compile(specSource, compilerOptions))

      if (observations) {
        println(unwrapResult(Observations.Generator.translate(core)))
        return
      }

      if (listInStreams) {
        core.inStreams.foreach { is => println(is.name) }
      }

      if (listOutStreams) {
        core.outStreams.foreach { os => println(os.nameOpt.getOrElse(s"print ${os.stream}")) }
      }

      if (computationDepth) {
        println(DepthChecker.nestingDepth(core))
      }

      if (recursionDepth) {
        println(RecursiveDepthChecker.nestingDepth(core))
      }

      if (nodeCount) {
        println(NodeCounter.nodeCount(core))
      }

      if (printCore) {
        println(core)
      }

      if (listInStreams || listOutStreams || computationDepth || recursionDepth || nodeCount || verifyOnly) {
        return
      }

      val abortAtValue = abortAt.map(BigInt(_))

      val traceInstance = new Trace(evaluator)

      if (ctfTrace) {
        if (!traceFile.isDefined) {
          System.err.println("No CTF trace input given")
          sys.exit(17)
        }
        val trace = traceInstance.fromCtfFile(traceFile.get, abortAtValue)
        val output = Interpreter.run(core, trace, stopOn, evaluator)
        output.foreach(println)
      } else {
        val trace = if (csvTrace) {
          traceFile.map(traceInstance.fromCsvFile(_, abortAtValue))
            .getOrElse(traceInstance.fromCsvSource(Source.stdin, "<stdin>", abortAtValue))
        } else {
          traceFile.map(traceInstance.fromFile(_, abortAtValue))
            .getOrElse(traceInstance.fromSource(Source.stdin, "<stdin>", abortAtValue))
        }
        if (flattenInput) {
          trace.foreach(println)
        } else {
          val output = Interpreter.run(core, trace, stopOn, evaluator)
          output.foreach(println)
        }
      }
    } catch {
      case ex: TesslaError =>
        System.err.println(s"Runtime error: $ex")
        if (debug) ex.printStackTrace()
      case ex: IOException =>
        System.err.println(s"IO Error: ${ex.getMessage}")
        if (debug) ex.printStackTrace()
    }
  }
}

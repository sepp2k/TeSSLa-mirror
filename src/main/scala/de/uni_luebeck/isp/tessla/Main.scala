package de.uni_luebeck.isp.tessla

import java.io.{File, IOException}

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.analyses.Observations
import de.uni_luebeck.isp.tessla.interpreter._
import org.antlr.v4.runtime.{CharStream, CharStreams}
import scopt.OptionParser

import scala.io.Source

object Main {
  val programName = BuildInfo.name
  val programVersion = BuildInfo.version
  val programDescription =
    "Evaluate the given Tessla specification on the input streams provided by the given trace file."
  val licenseLocation = "de/uni_luebeck/isp/tessla/License"

  case class Config(
    specSource: CharStream = null,
    traceFile: Option[File] = None,
    verifyOnly: Boolean = false,
    diagnostics: Boolean = true,
    printCore: Boolean = false,
    printTyped: Boolean = false,
    printLocations: Boolean = false,
    printAllTypes: Boolean = false,
    debug: Boolean = false,
    stopOn: Option[String] = None,
    listOutStreams: Boolean = false,
    listInStreams: Boolean = false,
    observations: Boolean = false,
    abortAt: Option[BigInt] = None,
    flattenInput: Boolean = false,
    ctfTrace: Boolean = false,
    csvTrace: Boolean = false,
    compilerOptions: Compiler.Options = Compiler.Options(
      baseTimeString = None,
      includeResolver = IncludeResolvers.fromFile,
      stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
      stdlibPath = "stdlib.tessla"
    )
  )

  val parser: OptionParser[Config] = new OptionParser[Config](programName) {
    head(s"$programName $programVersion")
    note(programDescription)
    arg[File]("<tessla-file>")
      .action((s, c) => c.copy(specSource = CharStreams.fromFileName(s.getPath)))
      .text("The file containing the Tessla specification")
    arg[File]("<trace-file>")
      .optional()
      .action((s, c) => c.copy(traceFile = Some(s)))
      .text(
        "The file containing the trace data used as input for the specification." +
          " If this is not provided, input is read from stdin"
      )
    opt[String]("stop-on")
      .action((s, c) => c.copy(stopOn = Some(s)))
      .text("Stop when the output stream with the given name generates its first event")
    opt[String]("base-time")
      .action((s, c) => c.copy(compilerOptions = c.compilerOptions.copy(baseTimeString = Some(s))))
      .text(
        "Use the given time constant (including a unit) as the reference time for timestamps in the input trace"
      )
    opt[BigInt]("abort-at")
      .action((s, c) => c.copy(abortAt = Some(s)))
      .text("Stop the interpreter after a given amount of events.")
    opt[File]("stdlib")
      .valueName("<file>")
      .action((f, c) =>
        c.copy(compilerOptions =
          c.compilerOptions
            .copy(stdlibPath = f.getPath, stdlibIncludeResolver = IncludeResolvers.fromFile)
        )
      )
      .text("Use the given standard library instead of the default.")
    note("") // Spacer
    opt[Unit]("verify-only")
      .action((_, c) => c.copy(verifyOnly = true))
      .text("Only check the Tessla spec for errors and don't execute it")
    opt[Unit]("no-diagnostics")
      .action((_, c) => c.copy(diagnostics = false))
      .text("Don't print error messages and warnings")
    opt[Unit]("print-core")
      .action((_, c) => c.copy(printCore = true))
      .text("Print the Tessla Core representation generated from the Tessla specification")
    opt[Unit]("print-typed")
      .action((_, c) => c.copy(printTyped = true))
      .text("Print the typed Tessla representation generated from the Tessla specification")
    opt[Unit]("print-locations")
      .action((_, c) => c.copy(printLocations = true))
      .text("Print ASTs with locations")
    opt[Unit]("print-all-types")
      .action((_, c) => c.copy(printAllTypes = true))
      .text("Print ASTs with all types")
    opt[Unit]("debug")
      .hidden()
      .action((_, c) => c.copy(debug = true))
      .text("Print stack traces for runtime errors")
    opt[Unit]("list-out-streams")
      .action((_, c) => c.copy(listOutStreams = true))
      .text("Print a list of the output streams defined in the given tessla spec and then exit")
    opt[Unit]("list-in-streams")
      .action((_, c) => c.copy(listInStreams = true))
      .text("Print a list of the input streams defined in the given tessla spec and then exit")
    opt[Unit]("observations")
      .action((_, c) => c.copy(observations = true))
      .text("Generate observation specification file from the corresponding annotations")
    opt[Unit]("flatten-input")
      .action((_, c) => c.copy(flattenInput = true))
      .text("Print the input trace in a flattened form.")
    opt[Unit]("ctf")
      .action((_, c) => c.copy(ctfTrace = true))
      .text(
        "The trace-file with the input data is in CTF format. With this option you must specify " +
          "a trace-file. stdin is not supported."
      )
    opt[Unit]("csv")
      .action((_, c) => c.copy(csvTrace = true))
      .text("The trace-file or the input stream is in CSV format.")
    note("") // Spacer
    help("help")
      .text("Prints this help message and exit.")
    version("version")
      .text("Print the version and exit.")
    opt[Unit]("license")
      .action((_, _) => {
        println(Source.fromResource(licenseLocation).mkString)
        sys.exit(0)
      })
      .text("Print the legal information for this software and exit.")
    // Final validation
    checkConfig(c =>
      if (c.specSource == null) failure("No Tessla specification provided.")
      else if (c.ctfTrace && c.traceFile.isEmpty) failure("No CTF trace input given.")
      else success
    )
  }

  def main(args: Array[String]): Unit = {
    val config = parser.parse(args, Config()).getOrElse(sys.exit(1))

    def unwrapResult[T](result: Result[T]): T = result match {
      case Success(res, warnings) =>
        if (config.diagnostics) warnings.foreach(w => System.err.println(s"Warning: $w"))
        res
      case Failure(errors, warnings) =>
        if (config.diagnostics) {
          warnings.foreach(w => System.err.println(s"Warning: $w"))
          errors.foreach { e =>
            System.err.println(s"Error: $e")
            if (config.debug) e.printStackTrace()
          }
          System.err.println(
            s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors"
          )
        }
        sys.exit(1)
    }

    val compiler = new Compiler
    try {
      val result = unwrapResult(compiler.compile(config.specSource, config.compilerOptions))
      val core = unwrapResult(result._2)
      val printOptions = TesslaAST.PrintOptions(
        !config.printAllTypes,
        config.printAllTypes,
        config.printAllTypes,
        paramTypes = true,
        config.printLocations
      )

      if (config.printTyped) {
        println(result._1.print(printOptions))
      }

      if (config.printCore) {
        println(core.print(printOptions))
        return
      }

      if (config.observations) {
        println(unwrapResult(Observations.Generator.translate(core)))
        return
      }

      if (config.listInStreams) {
        core.in.foreach { is => println(is._1.idOrName) }
        return
      }

      if (config.listOutStreams) {
        core.out.foreach { os => println(os._1.id.idOrName) }
        return
      }

      if (config.verifyOnly) {
        return
      }

      val traceInstance = new Trace()

      if (config.ctfTrace) {
        val trace = traceInstance.fromCtfFile(config.traceFile.get, config.abortAt)
        val output = Interpreter.run(core, trace, config.stopOn)
        output.foreach(println)
      } else {
        val trace = if (config.csvTrace) {
          config.traceFile
            .map(traceInstance.fromCsvFile(_, config.abortAt))
            .getOrElse(traceInstance.fromCsvSource(Source.stdin, "<stdin>", config.abortAt))
        } else {
          config.traceFile
            .map(traceInstance.fromFile(_, config.abortAt))
            .getOrElse(traceInstance.fromSource(Source.stdin, "<stdin>", config.abortAt))
        }
        if (config.flattenInput) {
          trace.foreach(println)
        } else {
          val output = Interpreter.run(core, trace, config.stopOn)
          output.foreach(println)
        }
      }
    } catch {
      case ex: TesslaError =>
        System.err.println(s"Runtime error: $ex")
        if (config.debug) ex.printStackTrace()
      case ex: IOException =>
        System.err.println(s"IO Error: ${ex.getMessage}")
        if (config.debug) ex.printStackTrace()
    }
  }
}

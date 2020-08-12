package de.uni_luebeck.isp.tessla

import java.io.File

import org.antlr.v4.runtime.{CharStream, CharStreams}
import scopt.OptionParser
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers}

import scala.collection.mutable
import scala.io.Source

object CLIParser {
  val programName: String = BuildInfo.name
  val programVersion: String = BuildInfo.version
  val programDescription =
    "Compile Tessla specifications and evaluate them on provided input streams."
  val licenseLocation = "de/uni_luebeck/isp/tessla/License"

  sealed trait Config

  case class DocConfig(
    stdLib: Boolean = false,
    includes: Boolean = false,
    outfile: Option[File] = None,
    sources: Seq[CharStream] = Seq()
  ) extends Config

  case class CoreConfig(
    specSource: CharStream = null,
    printCore: Boolean = false,
    printCoreLanSpec: Boolean = false,
    printTyped: Boolean = false,
    printLocations: Boolean = false,
    printAllTypes: Boolean = false,
    listOutStreams: Boolean = false,
    listInStreams: Boolean = false,
    observations: Boolean = false,
    compilerOptions: Compiler.Options = Compiler.Options()
  ) extends Config

  case class InterpreterConfig(
    specSource: CharStream = null,
    traceFile: Option[File] = None,
    stopOn: Option[String] = None,
    abortAt: Option[BigInt] = None,
    traceFormat: String = "tessla",
    rejectUndeclaredInputs: Boolean = false,
    ctfTrace: Boolean = false,
    csvTrace: Boolean = false,
    compilerOptions: Compiler.Options = Compiler.Options()
  ) extends Config

  case class GlobalConfig(
    diagnostics: Boolean = true,
    debug: Boolean = false
  )

  case class Task[T <: Config](name: String, config: T)

  private var global = GlobalConfig()
  private var compilerOptions = Compiler.Options()
  private val tasks = mutable.ArrayBuffer[() => Task[Config]]()

  private val parser: OptionParser[Unit] = new OptionParser[Unit](programName) {
    head(s"$programName $programVersion")
    note(programDescription)
    opt[String]('t', "base-time")
      .action((s, _) => compilerOptions = compilerOptions.copy(baseTimeString = Some(s)))
      .text(
        "Use the given time constant (including a unit) as the reference time for time literals" +
          "(only in 'interpreter' and 'compile-core'"
      )
    opt[File]('s', "stdlib")
      .valueName("<file>")
      .action((f, _) =>
        compilerOptions = compilerOptions.copy(
          stdlibPath = f.getPath,
          stdlibIncludeResolver = IncludeResolvers.fromFile
        )
      )
      .text(
        "Provide a standard library to use instead of the default one." +
          "(only in 'interpreter' and 'compile-core'"
      )
    opt[Unit]("debug")
      .hidden()
      .action((_, _) => global = global.copy(debug = true))
      .text("Print stack traces for runtime errors")
    note("") // Spacer
    help("help")
      .text("Prints this help message and exit.")
    version("version")
      .text("Print the version and exit.")
    opt[Unit]('l', "license")
      .action((_, _) => {
        println(Source.fromResource(licenseLocation).mkString)
        sys.exit(0)
      })
      .text("Print the legal information for this software and exit.")
  }

  // Extension for the 'interpreter' command
  {
    import parser._
    var config = InterpreterConfig()

    note("")
    cmd("interpreter")
      .text("Evaluate the given Tessla specification on the input streams provided by a trace file.")
      .action((_, _) => tasks += (() => Task("interpreter", config.copy(compilerOptions = compilerOptions))))
      .children(
        arg[File]("<tessla-file>")
          .optional()
          .action((s, _) => config = config.copy(specSource = CharStreams.fromFileName(s.getPath)))
          .text("The file containing the Tessla specification"),
        arg[File]("<trace-file>")
          .optional()
          .action((s, _) => config = config.copy(traceFile = Some(s)))
          .text(
            "The file containing the trace data used as input for the specification." +
              " If this is not provided, input is read from stdin"
          ),
        opt[String]('S', "stop-on")
          .action((s, _) => config = config.copy(stopOn = Some(s)))
          .text("Stop when the output stream with the given name generates its first event"),
        opt[Unit]('r', "reject-undeclared-inputs")
          .action((_, _) => config = config.copy(rejectUndeclaredInputs = true))
          .text("Throw an error if an undeclared input stream occurs in the trace data"),
        opt[BigInt]('a', "abort-at")
          .action((s, _) => config = config.copy(abortAt = Some(s)))
          .text("Stop the interpreter after a given amount of events."),
        opt[Unit]("ctf")
          .action((_, _) => config = config.copy(ctfTrace = true))
          .text(
            "The trace-file with the input data is in CTF format. With this option you must specify " +
              "a trace-file. stdin is not supported."
          ),
        opt[Unit]("csv")
          .action((_, _) => config = config.copy(csvTrace = true))
          .text("The trace-file or the input stream is in CSV format.")
      )
  }

  // Extension for the 'compile-core' command
  {
    import parser._
    var config = CoreConfig()

    note("")
    cmd("compile-core")
      .text("Compile the provided specification to Tessla Core")
      .action((_, _) => tasks += (() => Task("compile-core", config.copy(compilerOptions = compilerOptions))))
      .children(
        arg[File]("<tessla-file>")
          .optional()
          .action((s, _) => config = config.copy(specSource = CharStreams.fromFileName(s.getPath)))
          .text("The file containing the Tessla specification"),
        note(""), // Spacer
        opt[Unit]('c', "print-core")
          .action((_, _) => config = config.copy(printCore = true))
          .text("Print the Tessla Core representation generated from the Tessla specification"),
        opt[Unit]("print-core-lanspec")
          .action((_, _) => {
            config = config.copy(printCoreLanSpec = true)
            compilerOptions = compilerOptions.copy(flattenCore = true)
          })
          .text("Print the Tessla Core representation conform to the language specification."),
        opt[Unit]("print-typed")
          .action((_, _) => config = config.copy(printTyped = true))
          .text("Print the typed Tessla representation generated from the Tessla specification"),
        opt[Unit]("print-locations")
          .action((_, _) => config = config.copy(printLocations = true))
          .text("Print ASTs with locations"),
        opt[Unit]("print-all-types")
          .action((_, _) => config = config.copy(printAllTypes = true))
          .text("Print ASTs with all types"),
        opt[Unit]("list-out-streams")
          .action((_, _) => config = config.copy(listOutStreams = true))
          .text("Print a list of the output streams defined in the given Tessla specification and then exit"),
        opt[Unit]("list-in-streams")
          .action((_, _) => config = config.copy(listInStreams = true))
          .text("Print a list of the input streams defined in the given Tessla specification and then exit"),
        opt[Unit]("observations")
          .action((_, _) => config = config.copy(observations = true))
          .text("Generate observation specification file from the corresponding annotations")
      )
  }

  // Extension for the 'doc' command
  {
    import parser._
    var config = DocConfig()

    note("")
    cmd("doc")
      .text("Generate documentation for Tessla code")
      .action((_, _) => tasks += (() => Task("doc", config)))
      .children(
        opt[Unit]('s', "stdlib")
          .action((_, _) => config = config.copy(stdLib = true))
          .text("Include documentation for definitions from the standard library"),
        opt[Unit]('i', "includes")
          .action((_, _) => config = config.copy(includes = true))
          .text("Include documentation from included files"),
        opt[File]('o', "outfile")
          .action((s, _) => config = config.copy(outfile = Some(s)))
          .text("Write the generated docs to the given file instead of stdout"),
        arg[File]("<files>")
          .optional()
          .unbounded()
          .action((s, _) => config = config.copy(sources = config.sources :+ CharStreams.fromFileName(s.getPath)))
          .text("The TeSSLa files for which to generate documentation")
      )
  }

  def parse(args: Array[String], helpOnEmpty: Boolean = true): (GlobalConfig, List[Task[Config]]) = {
    val a = if (helpOnEmpty && args.isEmpty) Array("--help") else args
    parser.parse(a, ()).getOrElse(sys.exit(1))
    (global, tasks.map(_()).toList)
  }
}

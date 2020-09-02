package de.uni_luebeck.isp.tessla

import java.io.File

import org.antlr.v4.runtime.{CharStream, CharStreams}
import scopt.OptionParser
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers}

import scala.collection.mutable
import scala.io.{Codec, Source}

object CLIParser {
  val programName: String = BuildInfo.name
  val programVersion: String = BuildInfo.version
  val programDescription =
    "Compile Tessla specifications and evaluate them on provided input streams."
  val licenseLocation = "LICENSE"

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
    observations: File = null,
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

  case class TesslacConfig(
    specSource: CharStream = null,
    optimise: Boolean = true,
    outFile: Option[File] = None,
    jarFile: Option[File] = None
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
      .foreach(s => compilerOptions = compilerOptions.copy(baseTimeString = Some(s)))
      .text(
        "Use the given time constant (including a unit) as the reference time for time literals" +
          "(only in 'interpreter' and 'compile-core'"
      )
    opt[File]('s', "stdlib")
      .valueName("<file>")
      .foreach(f =>
        compilerOptions = compilerOptions.copy(
          stdlibPath = f.getPath,
          stdlibIncludeResolver = IncludeResolvers.fromFile
        )
      )
      .text(
        "Provide a standard library to use instead of the default one." +
          "(only in 'interpreter' and 'compile-core'"
      )

    opt[Unit]("no-diagnostics")
      .foreach(_ => global = global.copy(diagnostics = false))
      .text("Suppress error messages and warnings")
    opt[Unit]("debug")
      .foreach(_ => global = global.copy(debug = true))
      .text("Print stack traces for errors and provide more verbose output")
    note("") // Spacer
    help("help")
      .text("Prints this help message and exit.")
    version("version")
      .text("Print the version and exit.")
    opt[Unit]('l', "license")
      .foreach(_ => printLicense())
      .text("Print the legal information for this software and exit.")
  }

  private def printLicense(): Unit = {
    println(Source.fromResource(licenseLocation)(Codec.UTF8).mkString)
    sys.exit(0)
  }

  // Extension for the 'interpreter' command
  {
    import parser._
    var config = InterpreterConfig()

    note("")
    cmd("interpreter")
      .text("Evaluate the given Tessla specification on the input streams provided by a trace file.")
      .foreach(_ => tasks += (() => Task("interpreter", config.copy(compilerOptions = compilerOptions))))
      .children(
        arg[File]("<tessla-file>")
          .optional()
          .foreach(s => config = config.copy(specSource = CharStreams.fromFileName(s.getPath)))
          .text("The file containing the Tessla specification"),
        arg[File]("<trace-file>")
          .optional()
          .foreach(s => config = config.copy(traceFile = Some(s)))
          .text(
            "The file containing the trace data used as input for the specification." +
              " If this is not provided, input is read from stdin"
          ),
        opt[String]('S', "stop-on")
          .foreach(s => config = config.copy(stopOn = Some(s)))
          .text("Stop when the output stream with the given name generates its first event"),
        opt[Unit]('r', "reject-undeclared-inputs")
          .foreach(_ => config = config.copy(rejectUndeclaredInputs = true))
          .text("Throw an error if an undeclared input stream occurs in the trace data"),
        opt[BigInt]('a', "abort-at")
          .foreach(s => config = config.copy(abortAt = Some(s)))
          .text("Stop the interpreter after a given amount of events."),
        opt[Unit]("ctf")
          .foreach(_ => config = config.copy(ctfTrace = true))
          .text(
            "The trace-file with the input data is in CTF format. With this option you must specify " +
              "a trace-file. stdin is not supported."
          ),
        opt[Unit]("csv")
          .foreach(_ => config = config.copy(csvTrace = true))
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
      .foreach(_ => tasks += (() => Task("compile-core", config.copy(compilerOptions = compilerOptions))))
      .children(
        arg[File]("<tessla-file>")
          .foreach(s => config = config.copy(specSource = CharStreams.fromFileName(s.getPath)))
          .text("The file containing the Tessla specification"),
        note(""), // Spacer
        opt[Unit]('c', "print-core")
          .foreach(_ => config = config.copy(printCore = true))
          .text("Print the extended Tessla Core representation generated from the Tessla specification"),
        opt[Unit]("print-core-lanspec")
          .foreach(_ => {
            config = config.copy(printCoreLanSpec = true)
            compilerOptions = compilerOptions.copy(flattenCore = true)
          })
          .text("Print the Tessla Core representation conform to the language specification."),
        opt[Unit]("print-typed")
          .foreach(_ => config = config.copy(printTyped = true))
          .text("Print the typed Tessla representation generated from the Tessla specification"),
        opt[Unit]("print-locations")
          .foreach(_ => config = config.copy(printLocations = true))
          .text("Print ASTs with locations"),
        opt[Unit]("print-all-types")
          .foreach(_ => config = config.copy(printAllTypes = true))
          .text("Print ASTs with all types"),
        opt[Unit]("list-out-streams")
          .foreach(_ => config = config.copy(listOutStreams = true))
          .text("Print a list of the output streams defined in the given Tessla specification and then exit"),
        opt[Unit]("list-in-streams")
          .foreach(_ => config = config.copy(listInStreams = true))
          .text("Print a list of the input streams defined in the given Tessla specification and then exit"),
        opt[File]("observations")
          .foreach(f => config = config.copy(observations = f))
          .text("Instrument the provided C file according to the specification")
      )
  }

  // Extension for the 'doc' command
  {
    import parser._
    var config = DocConfig()

    note("")
    cmd("doc")
      .text("Generate documentation for Tessla code")
      .foreach(_ => tasks += (() => Task("doc", config)))
      .children(
        opt[Unit]('s', "stdlib")
          .foreach(_ => config = config.copy(stdLib = true))
          .text("Include documentation for definitions from the standard library"),
        opt[Unit]('i', "includes")
          .foreach(_ => config = config.copy(includes = true))
          .text("Include documentation from included files"),
        opt[File]('o', "outfile")
          .foreach(s => config = config.copy(outfile = Some(s)))
          .text("Write the generated docs to the given file instead of stdout"),
        arg[File]("<files>")
          .optional()
          .unbounded()
          .foreach(s => config = config.copy(sources = config.sources :+ CharStreams.fromFileName(s.getPath)))
          .text("The TeSSLa files for which to generate documentation")
      )
  }

  // Extension for the 'compile' command
  {
    import parser._
    var config = TesslacConfig()

    note("")
    cmd("compile")
      .foreach(_ => tasks += (() => Task("compile", config)))
      .text("Compile TeSSLa specifications to Scala")
      .children(
        arg[File]("<tessla-file>")
          .foreach(f => config = config.copy(specSource = CharStreams.fromFileName(f.getPath)))
          .text("The file containing the Tessla specification"),
        opt[File]('o', "out-file")
          .foreach(f => config = config.copy(outFile = Some(f)))
          .text("Path to the output file. If not specified source is printed to stdout."),
        opt[File]('j', "jar-file")
          .foreach(f => config = config.copy(jarFile = Some(f)))
          .text(
            "Compiles Scala code to a jar file which is created at the given location. No source output is generated"
          )
      )
  }

  def parse(args: Array[String], helpIfEmpty: Boolean = true): (GlobalConfig, List[Task[Config]]) = {
    val a = if (helpIfEmpty && args.isEmpty) Array("--help") else args
    // handle this separately to suppress parsing errors... can scopt handle this differently?
    if (a.contains("--license") || a.contains("-l")) printLicense()
    parser.parse(a, ()).getOrElse(sys.exit(1))
    (global, tasks.map(_()).toList)
  }
}

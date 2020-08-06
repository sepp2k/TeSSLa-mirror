package de.uni_luebeck.isp.tessla

import java.io.File

import de.uni_luebeck.isp.tessla.Main.{licenseLocation, programDescription, programName, programVersion}
import org.antlr.v4.runtime.{CharStream, CharStreams}
import scopt.OptionParser

import scala.io.Source

object CLIParser {

  sealed trait Mode

  object Mode {
    case object Default extends Mode
    case object Doc extends Mode
  }

  case class DocConfig(
    stdLib: Boolean = false,
    includes: Boolean = false,
    outfile: Option[File] = None,
    sources: Seq[CharStream] = Seq()
  )

  case class Config(
    mode: Mode = Mode.Default,
    specSource: CharStream = null,
    traceFile: Option[File] = None,
    verifyOnly: Boolean = false,
    diagnostics: Boolean = true,
    printCore: Boolean = false,
    printCoreLanSpec: Boolean = false,
    printTyped: Boolean = false,
    printLocations: Boolean = false,
    printAllTypes: Boolean = false,
    debug: Boolean = false,
    stopOn: Option[String] = None,
    rejectUndeclaredInputs: Boolean = false,
    listOutStreams: Boolean = false,
    listInStreams: Boolean = false,
    observations: Boolean = false,
    abortAt: Option[BigInt] = None,
    ctfTrace: Boolean = false,
    csvTrace: Boolean = false,
    doc: DocConfig = DocConfig(),
    compilerOptions: Compiler.Options = Compiler.Options(
      baseTimeString = None,
      includeResolver = IncludeResolvers.fromFile,
      stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
      stdlibPath = "stdlib.tessla",
      flattenCore = false
    )
  )

  private val parser: OptionParser[Config] = new OptionParser[Config](programName) {
    head(s"$programName $programVersion")
    note(programDescription)

    arg[File]("<tessla-file>")
      .optional()
      .action((s, c) => c.copy(specSource = CharStreams.fromFileName(s.getPath)))
      .text("The file containing the Tessla specification")
    arg[File]("<trace-file>")
      .optional()
      .action((s, c) => c.copy(traceFile = Some(s)))
      .text(
        "The file containing the trace data used as input for the specification." +
          " If this is not provided, input is read from stdin"
      )
    opt[String]('S', "stop-on")
      .action((s, c) => c.copy(stopOn = Some(s)))
      .text("Stop when the output stream with the given name generates its first event")
    opt[Unit]('r', "reject-undeclared-inputs")
      .action((_, c) => c.copy(rejectUndeclaredInputs = true))
      .text("Throw an error if an undeclared input stream occurs in the trace data")
    opt[String]('t', "base-time")
      .action((s, c) => c.copy(compilerOptions = c.compilerOptions.copy(baseTimeString = Some(s))))
      .text(
        "Use the given time constant (including a unit) as the reference time for timestamps in the input trace"
      )
    opt[BigInt]('a', "abort-at")
      .action((s, c) => c.copy(abortAt = Some(s)))
      .text("Stop the interpreter after a given amount of events.")
    opt[File]('s', "stdlib")
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
    opt[Unit]('c', "print-core")
      .action((_, c) => c.copy(printCore = true))
      .text("Print the Tessla Core representation generated from the Tessla specification")
    opt[Unit]("print-core-lanspec")
      .action((_, c) =>
        c.copy(
          printCoreLanSpec = true,
          compilerOptions = c.compilerOptions.copy(flattenCore = true)
        )
      )
      .text("Print the Tessla Core representation conform to the language specification.")
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
      .text("Print a list of the output streams defined in the given Tessla specification and then exit")
    opt[Unit]("list-in-streams")
      .action((_, c) => c.copy(listInStreams = true))
      .text("Print a list of the input streams defined in the given Tessla specification and then exit")
    opt[Unit]("observations")
      .action((_, c) => c.copy(observations = true))
      .text("Generate observation specification file from the corresponding annotations")
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
    opt[Unit]('l', "license")
      .action((_, _) => {
        println(Source.fromResource(licenseLocation).mkString)
        sys.exit(0)
      })
      .text("Print the legal information for this software and exit.")

    // Final validation
    checkConfig(c =>
      c.mode match {
        case Mode.Default =>
          if (c.specSource == null) failure("No Tessla specification provided.")
          else if (c.ctfTrace && c.traceFile.isEmpty) failure("No CTF trace input given.")
          else success
        case Mode.Doc => success
      }
    )
  }

  // Extension for the 'doc' command
  {
    import parser._
    def cb(c: Config, r: DocConfig => DocConfig): Config = c.copy(doc = r(c.doc))

    note("")
    cmd("doc")
      .text("Generate documentation for TeSSLa code")
      .action((_, c) => c.copy(mode = Mode.Doc))
      .children(
        opt[Unit]('s', "stdlib")
          .action((_, c) => cb(c, _.copy(stdLib = true)))
          .text("Include documentation for definitions from the standard library"),
        opt[Unit]('i', "includes")
          .action((_, c) => cb(c, _.copy(includes = true)))
          .text("Include documentation from included files"),
        opt[File]('o', "outfile")
          .action((s, c) => cb(c, _.copy(outfile = Some(s))))
          .text("Write the generated docs to the given file instead of stdout"),
        arg[File]("<files>")
          .optional()
          .unbounded()
          .action((s, c) => cb(c, d => d.copy(sources = d.sources :+ CharStreams.fromFileName(s.getPath))))
          .text("The TeSSLa files for which to generate documentation")
      )
  }

  def parse(args: Array[String]): Config = parser.parse(args, Config()).getOrElse(sys.exit(1))
}

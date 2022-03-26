/*
 * Copyright 2022 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers}
import de.uni_luebeck.isp.tessla.tessla_compiler.CompilerStdLibIncludeResolver
import org.antlr.v4.runtime.{CharStream, CharStreams}
import scopt.OptionParser

import java.io.File
import java.nio.file.{Path, Paths}
import scala.collection.mutable
import scala.io.{Codec, Source}

/**
 * Parser of the command line interface of the Tessla front-end. This also generates the different configuration
 * data structures for each of the existing commands.
 */
object CLIParser {
  val programName: String = BuildInfo.name
  val programVersion: String = BuildInfo.version
  val programDescription =
    "Compile Tessla specifications and evaluate them on provided input streams."
  val licenseLocation = "LICENSE_NOTE"

  sealed trait Config

  case class DocConfig(
    stdLib: Boolean = false,
    includes: Boolean = false,
    outfile: Option[File] = None,
    sources: Seq[CharStream] = Seq(),
    compilerOptions: Compiler.Options = Compiler.Options()
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
    exportAnnotations: Option[File] = None,
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

  case class TesslacScalaConfig(
    specSource: CharStream = null,
    optimise: Boolean = true,
    outFile: Option[File] = None,
    jarFile: Option[File] = None,
    additionalSource: String = "",
    ioInterface: Boolean = true,
    compilerOptions: Compiler.Options = Compiler.Options()
  ) extends Config

  case class TesslacRustConfig(
    specSource: CharStream = null,
    exportBinary: Option[Path] = None,
    exportWorkspace: Option[Path] = None,
    generateMain: Boolean = false,
    additionalSource: String = "",
    compilerOptions: Compiler.Options = Compiler.Options()
  ) extends Config

  case class InstrumenterConfig(
    specSource: CharStream = null,
    cFile: File = null,
    includes: Seq[File] = Seq(),
    compilerOptions: Compiler.Options = Compiler.Options()
  ) extends Config

  case class GlobalConfig(
    userStdLib: Boolean = false,
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
      .text("Use the given time constant (including a unit) as the reference time for time literals")
    opt[File]('s', "stdlib")
      .valueName("<file>")
      .foreach(f => {
        global = global.copy(userStdLib = true)
        compilerOptions = compilerOptions.copy(
          stdlibPath = f.getName,
          stdlibIncludeResolver = (s: String) => IncludeResolvers.fromFile(f.toPath.resolveSibling(s).toString)
        )
      })
      .text("Provide a standard library to use instead of the default one.")

    opt[Unit]("no-diagnostics")
      .foreach(_ => global = global.copy(diagnostics = false))
      .text("Suppress error messages and warnings")
    opt[Unit]("debug")
      .foreach(_ => global = global.copy(debug = true))
      .text("Print stack traces for errors and provide more verbose output")
    note("") // Spacer
    help("help")
      .abbr("h")
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
        opt[File]('a', "export-annotations")
          .foreach(f => config = config.copy(exportAnnotations = Some(f)))
          .text("Generate a Json file containing the annotation information of the specification")
      )
  }

  // Extension for the 'doc' command
  {
    import parser._
    var config = DocConfig()

    note("")
    cmd("doc")
      .text("Generate documentation for Tessla code")
      .foreach(_ => tasks += (() => Task("doc", config.copy(compilerOptions = compilerOptions))))
      .children(
        opt[Unit]("include-stdlib")
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

  private def getFileContent(file: File): String = {
    val addIncludeFile = Source.fromFile(file)
    try addIncludeFile.mkString
    finally addIncludeFile.close
  }

  // Extension for the 'compile-scala' command
  {
    import parser._
    var config = TesslacScalaConfig()

    compilerOptions =
      if (!global.userStdLib)
        compilerOptions.copy(stdlibIncludeResolver = CompilerStdLibIncludeResolver.fromCompilerStdlibResource)
      else
        compilerOptions

    note("")
    cmd("compile-scala")
      .foreach(_ => tasks += (() => Task("compile-scala", config.copy(compilerOptions = compilerOptions))))
      .text("Compile TeSSLa specifications to Scala")
      .children(
        arg[File]("<tessla-file>")
          .foreach(f => config = config.copy(specSource = CharStreams.fromFileName(f.getPath)))
          .text("The file containing the Tessla specification"),
        opt[File]('a', "add-source")
          .foreach(f => config = config.copy(additionalSource = getFileContent(f)))
          .text("Additional source file included on top of the generated source"),
        opt[File]('o', "out-file")
          .foreach(f => config = config.copy(outFile = Some(f)))
          .text("Place the generated Scala source code at this location."),
        opt[File]('j', "jar-file")
          .foreach(f => config = config.copy(jarFile = Some(f)))
          .text("Compile TeSSLa specification to an executable jar file which is created at the given location."),
        opt[Unit]('n', "no-io")
          .foreach(_ => config = config.copy(ioInterface = false))
          .text("Replaces I/O Handling in generated source with simple API interface")
      )
  }

  // Extension for the 'compile-rust' command
  {
    import parser._
    var config = TesslacRustConfig()

    compilerOptions =
      if (!global.userStdLib)
        compilerOptions.copy(stdlibIncludeResolver = CompilerStdLibIncludeResolver.fromCompilerStdlibResource)
      else
        compilerOptions

    note("")
    cmd("compile-rust")
      .foreach(_ => tasks += (() => Task("compile-rust", config.copy(compilerOptions = compilerOptions))))
      .text("Compile TeSSLa specifications to Rust")
      .children(
        arg[File]("<tessla-file>")
          .foreach(f => config = config.copy(specSource = CharStreams.fromFileName(f.getPath)))
          .text("The file containing the Tessla specification"),
        opt[File]('b', "bin-file")
          .foreach(f => config = config.copy(exportBinary = Some(Paths.get(f.getAbsolutePath))))
          .text("Compile a full monitor binary and place it in the given location"),
        opt[File]('a', "add-source")
          .foreach(f => config = config.copy(additionalSource = getFileContent(f)))
          .text("Additional rust file inserted at the top of the monitor library"),
        opt[File]('p', "project-dir")
          .foreach {
            case f if f.isFile => failure("must be a directory")
            case f             => config = config.copy(exportWorkspace = Some(Paths.get(f.getAbsolutePath)))
          }
          .text("Export a Cargo workspace with everything necessary to modify and build the tessla monitor yourself"),
        opt[Unit]('m', "io-interface")
          .foreach(_ => config = config.copy(generateMain = true))
          .text("Generate the code for an executable I/O interface (src/main.rs) in the exported workspace"),
        checkConfig(_ =>
          if (config.exportBinary.isEmpty && config.exportWorkspace.isEmpty)
            failure("You must at least specify one of the following two options: --bin-file, --project-dir")
          else success
        )
      )
  }

  // Extension for the 'instrumenter' command
  {
    import parser._
    var config = InstrumenterConfig()

    note("")
    cmd("instrumenter")
      .foreach(_ => tasks += (() => Task("instrumenter", config.copy(compilerOptions = compilerOptions))))
      .text("Instrument C code based on the provided annotations (linux-amd64, windows-x64 only)")
      .children(
        arg[File]("<tessla-file>")
          .foreach(f => config = config.copy(specSource = CharStreams.fromFileName(f.getPath)))
          .text("The file containing the Tessla specification, with annotations for the instrumentation."),
        arg[File]("<c-file>")
          .foreach(f => config = config.copy(cFile = f))
          .text("Instrument the provided C file according to the specification"),
        arg[File]("<include-path>...")
          .optional()
          .unbounded()
          .foreach(f => config = config.copy(includes = config.includes :+ f))
          .text("Include paths for the C compiler")
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

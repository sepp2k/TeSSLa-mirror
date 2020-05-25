package de.uni_luebeck.isp.tessla.tessladoc

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.{CharStream, CharStreams}
import de.uni_luebeck.isp.tessla.{BuildInfo, IncludeResolvers}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import de.uni_luebeck.isp.tessla.tessladoc.TesslaDoc.DefDoc
import de.uni_luebeck.isp.tessla.util._
import scopt.OptionParser

object Main {
  val programName = "tessladoc"
  val programVersion = BuildInfo.version
  val programDescription = "Generate documentation for TeSSLa code"

  case class Config(
    stdLib: Boolean = false,
    includes: Boolean = false,
    globalsOnly: Boolean = false,
    outfile: Option[File] = None,
    sources: Seq[CharStream] = Seq()
  )

  val parser: OptionParser[Config] = new OptionParser[Config](programName) {
    head(s"$programName $programVersion")
    note(programDescription)
    opt[Unit]('s', "stdlib")
      .action((_, c) => c.copy(stdLib = true))
      .text("Include documentation for definitions from the standard library")
    opt[Unit]('i', "includes")
      .action((_, c) => c.copy(includes = true))
      .text("Include documentation from included files")
    opt[File]('o', "outfile")
      .action((s, c) => c.copy(outfile = Some(s)))
      .text("Write the generated docs to the given file instead of stdout")
    arg[File]("<files>")
      .optional()
      .unbounded()
      .action((s, c) => c.copy(sources = c.sources :+ CharStreams.fromFileName(s.getPath)))
      .text("The TeSSLa files for which to generate documentation")
    note("") // Spacer
    help("help")
      .text("Prints this help message and exit.")
    version("version")
      .text("Print the version and exit.")
  }

  def main(args: Array[String]): Unit = {
    val config = parser.parse(args, Config()).getOrElse(sys.exit(1))
    val includeResolver = optionIf(config.includes)(IncludeResolvers.fromFile _)
    val output =
      TesslaDoc.extract(config.sources, includeResolver, includeStdlib = config.stdLib) match {
        case Success(tesslaDocs, warnings) =>
          warnings.foreach(w => System.err.println(s"Warning: $w"))
          tesslaDocs.globalsOnly.toString
        case Failure(errors, warnings) =>
          warnings.foreach(w => System.err.println(s"Warning: $w"))
          errors.foreach(e => System.err.println(s"Error: $e"))
          System.err.println(
            s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors"
          )
          sys.exit(1)
      }
    config.outfile match {
      case Some(file) =>
        try {
          Files.write(file.toPath, output.getBytes("UTF-8"))
        } catch {
          case ex: IOException =>
            System.err.println(s"Could not write to output file: $ex")
            sys.exit(2)
        }
      case None =>
        println(output)
    }
  }
}

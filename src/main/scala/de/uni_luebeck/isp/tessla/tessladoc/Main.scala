package de.uni_luebeck.isp.tessla.tessladoc

import java.io.IOException
import java.nio.file.{Files, Paths}
import org.antlr.v4.runtime.CharStreams
import sexyopt.SexyOpt
import de.uni_luebeck.isp.tessla.{BuildInfo, IncludeResolvers}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import de.uni_luebeck.isp.tessla.util._

object Main extends SexyOpt {
  override def programName = "tessladoc"
  override val version = Some(BuildInfo.version)
  override val programDescription = "Generate documentation for TeSSLa code"

  val files = restArgs("files", "The TeSSLa files for which to generate documentation", atLeastOne = false)
  val stdLib = flag("stdlib", 's', "Include documentation for definitions from the standard library")
  val includes = flag("includes", 'i', "Include documentation from included files")
  val globalsOnly = flag("globals-only", 'g', "Do not show information for local definitions")
  val markdown = flag("markdown", 'm', "Generate documentation as markdown")
  val outFile = option("outfile", 'o', "Write the generated docs to the given file instead of stdout")

  def main(args: Array[String]): Unit = {
    parse(args)
    val streams = files.map(CharStreams.fromFileName)
    val includeResolver = optionIf(includes)(IncludeResolvers.fromFile _)
    val output = TesslaDoc.extract(streams.toSeq, includeResolver, includeStdlib = stdLib) match {
      case Success(tesslaDocs, warnings) =>
        warnings.foreach(w => System.err.println(s"Warning: $w"))
        val relevantDocs = if(globalsOnly) tesslaDocs.globalsOnly else tesslaDocs
        if (markdown) {
          val generator = new MarkdownGenerator(relevantDocs)
          generator.generateMarkdown
        } else {
          relevantDocs.toString
        }
      case Failure(errors, warnings) =>
        warnings.foreach(w => System.err.println(s"Warning: $w"))
        errors.foreach(e => System.err.println(s"Error: $e"))
        System.err.println(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors")
        sys.exit(1)
    }
    outFile.value match {
      case Some(file) =>
        try {
          Files.write(Paths.get(file), output.getBytes("UTF-8"))
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

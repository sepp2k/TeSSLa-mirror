package de.uni_luebeck.isp.tessla.tessladoc

import de.uni_luebeck.isp.tessla.BuildInfo
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import org.antlr.v4.runtime.CharStreams
import sexyopt.SexyOpt

object Main extends SexyOpt {
  override def programName = "tessladoc"
  override val version = Some(BuildInfo.version)
  override val programDescription = "Generate documentation for TeSSLa code"

  val files = restArgs("file", "The TeSSLa files for which to generate documentation", atLeastOne = false)
  //val stdLib = flag("stdlib", 's', "Include documentation for definitions from the standard library")
  val includes = flag("includes", 'i', "Include documentation from included files")

  def main(args: Array[String]) = {
    parse(args)
    val streams = files.map(CharStreams.fromFileName)
    TesslaDoc.extractAsJSON(streams, currentFileOnly = !includes) match {
      case Success(tesslaDocs, warnings) =>
        warnings.foreach(w => System.err.println(s"Warning: $w"))
        println(tesslaDocs)
      case Failure(errors, warnings) =>
        warnings.foreach(w => System.err.println(s"Warning: $w"))
        errors.foreach(e => System.err.println(s"Error: $e"))
        System.err.println(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors")
        sys.exit(1)
    }
  }
}

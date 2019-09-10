package de.uni_luebeck.isp.tessla.tessla_compiler

import sexyopt.SexyOpt
import de.uni_luebeck.isp.tessla.{Compiler, IncludeResolvers}
import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import org.antlr.v4.runtime.CharStreams

/**
 * Main class of the project launching the translation and parsing command line
 * options
 */
object Main extends SexyOpt {
  override val programName = ""
  override val version = Some("0.0.1")
  override val programDescription = "Generate Java/Rust code from a TeSSLa specification"

  val tesslaFile = posArg("tessla-file", "The file containing the Tessla specification")
  val debug = flag("debug", "Print stack traces for runtime errors")
  val noDiagnostics = flag("no-diagnostics", "Don't print error messages and warnings")
  val noOptimization = flag("no-optimization", "Produce non-optimized output code")
  val bareCode = flag("bare-code", "Generates Code where no stdin/stdout is used. May not " +
                                   "be used without --rust argument.")
  val translateToRust = flag("rust", "Compile to Rust instead of Java")
  def diagnostics = !noDiagnostics.value

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
    try {
        val specSource = CharStreams.fromFileName(tesslaFile)
        val compilerOptions = Compiler.Options(
          timeUnitString = None,
          includeResolver = IncludeResolvers.fromFile,
          stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
          stdlibPath = "Predef.tessla",
          currySignalLift = true
        )
        val core = unwrapResult(Compiler.compile(specSource, compilerOptions))
        val intermediateCode = unwrapResult(TesslaCoreToIntermediate.translate(core))
        println(intermediateCode)

    } catch {
      case ex: TesslaError =>
        System.err.println(s"Runtime error: $ex")
        if (debug) ex.printStackTrace()
    }
  }
}

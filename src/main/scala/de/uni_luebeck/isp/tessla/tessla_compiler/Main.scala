package de.uni_luebeck.isp.tessla.tessla_compiler

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path}

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.IncludeResolvers
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend.{ScalaBackend, ScalaCompiler}
import de.uni_luebeck.isp.tessla.Compiler
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{LazynessAnalysis, UsageAnalysis}
import org.antlr.v4.runtime.CharStreams
import sexyopt.SexyOpt

/**
 * Main class of the project launching the translation and parsing command line options
 */

object Main extends SexyOpt {
  override val programName = "tessla-compiler"
  override val version = Some("1.0.1")
  override val programDescription = "Generate Java/Rust code from a TeSSLa specification"

  val tesslaFile = posArg("tessla-file", "The file containing the Tessla specification")
  val target = option("target", 't', "Target language: scala (default), javascript, rust or rust-bare", "scala")
  val debug = flag("debug", "Print stack traces for runtime errors")
  val noDiagnostics = flag("no-diagnostics", "Don't print error messages and warnings")
  val noOptimization = flag("no-optimization", "Produce non-optimized output code")
  val outputPath = option("output-file", 'o', "Location of the output (including filename)")
  val jarPath = option(
    "jar-file",
    'j',
    "Compiles Scala code to a jar file which is created at the given location. No source output is generated"
  )
  val verbose = flag("verbose", 'v', "Produce a lot of output")

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
        baseTimeString = None,
        includeResolver = IncludeResolvers.fromFile,
        stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
        stdlibPath = "stdlib.tessla",
        flattenCore = false
      )
      val (backend, stdinRead): (backends.BackendInterface, Boolean) = target.value match {
        case "scala"      => (new ScalaBackend, true)
        case "javascript" => throw Diagnostics.NotYetImplementedError("Javascript translation not implemented yet")
        case "rust"       => throw Diagnostics.NotYetImplementedError("Rust translation not implemented yet")
        case "rust-bare"  => throw Diagnostics.NotYetImplementedError("Bare metal Rust translation not implemented yet")
        case _            => throw Diagnostics.CLIError(s"Unvalid option for target: ${target.value}")
      }

      val compiler = new Compiler(compilerOptions)
      val unflatCore = unwrapResult(compiler.tesslaToTyped(specSource).andThen(compiler.typedToCore))
      //val unflatCore = unwrapResult((new Compiler).compile(specSource, compilerOptions))
      val core = unwrapResult(
        (new UsageAnalysis)
          .translate(unflatCore)
          .andThen(new LazynessAnalysis)
      )

      if (verbose.value) {
        println("###############################")
        println("#        TeSSLa Core          #")
        println("###############################")
        println(core.spec)
        println("###############################")
      }

      val intermediateCode = unwrapResult((new TesslaCoreToIntermediate(stdinRead)).translate(core))
      val optIntermediateCode = unwrapResult(UnusedVarRemove.translate(intermediateCode))

      if (verbose.value) {
        println("###############################")
        println("#      Intermediate Code      #")
        println("###############################")
        println(optIntermediateCode)
        println("###############################")
      }

      val source = backend.translate(optIntermediateCode)
      val sourceStr = unwrapResult(source)

      if (jarPath.isDefined) {
        val p = Path.of(jarPath.get).toAbsolutePath
        val isDir = Files.exists(p) && Files.isDirectory(p)
        source.andThen(new ScalaCompiler(if (isDir) p else p.getParent, if (isDir) "monitor.jar" else p.toFile.getName))
      } else if (outputPath.isDefined) {
        val pw = new PrintWriter(new File(outputPath.get))
        pw.write(sourceStr)
        pw.close()
      } else {
        println(sourceStr)
      }
    } catch {
      case ex: TesslaError =>
        System.err.println(s"Compilation error: $ex")
        if (debug) ex.printStackTrace()
    }
  }

  def diagnostics = !noDiagnostics.value

}

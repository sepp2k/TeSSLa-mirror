package de.uni_luebeck.isp.tessla.tessla_compiler

import java.io.{File, PrintWriter}

import sexyopt.SexyOpt
import de.uni_luebeck.isp.tessla.{Compiler, IncludeResolvers}
import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.{MutabilityChecker, TesslaCoreWithMutabilityInfo}
import org.antlr.v4.runtime.CharStreams

/**
 * Main class of the project launching the translation and parsing command line
 * options
 */
object Main extends SexyOpt {
  override val programName = "tessla-compiler"
  override val version = Some("0.0.1")
  override val programDescription = "Generate Java/Rust code from a TeSSLa specification"

  val tesslaFile = posArg("tessla-file", "The file containing the Tessla specification")
  val target = option("target", 't', "Target language: java (default), javascript, rust or rust-bare", "java")
  val debug = flag("debug", "Print stack traces for runtime errors")
  val noDiagnostics = flag("no-diagnostics", "Don't print error messages and warnings")
  val noOptimization = flag("no-optimization", "Produce non-optimized output code")
  val noMutability = flag("no-mutability", "Produce code with exclusively immutable datastructures")
  val outputPath = option("output-file", 'o', "Location of the output (including filename)")
  val verbose = flag("verbose", 'v', "Produce a lot of output")

  def diagnostics = !noDiagnostics.value
  def mutability = !noOptimization.value && !noMutability.value

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
          stdlibPath = "stdlib.tessla"
        )
        val (backend, stdinRead) : (backends.BackendInterface, Boolean) = target.value match {
          case "java" => (new backends.JavaBackend, true)
          case "javascript" => throw new Errors.NotYetImplementedError("Javascript translation not implemented yet")
          case "rust" => throw new Errors.NotYetImplementedError("Rust translation not implemented yet")
          case "rust-bare" => throw new Errors.NotYetImplementedError("Bare metal Rust translation not implemented yet")
          case _=> throw new Errors.CLIError(s"Unvalid option for target: ${target.value}")
        }

        val unflatCore = unwrapResult(unwrapResult((new Compiler).compile(specSource, compilerOptions))._2)
        val core = unwrapResult((new StreamDefFlattener).translate(unflatCore))

        if (verbose.value) {
          println("###############################")
          println("#        TeSSLa Core          #")
          println("###############################")
          println(core)
          println("###############################")
        }

        val coreWithMutInf = if (mutability) {
          unwrapResult(MutabilityChecker.translate(core))
        } else {
          new TesslaCoreWithMutabilityInfo(core, Set())
        }
        val intermediateCode = unwrapResult((new TesslaCoreToIntermediate(stdinRead)).translate(coreWithMutInf))

        if (verbose.value) {
          println("###############################")
          println("#      Intermediate Code      #")
          println("###############################")
          println(intermediateCode)
          println("###############################")
        }

        val source = unwrapResult(backend.translate(intermediateCode))

        if (outputPath.get != "") {
          val pw = new PrintWriter(new File(outputPath.get))
          pw.write(source)
          pw.close
        } else {
          println(source)
        }
    } catch {
      case ex: TesslaError =>
        System.err.println(s"Compilation error: $ex")
        if (debug) ex.printStackTrace()
    }
  }
}

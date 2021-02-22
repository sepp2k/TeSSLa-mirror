package de.uni_luebeck.isp.tessla.tessla_compiler

import java.io.{File, PrintWriter}
import de.uni_luebeck.isp.tessla.{Compiler, IncludeResolvers}
import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend.ScalaBackend
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{
  ASTPreprocessor,
  ASTRemoveUnused,
  StreamDefFlattener
}
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check._
import org.antlr.v4.runtime.CharStreams
import sexyopt.SexyOpt

/**
  * Main class of the project launching the translation and parsing command line
  * options
  */
object Main extends SexyOpt {
  override val programName = "tessla-compiler"
  override val version: Option[String] = Some("0.1.0-SNAPSHOT")
  override val programDescription =
    "Generate Java/Rust code from a TeSSLa specification"

  val tesslaFile: Main.Argument[String] =
    posArg("tessla-file", "The file containing the Tessla specification")
  val debug: Main.Argument[Boolean] =
    flag("debug", "Print stack traces for runtime errors")

  val noMutability: Main.Argument[Boolean] = flag(
    "no-mutability",
    "Produce code with exclusively immutable datastructures"
  )
  val outputPath: Main.Argument[Option[String]] =
    option("output-file", 'o', "Location of the output (including filename)")
  val verbose: Main.Argument[Boolean] =
    flag("verbose", 'v', "Produce a lot of output")

  val userCode: Main.Argument[Option[String]] =
    option("user-include", 'u', "User-specific code to be included"
  )

  def mutability: Boolean = !noMutability.value

  def main(args: Array[String]): Unit = {

    def unwrapResult[T](result: Result[T]): T = result match {
      case Success(res, warnings) =>
        warnings.foreach(w => System.err.println(s"Warning: $w"))
        res
      case Failure(errors, warnings) =>
          warnings.foreach(w => System.err.println(s"Warning: $w"))
          errors.foreach { e =>
            System.err.println(s"Error: $e")
            if (debug) e.printStackTrace()
          }
          System.err.println(
            s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors"
          )
        sys.exit(1)
    }

    parse(args)
    try {
      val specSource = CharStreams.fromFileName(tesslaFile)
      val compilerOptions = Compiler.Options(
        baseTimeString = None,
        includeResolver = IncludeResolvers.fromFile,
        stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
        stdlibPath = "stdlib.tessla"
      )

      val (backend, stdinRead): (backends.BackendInterface, Boolean) = (new ScalaBackend(userCode.getOrElse("")), true)

        val unflatCore = unwrapResult(unwrapResult((new Compiler).compile(specSource, compilerOptions))._2)
        val core = unwrapResult((new StreamDefFlattener).translate(unflatCore).andThen(new ASTPreprocessor(mutability)).andThen(new ASTRemoveUnused))

        if (verbose.value) {
          println("###############################")
          println("#        TeSSLa Core          #")
          println("###############################")
          println(core)
          println("###############################")
        }

        val coreWithMutInf = if (mutability) {
          unwrapResult(MutabilityChecker.translate(core).andThen(ASTTransformation))
        } else {
          new TesslaCoreWithMutabilityInfo(
                    core,
                    (id, idMap: Map[Identifier, DefinitionExpression]) => idMap(id).tpe,
                    Map(),
                    new ImplicationChecker(core)
                  )
        }

      //println(coreWithMutInf)

      val optIntermediateCode = unwrapResult(
        (new TesslaCoreToIntermediate(stdinRead))
          .translate(coreWithMutInf)
          .andThen(UnusedVarRemove)
      )

      if (verbose.value) {
        println("###############################")
        println("#      Intermediate Code      #")
        println("###############################")
        println(optIntermediateCode)
        println("###############################")
      }

      val source = unwrapResult(backend.translate(optIntermediateCode))

      if (outputPath.isDefined) {
        val pw = new PrintWriter(new File(outputPath.get))
        pw.write(source)
        pw.close()
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

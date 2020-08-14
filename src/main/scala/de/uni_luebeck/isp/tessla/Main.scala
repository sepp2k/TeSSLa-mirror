package de.uni_luebeck.isp.tessla

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import de.uni_luebeck.isp.tessla.CLIParser.{Config, DocConfig, Task}
import de.uni_luebeck.isp.tessla.core.Errors.TesslaError
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.core.analyses.Observations
import de.uni_luebeck.isp.tessla.interpreter._
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers, TesslaAST}
import de.uni_luebeck.isp.tessla.tessla_compiler.{TesslaCoreToIntermediate, UnusedVarRemove}
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend.{ScalaBackend, ScalaCompiler}
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{Laziness, UsageAnalysis}
import de.uni_luebeck.isp.tessla.tessladoc.TesslaDoc

import scala.io.Source

/**
 * Entry point of the application.
 * Depending on the provided arguments, the according task is executed.
 */
object Main {

  def main(args: Array[String]): Unit = {
    val (global, tasks) = CLIParser.parse(args)
    new Application(global, tasks).run()
  }

  /**
   * Contains the different running modes.
   *
    * @param global the global, task-independent configuration settings
   * @param tasks the tasks to be executed
   */
  class Application(global: CLIParser.GlobalConfig, tasks: List[Task[Config]]) {

    /**
     * Decides which mode to run in depending on the configuration provided.
     */
    def run(): Unit = {
      try {
        tasks.foreach {
          case Task(_, config: CLIParser.DocConfig)         => runDoc(config)
          case Task(_, config: CLIParser.CoreConfig)        => runCore(config)
          case Task(_, config: CLIParser.InterpreterConfig) => runInterpreter(config)
          case Task(_, config: CLIParser.TesslacConfig) => runTesslaCompiler(config)
        }
      } catch {
        case ex: TesslaError =>
          printErr(s"Runtime error: $ex")
          if (global.debug) ex.printStackTrace()
        case ex: IOException =>
          printErr(s"IO Error: ${ex.getMessage}")
          if (global.debug) ex.printStackTrace()
      }

    }
    /**
     * Generate documentation.
     *
      * This mode parses the input, then extracts and processes the documentation strings
     * from each definition. The result is either printed to stdout or to a file, depending on the configuration.
     *
      * @see See [[TesslaDoc.extract]] for more
     */
    def runDoc(docConfig: DocConfig): Unit = {
      val includeResolver = Option.when(docConfig.includes)(IncludeResolvers.fromFile _)
      val output = unwrapResult(
        TesslaDoc.extract(docConfig.sources, includeResolver, includeStdlib = docConfig.stdLib)
      ).toString

      docConfig.outfile match {
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

    /**
     * Runs the [[Compiler]] on the provided specification and prints the resulting Tessla-Core code,
     * depending on the configuration.
     */
    def runCore(config: CLIParser.CoreConfig): Unit = {
      val compiler = new Compiler(config.compilerOptions)
      val typed = unwrapResult(compiler.tesslaToTyped(config.specSource))
      val core = unwrapResult(compiler.typedToCore(typed))
      lazy val flatCore = unwrapResult(compiler.coreToFlatCore(core))
      val printOptions = TesslaAST.PrintOptions(
        !config.printAllTypes,
        config.printAllTypes,
        config.printAllTypes,
        paramTypes = true,
        config.printLocations
      )

      // All those options are mutually exclusive, only apply the first one in this list
      LazyList(
        if (config.printCore) println(core.print(printOptions)),
        if (config.printCoreLanSpec) println(flatCore.print(printOptions)),
        if (config.printTyped) println(typed.print(printOptions)),
        if (config.observations) println(unwrapResult(Observations.Generator.translate(core))),
        if (config.listInStreams) core.in.foreach(is => println(is._1.idOrName)),
        if (config.listOutStreams) core.out.foreach(os => println(os._1.id.idOrName))
      ).headOption

    }

    /**
     * Runs the [[Interpreter]] on the provided specification and trace, or uses the stdin if no trace is provided.
     */
    def runInterpreter(config: CLIParser.InterpreterConfig): Unit = {
      val core = unwrapResult(Compiler.compile(config.specSource, config.compilerOptions))

      val trace = if (config.ctfTrace) {
        config.traceFile
          .map(Trace.fromCtfFile(_, config.abortAt))
          .getOrElse {
            printErr("No CTF trace file provided.")
            sys.exit(1)
          }
      } else if (config.csvTrace) {
        config.traceFile
          .map(Trace.fromCsvFile(_, config.abortAt))
          .getOrElse(Trace.fromCsvSource(Source.stdin, "<stdin>", config.abortAt))
      } else {
        config.traceFile
          .map(Trace.fromFile(_, config.abortAt))
          .getOrElse(Trace.fromSource(Source.stdin, "<stdin>", config.abortAt))
      }
      val output = Interpreter.run(core, trace, config.stopOn, config.rejectUndeclaredInputs)
      output.foreach(println)

    }

    def runTesslaCompiler(config: CLIParser.TesslacConfig): Unit = {
      try {
        val compilerOptions = Compiler.Options()
        val (backend, stdinRead) = (new ScalaBackend, true)

        val core = unwrapResult(
          Compiler.compile(config.specSource, compilerOptions)
          andThen UsageAnalysis
          andThen Laziness
        )

        if (global.debug) {
          println("###############################")
          println("#        TeSSLa Core          #")
          println("###############################")
          println(core.spec)
          println("###############################")
        }

        val optIntermediateCode = unwrapResult(
          new TesslaCoreToIntermediate(stdinRead)(core)
          andThen UnusedVarRemove
        )

        if (global.debug) {
          println("###############################")
          println("#      Intermediate Code      #")
          println("###############################")
          println(optIntermediateCode)
          println("###############################")
        }

        val source = backend.translate(optIntermediateCode)
        val sourceStr = unwrapResult(source)

        config.jarFile.map{ file =>
        val p = file.toPath
          val (dirPath, name) = if (file.isDirectory) {
            (p, "monitor.jar")
          } else {
            val n = file.getName
            (p.getParent, if (n.contains(".")) n else s"$n.jar")
          }
          new ScalaCompiler(dirPath, name, false)().translate(sourceStr)
        }

        config.outFile match {
          case Some(f) =>
            Files.createDirectories(f.toPath.getParent)
            Files.write(f.toPath, sourceStr.getBytes(StandardCharsets.UTF_8))
          case None => println(sourceStr)
        }

      } catch {
        case ex: TesslaError =>
          System.err.println(s"Compilation error: $ex")
          if (global.debug) ex.printStackTrace()
      }
    }

    private def unwrapResult[T](result: Result[T]): T = result match {
      case Success(res, warnings) =>
        if (global.diagnostics) warnings.foreach(w => printErr(s"Warning: $w"))
        res
      case Failure(errors, warnings) =>
        if (global.diagnostics) {
          warnings.foreach(w => printErr(s"Warning: $w"))
          errors.foreach { e =>
            printErr(s"Error: $e")
            if (global.debug) e.printStackTrace()
          }
          printErr(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors")
        }
        sys.exit(1)
    }

    private def println(a: Any): Unit = {
      if (System.out.checkError()) System.exit(141)
      System.out.println(a)
    }

    private def printErr(a: Any): Unit = {
      if (System.err.checkError()) System.exit(141)
      System.err.println(a)
    }
  }

}

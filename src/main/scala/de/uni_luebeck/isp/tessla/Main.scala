package de.uni_luebeck.isp.tessla

import java.io.IOException
import java.nio.file.Files

import de.uni_luebeck.isp.tessla.CLIParser.Mode
import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import de.uni_luebeck.isp.tessla.analyses.Observations
import de.uni_luebeck.isp.tessla.interpreter._
import de.uni_luebeck.isp.tessla.tessladoc.TesslaDoc
import de.uni_luebeck.isp.tessla.util._

import scala.io.Source

object Main {
  val programName: String = BuildInfo.name
  val programVersion: String = BuildInfo.version
  val programDescription =
    "Evaluate the given Tessla specification on the input streams provided by the given trace file."
  val licenseLocation = "de/uni_luebeck/isp/tessla/License"

  def main(args: Array[String]): Unit = {
    val config = CLIParser.parse(args)
    new Application(config).run()
  }

  class Application(config: CLIParser.Config) {

    def run(): Unit = {
      config.mode match {
        case Mode.Doc     => runDoc()
        case Mode.Default => runCompiler()
      }
    }

    def runDoc(): Unit = {
      val docCfg = config.doc
      val includeResolver = optionIf(docCfg.includes)(IncludeResolvers.fromFile _)
      val output = unwrapResult(
        TesslaDoc.extract(docCfg.sources, includeResolver, includeStdlib = docCfg.stdLib)
      ).globalsOnly.toString

      docCfg.outfile match {
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

    def runCompiler(): Unit = {
      val compiler = new Compiler(config.compilerOptions)
      try {
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

        if (config.printTyped) {
          println(typed.print(printOptions))
          return
        }

        if (config.printCoreLanSpec) {
          println(flatCore.print(printOptions))
          return
        }

        if (config.printCore) {
          println(core.print(printOptions))
          return
        }

        if (config.observations) {
          println(unwrapResult(Observations.Generator.translate(core)))
          return
        }

        if (config.listInStreams) {
          core.in.foreach { is => println(is._1.idOrName) }
          return
        }

        if (config.listOutStreams) {
          core.out.foreach { os => println(os._1.id.idOrName) }
          return
        }

        if (config.verifyOnly) {
          return
        }

        if (config.ctfTrace) {
          val trace = Trace.fromCtfFile(config.traceFile.get, config.abortAt)
          val output = Interpreter.run(core, trace, config.stopOn, config.rejectUndeclaredInputs)
          output.foreach(println)
        } else {
          val trace = if (config.csvTrace) {
            config.traceFile
              .map(Trace.fromCsvFile(_, config.abortAt))
              .getOrElse(Trace.fromCsvSource(Source.stdin, "<stdin>", config.abortAt))
          } else {
            config.traceFile
              .map(Trace.fromFile(_, config.abortAt))
              .getOrElse(Trace.fromSource(Source.stdin, "<stdin>", config.abortAt))
          }
          if (config.flattenInput) {
            trace.foreach(println)
          } else {
            val output = Interpreter.run(core, trace, config.stopOn, config.rejectUndeclaredInputs)
            output.foreach(println)
          }
        }
      } catch {
        case ex: TesslaError =>
          printErr(s"Runtime error: $ex")
          if (config.debug) ex.printStackTrace()
        case ex: IOException =>
          printErr(s"IO Error: ${ex.getMessage}")
          if (config.debug) ex.printStackTrace()
      }
    }

    def unwrapResult[T](result: Result[T]): T = result match {
      case Success(res, warnings) =>
        if (config.diagnostics) warnings.foreach(w => printErr(s"Warning: $w"))
        res
      case Failure(errors, warnings) =>
        if (config.diagnostics) {
          warnings.foreach(w => printErr(s"Warning: $w"))
          errors.foreach { e =>
            printErr(s"Error: $e")
            if (config.debug) e.printStackTrace()
          }
          printErr(s"Compilation failed with ${warnings.length} warnings and ${errors.length} errors")
        }
        sys.exit(1)
    }

    def println(a: Any): Unit = {
      if (System.out.checkError()) System.exit(141)
      System.out.println(a)
    }

    def printErr(a: Any): Unit = {
      if (System.err.checkError()) System.exit(141)
      System.err.println(a)
    }
  }

}

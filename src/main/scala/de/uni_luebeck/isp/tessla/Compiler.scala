package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Compiler.Options
import org.antlr.v4.runtime.CharStream

object Compiler {

  case class Options(
    timeUnitString: Option[String],
    includeResolver: String => Option[CharStream],
    stdlibIncludeResolver: String => Option[CharStream],
    stdlibPath: String
  ) {
    lazy val timeUnit = timeUnitString.map(TimeUnit.fromString(_, Location.option("timeunit")))
  }

}

class Compiler(evaluator: Evaluator) {

  def instantiatePipeline(options: Options): TranslationPhase[CharStream, TesslaCore.Specification] = {
    new TesslaParser.WithIncludes(options.includeResolver)
      .andThen(new StdlibIncluder(options.stdlibIncludeResolver, options.stdlibPath))
      .andThen(TesslaSyntaxToTessla)
      .andThen(Flattener)
      .andThen(TypeChecker)
      .andThen(new ConstantEvaluator(options.timeUnit, evaluator))
      .andThen(RemoveUnusedDefinitions)
  }

  def compile(src: CharStream, options: Options): TranslationPhase.Result[TesslaCore.Specification] = {
    instantiatePipeline(options).translate(src)
  }

  class Printer[T] extends TranslationPhase[T, T] {
    override def translate(spec: T) = {
      println(s"=== ${spec.getClass.toString} ===")
      println(spec)
      TranslationPhase.Success(spec, Seq())
    }
  }

  class EnableIf[T](condition: Boolean, phase: TranslationPhase[T,T]) extends TranslationPhase[T, T] {
    override def translate(spec: T) = {
      if (condition) {
        phase.translate(spec)
      } else {
        TranslationPhase.Success(spec, Seq())
      }
    }
  }
}

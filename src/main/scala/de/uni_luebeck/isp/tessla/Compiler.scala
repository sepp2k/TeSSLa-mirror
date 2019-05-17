package de.uni_luebeck.isp.tessla

import org.antlr.v4.runtime.CharStream

object Compiler {
  case class Options(timeUnitString: Option[String], resolveInclude: String => Option[CharStream]) {
    lazy val timeUnit = timeUnitString.map(TimeUnit.fromString(_, Location.option("timeunit")))
  }

  def instantiatePipeline(options: Options): TranslationPhase[CharStream, TesslaCore.Specification] = {
    new TesslaParser.WithIncludes(options.resolveInclude)
      .andThen(TesslaSyntaxToTessla)
//      .andThen(new Printer[Tessla.Specification])
      .andThen(Flattener)
//      .andThen(new Printer[FlatTessla.Specification])
      .andThen(TypeChecker)
      .andThen(new ConstantEvaluator(options.timeUnit))
      .andThen(CycleDetection)
      .andThen(CurrySignalLift)
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
}

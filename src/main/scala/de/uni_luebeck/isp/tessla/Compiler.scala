package de.uni_luebeck.isp.tessla

import org.antlr.v4.runtime.CharStream

object Compiler {
  def instantiatePipeline(unit: Option[TimeUnit]): TranslationPhase[CharStream, TesslaCore.Specification] = {
    TesslaParser
      .andThen(TesslaSyntaxToTessla)
      .andThen(Flattener)
      .andThen(TypeChecker)
      .andThen(new ConstantEvaluator(unit))
      .andThen(CycleDetection)
      .andThen(CurrySignalLift)
      .andThen(RemoveUnusedDefinitions)
  }

  def compile(src: CharStream, unit: Option[TimeUnit]): TranslationPhase.Result[TesslaCore.Specification] = {
    instantiatePipeline(unit).translate(src)
  }

  class Printer[T] extends TranslationPhase[T, T] {
    override def translate(spec: T) = {
      println(s"=== ${spec.getClass.toString} ===")
      println(spec)
      TranslationPhase.Success(spec, Seq())
    }
  }

  def compile(src: CharStream, timeUnit: Option[String])(implicit dummy:DummyImplicit): TranslationPhase.Result[TesslaCore.Specification] = {
    val parsedTimeUnit = timeUnit.map(TimeUnit.fromString(_, Location.option("timeunit")))
    compile(src, parsedTimeUnit)
  }
}

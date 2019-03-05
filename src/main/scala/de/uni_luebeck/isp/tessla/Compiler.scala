package de.uni_luebeck.isp.tessla

import org.antlr.v4.runtime.CharStream

object Compiler {
  def compile(src: CharStream, unit: Option[TimeUnit]): TranslationPhase.Result[TesslaCore.Specification] = {
    new TesslaParser().translate(src)
      .andThen(new Flattener)
      .andThen(new TypeChecker)
      .andThen(new ConstantEvaluator(unit))
      .andThen(new CycleDetection)
      .andThen(new CurrySignalLift)
      .andThen(new RemoveUnusedDefinitions)
  }

  class Printer[T] extends TranslationPhase[T, T] {
    override def translateSpec(spec: T) = {
      println(s"=== ${spec.getClass.toString} ===")
      println(spec)
      spec
    }
  }

  def compile(src: CharStream, timeUnit: Option[String])(implicit i1:DummyImplicit): TranslationPhase.Result[TesslaCore.Specification] = {
    val parsedTimeUnit = timeUnit.map(TimeUnit.fromString(_, Location.option("timeunit")))
    compile(src, parsedTimeUnit)
  }
}

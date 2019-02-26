package de.uni_luebeck.isp.tessla

import org.antlr.v4.runtime.CharStream

class Compiler {
  def applyPasses(src: CharStream, unit: Option[TimeUnit]): TranslationPhase.Result[TesslaCore.Specification] = {
    new TesslaParser().translate(src)
      .andThen(new Flattener)
      .andThen(new TypeChecker)
      .andThen(new ConstantEvaluator(unit))
      .andThen(new CycleDetection)
  }

  class Printer[T] extends TranslationPhase[T, T] {
    override def translateSpec(spec: T) = {
      println(s"=== ${spec.getClass.toString} ===")
      println(spec)
      spec
    }
  }

  def compile(src: CharStream,
              timeUnit: Option[String],
              printCore: Boolean = false) = {
    val parsedTimeUnit = timeUnit.map(TimeUnit.fromString(_, Location.option("timeunit")))
    val result = applyPasses(src, parsedTimeUnit)
    if (printCore) result.andThen(new Printer[TesslaCore.Specification])
    else result
  }
}

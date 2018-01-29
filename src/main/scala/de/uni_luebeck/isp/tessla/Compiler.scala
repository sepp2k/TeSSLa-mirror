package de.uni_luebeck.isp.tessla

import Errors.InternalError

class Compiler {
  def applyPasses(src: TesslaSource, unit: Option[TimeUnit]): TranslationPhase.Result[TesslaCore.Specification] = {
    new TesslaParser().translate(src)
      .andThen(new Printer[Tessla.Specification])
      .andThen(new Flattener)
      .andThen(new Printer[FlatTessla.Specification])
      .andThen(new TypeChecker)
      .andThen(new Printer[TypedTessla.Specification])
      .andThen(new ConstantEvaluator(unit))
      .andThen(new Printer[TesslaCore.Specification])
  }

  class DummyPhase[T,U] extends TranslationPhase[T, U] {
    override def translateSpec(spec: T) = throw InternalError("Unimplemented translation")
  }

  class Printer[T] extends TranslationPhase[T, T] {
    override def translateSpec(spec: T) = {
      println(s"=== ${spec.getClass.toString} ===")
      println(spec)
      spec
    }
  }

  def compile(src: TesslaSource,
              timeUnitSource: Option[TesslaSource],
              printCore: Boolean = false) = {
    val timeUnit = timeUnitSource.map(TimeUnit.parse)
    val result = applyPasses(src, timeUnit)
    if (printCore) result.andThen(new Printer[TesslaCore.Specification])
    else result
  }
}

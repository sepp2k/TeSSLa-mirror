package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit
import Errors.InternalError

class Compiler {
  def applyPasses(src: TesslaSource, unit: Option[TimeUnit], customBuiltIns: CustomBuiltIns): TranslationPhase.Result[TesslaCore.Specification] = {
    new TesslaParser().translate(src)
      .andThen(new Flattener(customBuiltIns))
      .andThen(new Printer[FlatTessla.Specification])
      .andThen(new DummyPhase[FlatTessla.Specification, TesslaCore.Specification])
  }

  class DummyPhase[T,U] extends TranslationPhase[T, U] {
    override def translateSpec(spec: T) = throw InternalError("Unimplemented translation")
  }

  class Printer[T] extends TranslationPhase[T, T] {
    override def translateSpec(spec: T) = {
      println(spec)
      spec
    }
  }

  def compile(src: TesslaSource,
              timeUnitSource: Option[TesslaSource],
              customBuiltIns: CustomBuiltIns = CustomBuiltIns.mapAndSet,
              printCore: Boolean = false) = {
    val timeUnit = timeUnitSource.map(TimeUnit.parse)
    val result = applyPasses(src, timeUnit, customBuiltIns)
    if (printCore) result.andThen(new Printer[TesslaCore.Specification])
    else result
  }
}

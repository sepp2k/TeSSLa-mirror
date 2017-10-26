package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit

class Compiler {
  def applyPasses(src: TesslaSource, unit: Option[TimeUnit], customBuiltIns: CustomBuiltIns): TranslationPhase.Result[TesslaCore.Specification] = {
    new TesslaParser().translate(src).andThen(new AstToCore(unit, customBuiltIns))
  }

  class CorePrinter extends TranslationPhase[TesslaCore.Specification, TesslaCore.Specification] {
    override def translateSpec(spec: TesslaCore.Specification) = {
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
    if (printCore) result.andThen(new CorePrinter)
    else result
  }
}

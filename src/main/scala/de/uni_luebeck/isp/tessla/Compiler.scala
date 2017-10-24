package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit

class Compiler {
  def applyPasses(src: TesslaSource, unit: Option[TimeUnit]): TranslationPhase.Result[TesslaCore.Specification] = {
    new TesslaParser().translate(src).andThen(new AstToCore(unit))
  }

  class CorePrinter extends TranslationPhase[TesslaCore.Specification, TesslaCore.Specification] {
    override def translateSpec(spec: TesslaCore.Specification) = {
      println(spec)
      spec
    }
  }

  def compile(src: TesslaSource, timeUnitSource: Option[TesslaSource], printCore: Boolean = false) = {
    val timeUnit = timeUnitSource.map(TimeUnit.parse)
    val result = applyPasses(src, timeUnit)
    if (printCore) result.andThen(new CorePrinter)
    else result
  }
}

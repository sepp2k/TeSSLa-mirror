package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TimeUnit.Nanos

class Compiler {
  def applyPasses(src: TesslaSource, unit: TimeUnit.Unit = Nanos): TranslationPhase.Result[TesslaCore.Specification] = {
    new Parser().translate(src).andThen(new AstToCore(unit))
  }
}

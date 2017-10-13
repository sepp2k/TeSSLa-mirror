package de.uni_luebeck.isp.tessla

class Compiler {
  def applyPasses(src: TesslaSource, unit: Option[TimeUnit.TimeUnit]): TranslationPhase.Result[TesslaCore.Specification] = {
    new TesslaParser().translate(src).andThen(new AstToCore(unit))
  }
}

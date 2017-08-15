package de.uni_luebeck.isp.tessla

class Compiler {
  def applyPasses(src: TesslaSource, unit: Option[TimeUnit.Unit]): TranslationPhase.Result[TesslaCore.Specification] = {
    new Parser().translate(src).andThen(new AstToCore(unit))
  }
}

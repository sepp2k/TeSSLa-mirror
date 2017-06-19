package de.uni_luebeck.isp.tessla

class Compiler {
  def applyPasses(src: TesslaSource): TranslationPhase.Result[TesslaCore.Specification] = {
    new Parser().translate(src).andThen(new AstToCore)
  }
}

package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Compiler.Options
import de.uni_luebeck.isp.tessla.Tessla.TimeLiteral
import de.uni_luebeck.isp.tessla.TesslaAST._
import de.uni_luebeck.isp.tessla.TranslationPhase._
import org.antlr.v4.runtime.CharStream

object Compiler {

  case class Options(
    baseTimeString: Option[String],
    includeResolver: String => Option[CharStream],
    stdlibIncludeResolver: String => Option[CharStream],
    stdlibPath: String,
    flattenCore: Boolean
  ) {
    lazy val baseTime = baseTimeString.map(TimeLiteral.fromString(_, Location.option("basetime")))
  }

  def compile(src: CharStream, options: Options): Result[Core.Specification] = new Compiler(options).compile(src)
}

class Compiler(options: Options) {

  def instantiatePipeline: TranslationPhase[CharStream, Core.Specification] = {
    tesslaToTyped
      .andThen(typedToCore)
      .andThen(TranslationPhase.EnableIf(options.flattenCore, coreToFlatCore))
  }

  def tesslaToTyped: TranslationPhase[CharStream, TesslaAST.Typed.Specification] = {
    new TesslaParser.WithIncludes(options.includeResolver)
      .andThen(new StdlibIncluder(options.stdlibIncludeResolver, options.stdlibPath))
      .andThen(TesslaSyntaxToTessla)
      .andThen(Flattener)
      .andThen(TypeChecker)
      .andThen(new TypedTessla2TesslaASTCore(options.baseTime))
  }

  def typedToCore: TranslationPhase[Typed.Specification, Core.Specification] = new ConstantEvaluator

  def coreToFlatCore: TranslationPhase[Core.Specification, Core.Specification] = FlattenCore

  def compile(src: CharStream): Result[Core.Specification] = instantiatePipeline.translate(src)

}

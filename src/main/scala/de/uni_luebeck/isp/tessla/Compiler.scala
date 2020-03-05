package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Compiler.Options
import de.uni_luebeck.isp.tessla.Tessla.TimeLiteral
import org.antlr.v4.runtime.CharStream

object Compiler {

  case class Options(
                      baseTimeString: Option[String],
                      includeResolver: String => Option[CharStream],
                      stdlibIncludeResolver: String => Option[CharStream],
                      stdlibPath: String
  ) {
    lazy val baseTime = baseTimeString.map(TimeLiteral.fromString(_, Location.option("basetime")))
  }

}

class Compiler {

  def instantiatePipeline(options: Options): TranslationPhase[CharStream, (TesslaAST.Typed.Specification, TranslationPhase.Result[TesslaAST.Core.Specification])] = {
    new TesslaParser.WithIncludes(options.includeResolver)
      .andThen(new StdlibIncluder(options.stdlibIncludeResolver, options.stdlibPath))
      .andThen(TesslaSyntaxToTessla)
      .andThen(Flattener)
      .andThen(TypeChecker)
      .andThen(new TypedTessla2TesslaASTCore(options.baseTime))
      .andThen(new TranslationPhase.BypassPhase(new ConstantEvaluator))
    //.andThen(RemoveUnusedDefinitions)
  }

  def compile(src: CharStream, options: Options): TranslationPhase.Result[(TesslaAST.Typed.Specification, TranslationPhase.Result[TesslaAST.Core.Specification])] = {
    instantiatePipeline(options).translate(src)
  }

  class Printer[T] extends TranslationPhase[T, T] {
    override def translate(spec: T) = {
      println(s"=== ${spec.getClass.toString} ===")
      println(spec)
      TranslationPhase.Success(spec, Seq())
    }
  }

  class EnableIf[T](condition: Boolean, phase: TranslationPhase[T, T]) extends TranslationPhase[T, T] {
    override def translate(spec: T) = {
      if (condition) {
        phase.translate(spec)
      } else {
        TranslationPhase.Success(spec, Seq())
      }
    }
  }

}

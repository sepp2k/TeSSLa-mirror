package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.InternalError
import de.uni_luebeck.isp.tessla.TesslaParser.ParseResult
import org.antlr.v4.runtime.CharStream

class StdlibIncluder(resolveInclude: String => Option[CharStream], path: String)
    extends TranslationPhase[IndexedSeq[ParseResult], IndexedSeq[ParseResult]] {
  override def translate(spec: IndexedSeq[ParseResult]) = {
    resolveInclude(path)
      .map { src =>
        new TesslaParser.WithIncludes(resolveInclude).translate(src).map { stdlib =>
          stdlib ++ spec
        }
      }
      .getOrElse {
        TranslationPhase.Failure(Seq(InternalError("Could not find stdlib")), Seq())
      }
  }
}

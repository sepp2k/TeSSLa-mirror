/*

 */

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Errors.InternalError
import de.uni_luebeck.isp.tessla.core.TesslaParser.ParseResult
import org.antlr.v4.runtime.CharStream

/**
 * Translation phase handling the resolution and merging of include statements within Tessla.
 *
 * @param resolveInclude the function used to resolve include names to their source
 * @param path the root path to initially resolve
 */
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
        // $COVERAGE-OFF$
        TranslationPhase.Failure(Seq(InternalError("Could not find stdlib")), Seq())
        // $COVERAGE-ON$
      }
  }
}

/*
 * Copyright (c) 2020 Institute of Software Engineering and Programming Languages,
 * University of Lübeck, Germany
 *
 * Modified MIT license:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this binary (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software and the code which is
 * generated by the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Errors.InternalError
import de.uni_luebeck.isp.tessla.core.TesslaParser.ParseResult
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

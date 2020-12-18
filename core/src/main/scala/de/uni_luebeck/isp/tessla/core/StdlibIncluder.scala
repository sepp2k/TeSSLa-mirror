/*
 * Copyright 2020 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

/*
 * Copyright 2022 The TeSSLa Community
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

import de.uni_luebeck.isp.tessla.core.TranslationPhase.Failure
import org.antlr.v4.runtime.{CharStream, CharStreams}
import org.scalatest.funsuite.AnyFunSuite

// Tests which do not fit the general test schema
class AdditionalTests extends AnyFunSuite {

  test("testStackoverflow") {
    val spec =
      """def foo(n: Int): Int =
        |  if n <= 1 then 1 else foo(n-1) + foo(n-2)
        |
        |out foo(1000000000)""".stripMargin

    val result = Compiler.compile(CharStreams.fromString(spec), Compiler.Options())
    assert(result.isInstanceOf[TranslationPhase.Failure])
    val TranslationPhase.Failure(errors, _) = result
    assert(errors.size == 1)
    val error = errors.head
    assert(error.isInstanceOf[Errors.WithStackTrace])
    val Errors.WithStackTrace(inner, _) = error
    assert(inner.isInstanceOf[Errors.StackOverflow])
  }
}

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

package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.AbstractTestRunner
import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.core.Errors.TesslaError
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.{FlattenCore, TesslaAST, TranslationPhase}

import scala.collection.mutable

class InterpreterTests extends AbstractTestRunner[Core.Specification]("Interpreter") {

  override def translation(testCase: TestConfig): TranslationPhase[Core.Specification, Core.Specification] =
    FlattenCore

  override def run(
    spec: TesslaAST.Core.Specification,
    inputFile: String,
    testCase: TestConfig,
    resolver: PathResolver
  ): (String, String) = {
    import resolver._
    val result = mutable.HashSet[String]()
    try {
      source(inputFile) { src =>
        val trace = Trace.fromSource(src, resolve(inputFile), testCase.abortAt.map(BigInt(_)))
        Interpreter
          .run(spec, trace, None, rejectUndeclaredInputs = true)
          .foreach(event => result += event.toString)
      }
      (result.mkString("\n"), "")
    } catch {
      case ex: TesslaError => (result.mkString("\n"), ex.toString)
    }
  }

}

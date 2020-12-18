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

import de.uni_luebeck.isp.tessla.AbstractTestRunner
import de.uni_luebeck.isp.tessla.TestCase.TestConfig
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success
import org.scalactic.Prettifier
import spray.json._

class AnnotationJsonTests extends AbstractTestRunner[String]("Annotations-Json") {
  override def roots: Seq[String] = Seq("annotations-json/")
  override def translation(testCase: TestConfig): TranslationPhase[Core.Specification, String] =
    (spec: Core.Specification) => Success(AnnotationsToJson(spec), Seq())

  implicit val prettifier: Prettifier = {
    case js: JsValue => js.prettyPrint
    case o           => Prettifier.default(o)
  }

  override def compareCompilerResult(actual: String, expected: String): Unit = {
    val Replace = """"name"\s*:\s*"(.*)\$\d+""""
    val a = actual.replaceAll(Replace, """"name": "$1"""")
    val e = expected.replaceAll(Replace, """"name": "$1"""")

    assert(a.parseJson == e.parseJson)
  }
}

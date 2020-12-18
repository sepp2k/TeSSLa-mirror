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

package de.uni_luebeck.isp.tessla.tessladoc

import java.nio.charset.CodingErrorAction
import java.nio.file.Paths

import de.uni_luebeck.isp.tessla.tessladoc.TesslaDoc.Docs
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.util.Using
import spray.json._
import DocJsonProtocol._
import org.antlr.v4.runtime.CharStreams
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

class DocsTest extends AnyFunSuite {

  test("doc") {
    val expected = Using(
      Source.fromInputStream(getClass.getResourceAsStream("doc.json"))
    )(_.getLines().mkString("\n")).get.parseJson.convertTo[Docs]

    val resolver = IncludeResolvers.fromResource(getClass, "") _
    val charStream = resolver("foo.tessla").get
    val options = Compiler.Options(
      baseTimeString = None,
      includeResolver = resolver,
      stdlibIncludeResolver = resolver,
      stdlibPath = "stdlib.tessla"
    )
    val result = DocGenerator(Seq(charStream), options, includeStdlib = true, withIncludes = true)
    assert(result.isInstanceOf[Success[TesslaDoc.Docs]])

    val Success(actual, _) = result

    assert(actual == expected)

  }
}

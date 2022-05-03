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

package de.uni_luebeck.isp.tessla
import java.io.InputStream
import java.nio.charset.StandardCharsets

import spray.json.*
import DefaultJsonProtocol.*
import de.uni_luebeck.isp.tessla.core.IncludeResolvers
import org.antlr.v4.runtime.CharStream

import scala.io.Source
import scala.util.Using

object TestCase {

  case class TestConfig(
    spec: String,
    input: Option[String],
    expectedErrors: Option[String],
    expectedWarnings: Option[String],
    expectedRuntimeErrors: Option[String],
    expectedOutput: Option[String],
    expectedCompilerResult: Option[String],
    abortAt: Option[Int],
    baseTime: Option[String],
    skip: Boolean,
    externalSource: Option[String],
    options: List[String]
  )

  class PathResolver(root: String, path: String, name: String)(implicit clazz: Class[_] = this.getClass) {
    def resolve(file: String): String = path + file.replaceFirst("^_", name)

    def charStream(file: String): CharStream = {
      IncludeResolvers.fromResource(clazz, root)(resolve(file)).get
    }

    def source[T](file: String)(f: Source => T): T =
      Using(Source.fromInputStream(inStream(file))(StandardCharsets.UTF_8))(f).get

    def inStream(file: String): InputStream = clazz.getResourceAsStream(root + resolve(file))

    def string(file: String): String = source(file)(_.getLines().mkString("\n"))

    def grouped(file: String): Set[String] = string(file).split("\n(?! )").toSet
  }

  implicit val reader: JsonReader[TestConfig] = (js: JsValue) => {
    val o = js.asJsObject
    val filled = o.getFields("skip") match {
      case Seq(JsBoolean(true)) => JsObject(o.fields + ("spec" -> JsString("")))
      case _                    => JsObject(o.fields + ("skip" -> JsBoolean(false)))
    }
    val withOptions = filled.copy(filled.fields.updatedWith("options") {
      case None => Some(JsArray())
      case e    => e
    })
    jsonFormat12(TestConfig.apply).read(withOptions)
  }

  def fromString(s: String): TestConfig = s.parseJson.convertTo[TestConfig]
}

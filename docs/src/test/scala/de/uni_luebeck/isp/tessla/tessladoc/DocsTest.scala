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
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

class DocsTest extends AnyFunSuite {

  test("doc") {
    val expected = Using(
      Source.fromInputStream(getClass.getResourceAsStream("doc.json"))
    )(_.getLines().mkString("\n")).get.parseJson.convertTo[Docs]

    val charStream = IncludeResolvers.fromResource(getClass, "")("foo.tessla").get
    val result = DocGenerator(Seq(charStream), Compiler.Options(), includeStdlib = false, withIncludes = false)
    assert(result.isInstanceOf[Success[TesslaDoc.Docs]])

    val Success(actual, _) = result

    assert(actual == expected)

  }
}

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

package de.uni_luebeck.isp.tessla.core

import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

class TesslaParserTests extends AnyFunSuite {

  test("testSingleFileParserFailure") {
    val spec = "bla"
    val result = TesslaParser.SingleFile(CharStreams.fromString(spec, "testSingleFileParser"))
    assert(result.isInstanceOf[TranslationPhase.Failure])
  }

  test("testSingleFileParserSuccess") {
    val spec =
      """in x: Events[Int]
        |out x""".stripMargin
    val result = TesslaParser.SingleFile(CharStreams.fromString(spec, "testSingleFileParser"))
    assert(result.isInstanceOf[TranslationPhase.Success[TesslaParser.ParseResult]])
  }
}

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

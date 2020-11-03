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

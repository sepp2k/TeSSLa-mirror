package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.core.Errors.TesslaError
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.interpreter.{Interpreter, Trace}

class InterpreterTests extends AbstractTestRunner[Core.Specification]("Interpreter") {

  override def translation: TranslationPhase[Core.Specification, Core.Specification] = TranslationPhase.IdentityPhase()

  override def run(
    spec: TesslaAST.Core.Specification,
    inputFile: String,
    testCase: TestConfig,
    resolver: PathResolver
  ): (Set[String], Set[String]) = {
    import resolver._
    try {
      val result = source(inputFile) { src =>
        val trace = Trace.fromSource(src, resolve(inputFile), testCase.abortAt.map(BigInt(_)))
        Interpreter.run(spec, trace, None, rejectUndeclaredInputs = true).toSet
      }
      (result.map(_.toString), Set())
    } catch {
      case ex: TesslaError => (Set[String](), Set(ex.toString))
    }
  }

}

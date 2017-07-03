package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.io.Source

class InterpreterTests extends FunSuite {
  def reSource(path: String): Source = Source.fromInputStream(getClass.getResourceAsStream(path))

  def testFile(name: String, extension: String): Source = reSource(s"tests/$name.$extension")

  val testCases = reSource("tests").getLines.toSeq.groupBy {
    fileName => fileName.replaceFirst("""\.[^.]+$""", "")
  }.mapValues(_.map(_.replaceFirst("""^.*\.([^.]+)$""", "$1")).toSet)

  testCases.foreach {
    case (name, extensions) =>
      test(name) {
        if (extensions.contains("tessla")) {
          val result = Interpreter.fromSource(testFile(name, "tessla"))
          result match {
            case Success(spec, _) =>
              assert(extensions.contains("output"), "Expected: Compilation failure. Actual: Compilation success.")
              val expectedOutput = testFile(name, "output").getLines.toSet
              val actualOutput = mutable.Set[String]()
              spec.outStreams.foreach {
                case (name, stream) => stream.addListener {
                  case Some(value) => actualOutput += s"${spec.getTime}: $name = $value"
                  case None =>
                }
              }
              Traces.feedInput(spec, testFile(name, "input"))
              assert(actualOutput == expectedOutput)
            case Failure(errors, _) =>
              assert(extensions.contains("errors"), "Expected: Compilation success. Actual: Compilation failure.")
              assert(errors.map(_.toString).toSet == testFile(name, "errors").getLines.toSet)
          }
          if (extensions.contains("warnings")) {
            assert(result.warnings.map(_.toString).toSet == testFile(name, "warnings").getLines.toSet)
          }
        }
      }
  }
}

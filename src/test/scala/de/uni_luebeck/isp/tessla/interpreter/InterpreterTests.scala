package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.CompilationError
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.io.Source

class InterpreterTests extends FunSuite {
  def reSource(path: String): Source = Source.fromInputStream(getClass.getResourceAsStream(path))

  def testFile(name: String, extension: String): Source = reSource(s"tests/$name.$extension")

  def getFilesRecursively(root: String, path: String = ""): Stream[String] = {
    def isDir(filename: String) = !filename.contains(".")
    reSource(s"$root/$path").getLines.flatMap { file =>
      if (isDir(file)) getFilesRecursively(root, s"$path/$file")
      else Stream(s"$path/$file")
    }.toStream
  }

  val testCases = getFilesRecursively("tests").groupBy {
    fileName => fileName.replaceFirst("""\.[^.]+$""", "")
  }.mapValues(_.map(_.replaceFirst("""^.*\.([^.]+)$""", "$1"))).toSeq.sortBy(_._1)

  def assert(condition: Boolean, message: String): Unit = {
    if (!condition) fail(message)
  }

  def assertEquals[T](expected: T, actual: T, name: String): Unit = {
    assert(expected == actual, s"Actual $name did not equal $name. Expected: $expected. Actual: $actual.")
  }

  def assertEquals[T](expected: Set[T], actual: Set[T], name: String): Unit = {
    val onlyExpected = expected -- actual
    val onlyActual = actual -- expected
    val diff = (onlyExpected.map("+ " + _) ++ onlyActual.map("- " + _)).toSeq.sortBy(_.substring(2)).mkString("\n")
    assert(expected == actual, s"Actual $name did not equal $name. Diff:\n$diff\n")
  }

  def assertEquals[T](expected: mutable.Set[T], actual: mutable.Set[T], name: String): Unit = {
    assertEquals(expected.toSet, actual.toSet, name)
  }

  testCases.foreach {
    case (name, extensions) =>
      test(name) {
        if (extensions.contains("tessla")) {
          val result = Interpreter.fromSource(testFile(name, "tessla"))
          result match {
            case Success(spec, _) =>
              assert(!extensions.contains("errors"), "Expected: Compilation failure. Actual: Compilation success.")
              def expectedOutput = testFile(name, "output").getLines.toSet
              val actualOutput = mutable.Set[String]()
              spec.outStreams.foreach {
                case (streamName, stream) => stream.addListener {
                  case Some(value) => actualOutput += s"${spec.getTime}: $streamName = $value"
                  case None =>
                }
              }
              def runTraces() = {
                Traces.feedInput(spec, testFile(name, "input"), 100000)
              }
              if (extensions.contains("runtime-errors")) {
                val ex = intercept[CompilationError](runTraces())
                assertEquals(ex.toString, testFile(name, "runtime-errors").mkString, "runtime error")
              } else {
                runTraces()
              }
              assertEquals(actualOutput, expectedOutput, "output")
            case Failure(errors, _) =>
              assert(extensions.contains("errors"),
                s"Expected: Compilation success. Actual: Compilation failure:\n(${errors.mkString("\n")})")
              assertEquals(errors.map(_.toString).toSet, testFile(name, "errors").getLines.toSet, "errors")
          }
          if (extensions.contains("warnings")) {
            assertEquals(result.warnings.map(_.toString).toSet, testFile(name, "warnings").getLines.toSet, "warnings")
          }
        }
      }
  }
}

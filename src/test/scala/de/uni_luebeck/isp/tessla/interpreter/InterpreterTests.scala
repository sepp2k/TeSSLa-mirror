package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TesslaSource
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.io.Source

class InterpreterTests extends FunSuite {
  def reSource(path: String): Source = Source.fromInputStream(getClass.getResourceAsStream(path))

  def testFile(name: String, extension: String): TesslaSource = {
    TesslaSource.fromScalaSource(reSource(s"tests/$name.$extension"), s"$name.$extension")
  }

  def getFilesRecursively(root: String, path: String = ""): Stream[String] = {
    def isDir(filename: String) = !filename.contains(".")

    reSource(s"$root/$path").getLines.flatMap { file =>
      if (isDir(file)) getFilesRecursively(root, s"$path/$file")
      else Stream(s"$path/$file")
    }.toStream
  }

  def stripExtension(fileName: String) = fileName.replaceFirst("""\.[^.]+$""", "")
  def getExtension(fileName: String) = fileName.replaceFirst("""^.*\.([^.]+)$""", "$1")

  val files = getFilesRecursively("tests")
  val testCaseNames = files.filter{ _.endsWith(".tessla") }.map(stripExtension).toSet
  val testCases = files.groupBy(stripExtension).filter{
    case (fileName, _) => testCaseNames.contains(fileName)
  }.mapValues(_.map(getExtension)).toSeq.sortBy(_._1)

  def assert(condition: Boolean, message: String): Unit = {
    if (!condition) fail(message)
  }

  def assertEquals[T](actual: T, expected: T, name: String): Unit = {
    assert(expected == actual, s"Actual $name did not equal expected $name. Expected: $expected. Actual: $actual.")
  }

  def assertEqualSets[T: Ordering](actual: Set[T], expected: Set[T], name: String, stringify: T => String = (x:T) => x.toString): Unit = {
    val onlyExpected = (expected -- actual).map(x => (x, "-"))
    val onlyActual = (actual -- expected).map(x => (x, "+"))
    val diff = (onlyExpected ++ onlyActual).toSeq.sorted.map {case (entry, prefix) => s"$prefix ${stringify(entry)}"}.mkString("\n")
    assert(expected == actual, s"Actual $name did not equal expected $name. Diff:\n$diff\n")
  }

  def splitOutput(line: String): (BigInt, String) = {
    if(line.startsWith("$timeunit")) {
      (-1, line)
    } else {
      val parts = line.split(":", 2)
      (BigInt(parts(0)), parts(1))
    }
  }

  def unsplitOutput(pair: (BigInt, String)): String = s"${pair._1}:${pair._2}"

  testCases.foreach {
    case (name, extensions) =>
      test(name) {
        if (extensions.contains("tessla")) {
          try {
            val result = Interpreter.runSpec(testFile(name, "tessla"), testFile(name, "input"))
            result match {
              case Success(output, _) =>
                assert(!extensions.contains("errors"), "Expected: Compilation failure. Actual: Compilation success.")

                val expectedOutput = testFile(name, "output").getLines.toSet
                val actualOutput = output.toSet

                assertEqualSets(actualOutput.map(_.toString).map(splitOutput), expectedOutput.map(splitOutput),
                  "output", unsplitOutput)
              case Failure(errors, _) =>
                assert(extensions.contains("errors"),
                  s"Expected: Compilation success. Actual: Compilation failure:\n(${errors.mkString("\n")})")
                assertEqualSets(errors.map(_.toString).toSet, testFile(name, "errors").getLines.toSet, "errors")
            }
            if (extensions.contains("warnings")) {
              assertEqualSets(result.warnings.map(_.toString).toSet, testFile(name, "warnings").getLines.toSet, "warnings")
            }
          } catch {
            case ex: TesslaError =>
              assert(extensions.contains("runtime-errors"), s"Expected: success, Actual: Runtime error:\n${ex.message}")
              assertEquals(ex.toString, testFile(name, "runtime-errors").mkString, "runtime error")
          }
        }
      }
  }
}

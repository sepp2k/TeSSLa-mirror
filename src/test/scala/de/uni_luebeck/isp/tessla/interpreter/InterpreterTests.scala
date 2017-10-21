package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TesslaSource
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import org.scalatest.FunSuite
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.collection.mutable
import scala.io.Source

class InterpreterTests extends FunSuite {

  object JSON {

    case class Tests(spec: String, interpreterTest: Option[InterpreterTest], pipelineTest: Option[PipelineTest], testArgs: TestArgs)

    case class InterpreterTest(input: String, expectedOutput: Option[String],
                               expectedErrors: Option[String], expectedWarnings: Option[String], expectedRuntimeErrors: Option[String])

    case class PipelineTest(expectedPipeline: Option[String],
                            expectedPipelineErrors: Option[String], expectedPipelineWarnings: Option[String])

    case class TestArgs(threshold: Int, abortAt: Option[Int])

    implicit val interpreterTestReads: Reads[InterpreterTest] = Json.reads[InterpreterTest]
    implicit val pipelineTestReads: Reads[PipelineTest] = Json.reads[PipelineTest]
    implicit val testArgsReads: Reads[TestArgs] = Json.reads[TestArgs]

    implicit val testReads: Reads[Tests] = (
      (JsPath \ "spec").read[String] and
        (JsPath \ "interpreter").readNullable[InterpreterTest] and
        (JsPath \ "pipeline").readNullable[PipelineTest] and
        (JsPath \ "args").readWithDefault[TestArgs](TestArgs(0, None))
      ) (Tests.apply _)
  }

  val root = "tests"
  val testCases = getFilesRecursively().filter {
    _._2.endsWith(".json")
  }.map {
    case (path, file) => (path, stripExtension(file))
  }

  def stripExtension(fileName: String): String = fileName.replaceFirst("""\.[^.]+$""", "")

  def getFilesRecursively(path: String = ""): Stream[(String, String)] = {
    def isDir(filename: String) = !filename.contains(".")

    Source.fromInputStream(getClass.getResourceAsStream(s"$root/$path"))
      .getLines.flatMap { file =>
      if (isDir(file)) getFilesRecursively(s"$path/$file")
      else Stream((path, file))

    }.toStream
  }

  def assert(condition: Boolean, message: String): Unit = {
    if (!condition) fail(message)
  }

  def assertEquals[T](actual: T, expected: T, name: String): Unit = {
    assert(expected == actual, s"Actual $name did not equal expected $name. Expected: $expected. Actual: $actual.")
  }

  def assertEqualSets[T: Ordering](actual: Set[T], expected: Set[T], name: String, stringify: T => String = (x: T) => x.toString): Unit = {
    val onlyExpected = (expected -- actual).map(x => (x, "-"))
    val onlyActual = (actual -- expected).map(x => (x, "+"))
    val diff = (onlyExpected ++ onlyActual).toSeq.sorted.map { case (entry, prefix) => s"$prefix ${stringify(entry)}" }.mkString("\n")
    assert(expected == actual, s"Actual $name did not equal expected $name. Diff:\n$diff\n")
  }

  def splitOutput(line: String): (BigInt, String) = {
    if (line.startsWith("$timeunit")) {
      (-1, line)
    } else {
      val parts = line.split(":", 2)
      (BigInt(parts(0)), parts(1))
    }
  }

  def unsplitOutput(pair: (BigInt, String)): String = s"${pair._1}:${pair._2}"

  /*Parse the given file specified by the given relative path as json file, and convert it to a 'Tests' instance.*/
  def parseJson(path: String): JSON.Tests = {
    val json = Json.parse(getClass.getResourceAsStream(s"$root/$path.json"))

    val res = Json.fromJson[JSON.Tests](json)
    res match {
      case s: JsSuccess[JSON.Tests] => s.get
      case e: JsError => sys.error("Json Parsing Error: \n" + JsError.toJson(e).fields.mkString("\n"))
    }
  }

  testCases.foreach {
    case (path, name) =>
      def testFile(file: String): Source = Source.fromInputStream(getClass.getResourceAsStream(s"$root/$path/$file"))

      val tests = parseJson(s"$path/$name")
      if (tests.interpreterTest.isDefined) {
        val t = tests.interpreterTest.get
        /*Run Interpreter Test*/
        test(s"$path/$name (Interpreter)") {
          try {
            val traces = TracesParser.parseTraces(new TesslaSource(testFile(t.input), s"$path/${t.input}"))
            val result = Interpreter.fromTesslaSource(new TesslaSource(testFile(tests.spec), s"$path/${tests.spec}"), traces.timeStampUnit)
            result match {
              case Success(spec, _) =>
                assert(t.expectedErrors.isEmpty, "Expected: Compilation failure. Actual: Compilation success.")

                def expectedOutput = testFile(t.expectedOutput.get).getLines.toSet

                val actualOutput = mutable.Set[String]()
                traces.timeStampUnit.foreach(unit => actualOutput += ("$timeunit = \"" + unit + "\""))

                def runTraces(): Unit = {
                  traces.feedInput(spec, tests.testArgs.threshold) {
                    case (ts, n, value) => actualOutput += s"$ts: $n = $value"
                  }
                }

                if (t.expectedRuntimeErrors.isDefined) {
                  val ex = intercept[TesslaError](runTraces())
                  assertEquals(ex.toString, testFile(t.expectedRuntimeErrors.get).mkString, "runtime error")
                } else {
                  runTraces()
                }
                assertEqualSets(actualOutput.map(splitOutput).toSet, expectedOutput.map(splitOutput), "output", unsplitOutput)
              case Failure(errors, _) =>
                assert(t.expectedErrors.isDefined,
                  s"Expected: Compilation success. Actual: Compilation failure:\n(${errors.mkString("\n")})")
                assertEqualSets(errors.map(_.toString).toSet, testFile(t.expectedErrors.get).getLines.toSet, "errors")
            }
            if (t.expectedWarnings.isDefined) {
              assertEqualSets(result.warnings.map(_.toString).toSet, testFile(t.expectedWarnings.get).getLines.toSet, "warnings")
            }
          } catch {
            case ex: TesslaError =>
              assert(t.expectedRuntimeErrors.isDefined, s"Expected: success, Actual: Runtime error:\n${ex.message}")
              assertEquals(ex.toString, testFile(t.expectedRuntimeErrors.get).mkString, "runtime error")
          }
        }
      }
      if (tests.pipelineTest.isDefined) {
        /*Run Pipeline Test*/
        test(s"$path/$name (Pipeline)") {
          fail()
        }
      }
  }
}

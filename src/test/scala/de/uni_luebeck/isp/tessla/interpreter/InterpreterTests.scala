package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.TesslaSource
import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import org.scalatest.FunSuite
import play.api.libs.json._
import play.api.libs.json.Reads.verifying
import scala.collection.mutable
import scala.io.Source

class InterpreterTests extends FunSuite {

  object JSON {

    trait Test
    case class InterpreterTest(spec: String, input: String, expectedOutput: Option[String],
                               expectedErrors: Option[String], expectedWarnings: Option[String],
                               expectedRuntimeErrors: Option[String], threshold: Option[Int], abortAt: Option[Int]) extends Test

    case class PipelineTest(spec: String, expectedPipeline: Option[String],
                            expectedPipelineErrors: Option[String], expectedPipelineWarnings: Option[String],
                            timeunit: Option[String]) extends Test

    case class CompilerTest(spec: String, expectedErrors: Option[String], expectedWarnings: Option[String], timeunit: Option[String]) extends Test


    implicit val timeunitReads: Reads[Option[String]] = (__ \ "timeunit").readNullable[String](verifying(List("ns","us","ms","s","min","h","d").contains))
    implicit val interpreterTestReads: Reads[InterpreterTest] = Json.reads[InterpreterTest]
    implicit val pipelineTestReads: Reads[PipelineTest] = Json.reads[PipelineTest]
    implicit val compilerTestReads: Reads[CompilerTest] = Json.reads[CompilerTest]
  }

  val root = "tests"
  val testCases: Stream[(String, String)] = getFilesRecursively().filter {
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
  def parseJson(path: String): JSON.Test = {
    val json = Json.parse(getClass.getResourceAsStream(s"$root/$path.json"))

    /*Try to parse it as InterpreterTest*/
    Json.fromJson[JSON.InterpreterTest](json) match {
      case JsSuccess(value, _) => value
      case _ =>
        /*If failed, try to parse as PipelineTest*/
        Json.fromJson[JSON.PipelineTest](json) match {
        case JsSuccess(value, _) => value
        case _: JsError =>
          /*If failed, try to parse as CompilerTest*/
          Json.fromJson[JSON.CompilerTest](json) match {
          case JsSuccess(value, _) => value
          case e: JsError => sys.error("Json Parsing Error: \n" + JsError.toJson(e).fields.mkString("\n"))
        }
      }
    }
  }

  testCases.foreach {
    case (path, name) =>
      def testFile(file: String): Source = Source.fromInputStream(getClass.getResourceAsStream(s"$root/$path/$file"))
      val testCase = parseJson(s"$path/$name")
      testCase match{
        case JSON.InterpreterTest(spec, input, expOutput, expErr, expWarn, expRunErr, threshold, abortAt) =>
          /*Run Interpreter Test*/
          test(s"$path/$name (Interpreter)") {
            try {
              val traces = TracesParser.parseTraces(new TesslaSource(testFile(input), s"$path/$input"))
              val result = Interpreter.fromTesslaSource(new TesslaSource(testFile(spec), s"$path/$spec"), traces.timeStampUnit)
              result match {
                case Success(compiledSpec, _) =>
                  assert(expErr.isEmpty, "Expected: Compilation failure. Actual: Compilation success.")

                  def expectedOutput = testFile(expOutput.get).getLines.toSet

                  val actualOutput = mutable.Set[String]()
                  traces.timeStampUnit.foreach(unit => actualOutput += ("$timeunit = \"" + unit + "\""))

                  def runTraces(): Unit = {
                    traces.feedInput(compiledSpec, BigInt(threshold.getOrElse(0))) {
                      case (ts, n, value) => actualOutput += s"$ts: $n = $value"
                    }
                  }

                  if (expRunErr.isDefined) {
                    val ex = intercept[TesslaError](runTraces())
                    assertEquals(ex.toString, testFile(expRunErr.get).mkString, "runtime error")
                  } else {
                    runTraces()
                  }
                  assertEqualSets(actualOutput.map(splitOutput).toSet, expectedOutput.map(splitOutput), "output", unsplitOutput)
                case Failure(errors, _) =>
                  assert(expErr.isDefined,
                    s"Expected: Compilation success. Actual: Compilation failure:\n(${errors.mkString("\n")})")
                  assertEqualSets(errors.map(_.toString).toSet, testFile(expErr.get).getLines.toSet, "errors")
              }
              if (expWarn.isDefined) {
                assertEqualSets(result.warnings.map(_.toString).toSet, testFile(expWarn.get).getLines.toSet, "warnings")
              }
            } catch {
              case ex: TesslaError =>
                assert(expRunErr.isDefined, s"Expected: success, Actual: Runtime error:\n${ex.message}")
                assertEquals(ex.toString, testFile(expRunErr.get).mkString, "runtime error")
            }
          }
        case JSON.PipelineTest(spec, expPipe, expErr, expWarn, timeUnit) =>
          /*Run Pipeline Test*/
          test(s"$path/$name (Pipeline)") {
            fail()
          }
      }
  }
}

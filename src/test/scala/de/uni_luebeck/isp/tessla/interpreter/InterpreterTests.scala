package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.{Compiler, TesslaSource}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import org.scalatest.FunSuite
import play.api.libs.json._
import play.api.libs.json.Reads.verifying
import com.eclipsesource.schema._

import scala.io.Source

class InterpreterTests extends FunSuite {

  object JSON {

    trait Test

    case class InterpreterTest(spec: String, input: String, expectedOutput: Option[String],
                               expectedErrors: Option[String], expectedWarnings: Option[String],
                               expectedRuntimeErrors: Option[String], abortAt: Option[Int]) extends Test

    case class PipelineTest(spec: String, expectedPipeline: Option[String],
                            expectedPipelineErrors: Option[String], expectedPipelineWarnings: Option[String],
                            timeunit: Option[String]) extends Test

    case class CompilerTest(spec: String, expectedErrors: Option[String], expectedWarnings: Option[String], timeunit: Option[String]) extends Test


    implicit val timeunitReads: Reads[Option[String]] = (__ \ "timeunit").readNullable[String](verifying(List("ns", "us", "ms", "s", "min", "h", "d").contains))
    implicit val interpreterTestReads: Reads[InterpreterTest] = Json.reads[InterpreterTest]
    implicit val pipelineTestReads: Reads[PipelineTest] = Json.reads[PipelineTest]
    implicit val compilerTestReads: Reads[CompilerTest] = Json.reads[CompilerTest]

    /*Validates a test of a given type using its json instance and a schema for that test type
    (Schema for type X must be named XSchema.json and located in the root directory).
    Returns the test if successful, throws an Exception otherwise.*/
    def validate(test: Test, testjson: JsValue): JsResult[Test] = {
      val fileName = test.getClass.getTypeName.substring(test.getClass.getTypeName.lastIndexOf("$") + 1) + "Schema"
      val schema = Json.fromJson[SchemaType](Json.parse(getClass.getResourceAsStream(s"$root/$fileName.json"))).get
      SchemaValidator().validate(schema, testjson) match {
        case JsSuccess(_, path) => JsSuccess(test, path)
        case e: JsError => e
      }
    }

    def jsErrorToString(jsError: JsError): String = {
      jsError.errors.map {
        case (jspath, errors) => errors.map {
          case JsonValidationError(messages, _) => messages.mkString("\n")
        }.mkString("\n")
      }.mkString("\n")
    }
  }

  val root = "tests"
  val testCases: Stream[(String, String)] = getFilesRecursively().filter {
    case ((path, file)) => file.endsWith(".json") && !(file.endsWith("Schema.json") && path.isEmpty)
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
    val interpreterResult = Json.fromJson[JSON.InterpreterTest](json).flatMap(JSON.validate(_, json))
    interpreterResult match {
      case JsSuccess(value, _) => value
      case _: JsError =>
        val pipelineResult = Json.fromJson[JSON.PipelineTest](json).flatMap(JSON.validate(_, json))
        pipelineResult match {
          case JsSuccess(value, _) => value
          case _: JsError =>
            val compilerResult = Json.fromJson[JSON.CompilerTest](json).flatMap(JSON.validate(_, json))
            compilerResult match {
              case JsSuccess(value, _) => value
              case e: JsError => sys.error(s"Error in Json parsing: ${JSON.jsErrorToString(e)}")
            }
        }
    }
  }

  testCases.foreach {
    case (path, name) =>
      def testSource(file: String): TesslaSource = TesslaSource.fromJavaStream(getClass.getResourceAsStream(s"$root/$path/$file"), s"$path/$file")

      val testCase = parseJson(s"$path/$name")
      testCase match {
        case JSON.InterpreterTest(spec, input, expOutput, expErr, expWarn, expRunErr, abortAt) =>
          test(s"$path/$name (Interpreter)") {
            try {
              val result = Interpreter.runSpec(testSource(spec), testSource(input), abortAt = abortAt.map(BigInt(_)))
              result match {
                case Success(output, _) =>
                  assert(expErr.isEmpty, "Expected: Compilation failure. Actual: Compilation success.")
                  val expectedOutput = testSource(expOutput.get).getLines.toSet
                  val actualOutput = output.toSet

                  assertEqualSets(actualOutput.map(_.toString).map(splitOutput), expectedOutput.map(splitOutput),
                    "output", unsplitOutput)
                case Failure(errors, _) =>
                  assert(expErr.isDefined,
                    s"Expected: Compilation success. Actual: Compilation failure:\n(${errors.mkString("\n")})")
                  assertEqualSets(errors.map(_.toString).toSet, testSource(expErr.get).getLines.toSet, "errors")
              }
              if (expWarn.isDefined) {
                assertEqualSets(result.warnings.map(_.toString).toSet, testSource(expWarn.get).getLines.toSet, "warnings")
              }
            } catch {
              case ex: TesslaError =>
                assert(expRunErr.isDefined, s"Expected: success, Actual: Runtime error:\n${ex.message}")
                assertEquals(ex.toString, testSource(expRunErr.get).mkString, "runtime error")
            }
          }
        case JSON.PipelineTest(spec, expPipe, expErr, expWarn, timeUnit) =>
          test(s"$path/$name (Pipeline)") {
            // This is a place holder until the pipeline branch is merged
            fail()
          }
        case JSON.CompilerTest(spec, expErr, expWarn, timeUnit) =>
          test(s"$path/$name (Compiler)") {
            val result = new Compiler().compile(testSource(spec), timeUnit.map(TesslaSource.fromString(_, s"$path/$name.json#timeunit")))
            result match {
              case Success(_, _) =>
                assert(expErr.isEmpty, "Expected: Compilation failure. Actual: Compilation success.")
              case Failure(errors, _) =>
                assert(expErr.isDefined,
                  s"Expected: Compilation success. Actual: Compilation failure:\n(${errors.mkString("\n")})")
                assertEqualSets(errors.map(_.toString).toSet, testSource(expErr.get).getLines.toSet, "errors")
            }
            if (expWarn.isDefined) {
              assertEqualSets(result.warnings.map(_.toString).toSet, testSource(expWarn.get).getLines.toSet, "warnings")
            }
          }
      }
  }
}

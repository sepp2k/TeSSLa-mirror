package de.uni_luebeck.isp.tessla.interpreter

import java.nio.charset.StandardCharsets

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.{Compiler, IncludeResolvers, TranslationPhase}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}
import org.scalatest.FunSuite
import play.api.libs.json._
import play.api.libs.json.Reads.verifying
import com.eclipsesource.schema._
import de.uni_luebeck.isp.tessla.analyses.Observations
import org.antlr.v4.runtime.CharStream

import scala.collection.mutable
import scala.io.Source

class InterpreterTests extends FunSuite {

  object JSON {

    case class TestCase(spec: String, input: Option[String], expectedOutput: Option[String],
                        expectedErrors: Option[String], expectedWarnings: Option[String],
                        expectedRuntimeErrors: Option[String], expectedObservations: Option[String],
                        abortAt: Option[Int], timeUnit: Option[String])

    implicit val timeUnitReads: Reads[Option[String]] = (__ \ "timeunit").readNullable[String](verifying(List("ns", "us", "ms", "s", "min", "h", "d").contains))
    implicit val interpreterTestReads: Reads[TestCase] = Json.reads[TestCase]

    /*Validates a test of a given type using its json instance and a schema for that test type
    (Schema for type X must be named XSchema.json and located in the root directory).
    Returns the test if successful, throws an Exception otherwise.*/
    def validate(test: TestCase, testjson: JsValue): JsResult[TestCase] = {
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
    case (path, file) => file.endsWith(".json") && !(file.endsWith("Schema.json") && path.isEmpty)
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
  def parseJson(path: String): JSON.TestCase = {
    val json = Json.parse(getClass.getResourceAsStream(s"$root/$path.json"))
    Json.fromJson[JSON.TestCase](json).flatMap(JSON.validate(_, json)) match {
      case JsSuccess(value, _) => value
      case e: JsError => sys.error(s"Error in Json parsing: ${JSON.jsErrorToString(e)}")
    }
  }

  testCases.foreach {
    case (path, name) =>
      def testStream(file: String): CharStream = {
        IncludeResolvers.fromResource(getClass, root)(s"$path/$file").get
      }
      def testSource(file: String): Source = {
        Source.fromInputStream(getClass.getResourceAsStream(s"$root/$path/$file"))(StandardCharsets.UTF_8)
      }

      val testCase = parseJson(s"$path/$name")
      test(s"$path/$name (Interpreter)") {
        def handleResult[T](result: TranslationPhase.Result[T])(onSuccess: T => Unit): Unit = {
          result match {
            case Success(output, _) =>
              assert(testCase.expectedErrors.isEmpty, "Expected: Compilation failure. Actual: Compilation success.")
              onSuccess(output)
            case Failure(errors, _) =>
              testCase.expectedErrors match {
                case None =>
                  fail(s"Expected: Compilation success. Actual: Compilation failure:\n(${errors.mkString("\n")})")
                case Some(expectedErrorsFile) =>
                  // Only split on new lines if the next line is not indented because otherwise it's a continuation
                  // and still part of the same error message (e.g. a stack trace)
                  val expectedErrors = testSource(expectedErrorsFile).mkString.split("\n(?! )").toSet
                  assertEqualSets(errors.map(_.toString).toSet, expectedErrors, "errors")
              }
          }
          testCase.expectedWarnings match {
            case Some(expectedWarnings) =>
              assertEqualSets(result.warnings.map(_.toString).toSet, testSource(expectedWarnings).getLines.toSet, "warnings")
            case None =>
              // If there is no expected warnings file, we don't care whether there were warnings or not.
              // To assert that there should be no warnings, one should create an empty expected warnings file.
          }
        }

        val options = Compiler.Options(
          timeUnitString = testCase.timeUnit,
          includeResolver = IncludeResolvers.fromResource(getClass, root),
          stdlibIncludeResolver = IncludeResolvers.fromStdlibResource,
          stdlibPath = "Predef.tessla",
          currySignalLift = true
        )
        val src = testStream(testCase.spec)
        testCase.expectedObservations.foreach { observationFile =>
          val expectedObservation = testSource(observationFile).mkString
          handleResult(Compiler.compile(src, options).andThen(Observations.Generator)) { observation =>
            val actualObservation = observation.toString
            assertEquals(actualObservation, expectedObservation, "Observation")
          }
        }
        testCase.input match {
          case Some(input) =>
            try {
              val trace = Trace.fromSource(testSource(input), s"$path/$input", testCase.abortAt.map(BigInt(_)))
              val result = Compiler.compile(src, options).map(spec => Interpreter.run(spec, trace, None))

              handleResult(result) { output =>
                val expectedOutput = testSource(testCase.expectedOutput.get).getLines.toSet
                val actualOutput = output.toSet

                assert(testCase.expectedRuntimeErrors.isEmpty, "Expected: Runtime error. Actual: success")
                assertEqualSets(actualOutput.map(_.toString).map(splitOutput), expectedOutput.map(splitOutput),
                  "output", unsplitOutput)
              }
            } catch {
              case ex: TesslaError =>
                testCase.expectedRuntimeErrors match {
                  case Some(errors) =>
                    assertEquals(ex.toString, testSource(errors).mkString, "runtime error")
                  case None =>
                    fail(s"Expected: success, Actual: Runtime error:\n${ex.message}")
                }
            }
          case None =>
            handleResult(Compiler.compile(src, options))(_ => ())
        }
      }
  }
}

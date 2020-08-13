package de.uni_luebeck.isp.tessla.tessla_compiler

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.eclipsesource.schema._
import com.eclipsesource.schema.drafts.Version4._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Failure, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend._
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{Laziness, UsageAnalysis}
import de.uni_luebeck.isp.tessla.core.{IncludeResolvers, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.Compiler
import org.antlr.v4.runtime.CharStream
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.Reads.verifying
import play.api.libs.json._

import scala.io.Source
import scala.reflect.io.Directory
import scala.sys.process._

/**
 * Object contains static functions often used in test cases
 */
object CompilerTests {
  def compileChain(src: CharStream, compiler: Compiler): TranslationPhase.Result[String] = {
    compiler
      .tesslaToTyped(src)
      .andThen(compiler.typedToCore)
      .andThen(UsageAnalysis)
      .andThen(Laziness)
      .andThen(new TesslaCoreToIntermediate(true))
      .andThen(UnusedVarRemove)
      .andThen(new ScalaBackend)
  }

  def writeToFile(path: String, content: String): Unit = {
    val sPw = new PrintWriter(path)
    sPw.write(content)
    sPw.close()
  }

  class TestProcessLogger extends ProcessLogger {
    var output: Seq[String] = Seq()

    override def out(s: => String): Unit = output :+= s

    override def err(s: => String): Unit = output :+= s"E $s"

    override def buffer[T](f: => T): T = f
  }
}

class CompilerTests extends AnyFunSuite with BeforeAndAfterAll {

  val root = "tests/"
  val testCases: LazyList[(String, String)] = getFilesRecursively()
    .filter {
      case (path, file) => file.endsWith(".json") && !(file.endsWith("Schema.json") && path.isEmpty)
    }
    .map {
      case (path, file) => (path, stripExtension(file))
    }

  val fsPath: Path = Files.createTempDirectory("TesslaCompilerTests")

  def stripExtension(fileName: String): String = fileName.replaceFirst("""\.[^.]+$""", "")

  def getFilesRecursively(path: String = ""): LazyList[(String, String)] = {
    def isDir(filename: String) = !filename.contains(".")

    Source
      .fromInputStream(getClass.getResourceAsStream(s"$root$path"))
      .getLines()
      .flatMap { file =>
        if (isDir(file)) getFilesRecursively(s"$path$file/")
        else LazyList((path, file))
      }
      .to(LazyList)
  }

  def assertEquals[T](actual: T, expected: T, name: String): Unit = {
    assert(actual == expected, s"$actual\ndid not equal\n$expected")
  }

  def assert(condition: Boolean, message: String): Unit = {
    if (!condition) fail(message)
  }

  def assertEqualSets[T: Ordering](
    actual: Set[T],
    expected: Set[T],
    name: String,
    stringify: T => String = (x: T) => x.toString
  ): Unit = {
    def sort(s: Set[T]) = s.toIndexedSeq.sorted.map(stringify).mkString("\n")
    assert(actual == expected, s"${sort(actual)}\ndid not equal\n${sort(expected)}")
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
  def parseTestCase(path: String): JSON.TestCase = {
    parseJson[JSON.TestCase](s"$path.json", JSON.validate)
  }

  def parseJson[T: Reads](path: String, validate: JsValue => JsResult[_] = x => JsSuccess(x)): T = {
    val json = Json.parse(getClass.getResourceAsStream(s"$root$path"))
    validate(json).flatMap(_ => Json.fromJson[T](json)) match {
      case JsSuccess(value, _) => value
      case e: JsError          => sys.error(s"Error in Json parsing: ${JSON.jsErrorToString(e)}")
    }
  }

  override def afterAll(): Unit = {
    Directory(fsPath.toFile).deleteRecursively()
  }

  object JSON {

    /*Validates a test of a given type using its json instance and a schema for that test type
    (Schema for type X must be named XSchema.json and located in the root directory).
    Returns the test if successful, throws an Exception otherwise.*/
    def validate(testjson: JsValue): JsResult[JsValue] = {
      val fileName = "TestCaseSchema.json"
      val schema =
        Json.fromJson[SchemaType](Json.parse(getClass.getResourceAsStream(s"$root$fileName"))).get
      SchemaValidator().validate(schema, testjson)
    }

    implicit val timeUnitReads: Reads[Option[String]] = (__ \ "timeunit")
      .readNullable[String](verifying(List("ns", "us", "ms", "s", "min", "h", "d").contains))
    implicit val interpreterTestReads: Reads[TestCase] = Json.reads[TestCase]

    def jsErrorToString(jsError: JsError): String = {
      jsError.errors
        .map {
          case (_, errors) =>
            errors
              .map {
                case JsonValidationError(messages, _) => messages.mkString("\n")
              }
              .mkString("\n")
        }
        .mkString("\n")
    }

    case class TestCase(
      spec: String,
      input: Option[String],
      expectedOutput: Option[String],
      expectedErrors: Option[String],
      expectedWarnings: Option[String],
      expectedObservationErrors: Option[String],
      expectedRuntimeErrors: Option[String],
      expectedObservations: Option[String],
      abortAt: Option[Int],
      baseTime: Option[String],
      excludeForCompiler: Option[Boolean]
    )
  }

  testCases.zipWithIndex.foreach {
    case ((path, name), idx) =>
      def testStream(file: String): CharStream = {
        IncludeResolvers.fromResource(getClass, root)(s"$path$file").get
      }
      def testSource(file: String): Source = {
        Source.fromInputStream(getClass.getResourceAsStream(s"$root$path$file"))(
          StandardCharsets.UTF_8
        )
      }

      def compileAndExecute(sourceCode: String, inputTracePath: String): (Seq[String], Boolean) = {

        val compileLogger = new CompilerTests.TestProcessLogger
        val executionLogger = new CompilerTests.TestProcessLogger

        CompilerTests.writeToFile(fsPath.toAbsolutePath.toString + "/out.scala", sourceCode)
        CompilerTests.writeToFile(
          fsPath.toAbsolutePath.toString + "/input",
          testSource(inputTracePath).getLines().mkString("\n")
        )

        val scalac = sys.props("os.name").toLowerCase match {
          case s if s.contains("windows") => "scalac.bat"
          case _ => "scalac"
        }
        val workingDir = new File(fsPath.toAbsolutePath.toString)
        val compile = Process(s"$scalac out.scala", workingDir)
        if (compile.!(compileLogger) != 0) {
          fail(s"Compilation failed:\n${compileLogger.output.mkString("\n")}")
        }

        val run = Process("cat input", workingDir) #|
          Process(s"scala Main < input", workingDir)
        val rc = run.!(executionLogger)

        (executionLogger.output, rc != 0)

      }

      def handleResult(
        result: TranslationPhase.Result[String],
        expectedErrors: Option[String],
        expectedWarnings: Option[String],
        inputTracePath: String
      )(onSuccess: (Seq[String], Boolean) => Unit): Unit = {
        println(s"Evaluate testcase ${idx + 1} / ${testCases.size}: $name ($path)...")
        result match {
          case Success(output, _) =>
            val compilation = compileAndExecute(output, inputTracePath)

            assert(
              expectedErrors.isEmpty,
              "Expected: Compilation failure. Actual: Compilation success."
            )
            onSuccess(compilation._1, compilation._2)
          case Failure(errors, _) =>
            expectedErrors match {
              case None =>
                fail(
                  s"Expected: Compilation success. Actual: Compilation failure:\n(${errors.mkString("\n")})"
                )
              case Some(expectedErrorsFile) =>
                // Only split on new lines if the next line is not indented because otherwise it's a continuation
                // and still part of the same error message (e.g. a stack trace)
                val expectedErrors =
                  testSource(expectedErrorsFile).getLines().mkString("\n").split("\n(?! )").toSet
                assertEqualSets(errors.map(_.toString).toSet, expectedErrors, "errors")
            }
        }
        expectedWarnings match {
          case Some(expectedWarnings) =>
            assertEqualSets(
              result.warnings.map(_.toString).toSet,
              testSource(expectedWarnings).getLines().toSet,
              "warnings"
            )
          case None =>
          // If there is no expected warnings file, we don't care whether there were warnings or not.
          // To assert that there should be no warnings, one should create an empty expected warnings file.
        }
      }

      val testCase = parseTestCase(s"$path$name")

      testCase.excludeForCompiler match {
        case Some(true) =>
        case _ =>
          test(s"$path$name (Compiler, Scala)") {
            val options = Compiler.Options(
              baseTimeString = testCase.baseTime,
              includeResolver = IncludeResolvers.fromResource(getClass, root),
            )
            val src = testStream(testCase.spec)
            val compiler = new Compiler(options)

            // testCase.expectedObservations.foreach { ??? }
            // testCase.expectedObservationErrors.foreach { ??? }

            testCase.input match {
              case Some(input) =>
                val code = CompilerTests.compileChain(src, compiler)

                handleResult(code, testCase.expectedErrors, testCase.expectedWarnings, input) {
                  (output: Seq[String], runtimeError: Boolean) =>
                    val expectedOutput = testSource(testCase.expectedOutput.get).getLines().toSet
                    val actualOutput = output.toSet

                    if (runtimeError) {
                      assert(
                        testCase.expectedRuntimeErrors.isDefined,
                        s"Expected: success. Actual: Runtime error\n${output.mkString("\n")}"
                      )
                    } else {
                      assert(
                        testCase.expectedRuntimeErrors.isEmpty,
                        s"Expected: Runtime error. Actual: success\n${testCase.expectedRuntimeErrors}"
                      )

                      assertEqualSets(
                        actualOutput.map(splitOutput),
                        expectedOutput.map(splitOutput),
                        "output",
                        unsplitOutput
                      )
                    }
                }
              case None =>
                handleResult(
                  CompilerTests.compileChain(src, compiler),
                  testCase.expectedErrors,
                  testCase.expectedWarnings,
                  ""
                )((_, _) => ())
            }
          }
      }
  }
}

package de.uni_luebeck.isp.tessla.tessla_compiler

import java.io.{ByteArrayOutputStream, InputStream, PrintStream, PrintWriter}
import java.lang.reflect.InvocationTargetException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.eclipsesource.schema._
import com.eclipsesource.schema.drafts.Version4._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Failure, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend._
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{Laziness, UsageAnalysis}
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers, TranslationPhase}
import de.uni_luebeck.isp.tessla.tessla_compiler.NoExitSecurityManager.SystemExitException
import org.antlr.v4.runtime.CharStream
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.Reads.verifying
import play.api.libs.json._

import scala.io.Source
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.reflect.io.Directory
import scala.tools.nsc.{Global, Settings}

/**
 * Object contains static functions often used in test cases
 */
object CompilerTests {
  def compileChain(src: CharStream, compiler: Compiler): TranslationPhase.Result[String] = {
    compiler
      .instantiatePipeline(src)
      .andThen(UsageAnalysis)
      .andThen(Laziness)
      .andThen(new TesslaCoreToIntermediate(true))
      .andThen(UnusedVarRemove)
      .andThen(new ScalaBackend)
  }

  def withRedirectedIO[T](in: InputStream, out: PrintStream, err: PrintStream)(body: => T): T = {
    val (oldIn, oldOut, oldErr) = (System.in, System.out, System.err)
    System.setIn(in)
    System.setOut(out)
    System.setErr(err)
    try Console.withIn(in)(
      Console.withOut(out)(
        Console.withErr(err)(
          body
        )
      )
    )
    finally {
      System.setIn(oldIn)
      System.setOut(oldOut)
      System.setErr(oldErr)
    }
  }

  // Search for Main$ in the provided path and execute Main$.main with the input trace.
  def execute(path: Path, inputTrace: InputStream): (Seq[String], Seq[String], Boolean) = {
    import scala.reflect.runtime.universe._
    val cl = new URLClassLoader(Seq(path.toUri.toURL), this.getClass.getClassLoader)

    // Load the generated Main$.class and look for the main method.
    val main = cl.loadClass("Main$")
    val method = main.getDeclaredMethod("main", classOf[Array[String]])

    // Create reflection mirror from the newly created class loader, to then load the module 'Main' to execute the main
    // method on
    val mirror = runtimeMirror(cl)
    val module = mirror.staticModule("Main")
    val obj = mirror.reflectModule(module)

    // Set a custom security manager to forbid system.exit calls, and override all IO (in, out, err)
    val secMan = System.getSecurityManager
    val out = new ByteArrayOutputStream()
    val err = new ByteArrayOutputStream()
    try {
      System.setSecurityManager(new NoExitSecurityManager())
      CompilerTests.withRedirectedIO(inputTrace, new PrintStream(out), new PrintStream(err))(
        method.invoke(obj.instance, Array.empty[String])
      )
    } catch {
      case e: InvocationTargetException =>
        // Since the main method which calls the system exit is invoked through reflection, the
        // thrown exit exception will be wrapped inside of an InvocationTargetException.
        e.getTargetException match {
          case SystemExitException(_) => // Suppress
          case t                      => throw t
        }
    } finally {
      System.setSecurityManager(secMan)
    }

    val errors = err.toString(StandardCharsets.UTF_8).linesIterator.toSeq
    val output = out.toString(StandardCharsets.UTF_8).linesIterator.toSeq

    // A bit of a workaround. Since the exception from the security manager is thrown while the redirected
    // IO is active, that exception will be logged by the generated code as well.
    val filtered = if (errors.lastOption.exists(_.contains("SystemExitException"))) errors.dropRight(2) else errors

    (output, filtered, filtered.nonEmpty)
  }

  def compile(dirPath: Path, sourceCode: String): Unit = {
    Files.writeString(dirPath.resolve("out.scala"), sourceCode)

    val settings = ScalaCompiler.defaultSettings(dirPath, false)
    settings.opt.clear()
    settings.usejavacp.value = false

    // IMPORTANT
    // This is a somewhat unstable workaround. When running the tests from sbt, the java.class.path is
    // overridden with the sbt-launcher, since sbt handles class loading differently. Since the scala compiler
    // however fetches java.class.path, this causes it to fail since none of the scala libraries can be found
    // This line takes the location of Predef and adds this to the classpath (so the scala-library) to prevent this.
    // However I don't know if that's always sufficient. Also, there's probably a cleaner solution for it.
    settings.classpath.value = Predef.getClass.getProtectionDomain.getCodeSource.getLocation.getPath

    val reporter = new TesslaCompilerReporter(settings)
    ScalaCompiler.compileCode(dirPath.resolve("out.scala"), settings, reporter)

    val compiler = new Global(settings, reporter)
    (new compiler.Run) compile List(dirPath.resolve("out.scala").toAbsolutePath.toString)
    reporter.finish()
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
    stringify: T => String = (x: T) => x.toString
  ): Unit = {
    def sort(s: Set[T]) = s.toIndexedSeq.sorted.map(stringify).mkString("\n")
    assert(actual == expected, s"${sort(actual)}\ndid not equal\n${sort(expected)}")
  }

  def splitOutput(line: String): (BigInt, String) = {
    val parts = line.split(":", 2)
    (BigInt(parts(0)), parts(1))
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
      jsError.errors.map {
        case (_, errors) => errors.map {
                case JsonValidationError(messages, _) => messages.mkString("\n")
              }.mkString("\n")
        }.mkString("\n")
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
        Source.fromInputStream(testInputStream(file))(StandardCharsets.UTF_8)
      }

      def testInputStream(file: String): InputStream = {
        getClass.getResourceAsStream(s"$root$path$file")
      }

      def handleResult(
        result: TranslationPhase.Result[String],
        expectedErrors: Option[String],
        expectedWarnings: Option[String]
      )(onSuccess: String => Unit): Unit = {
        println(s"Evaluate testcase ${idx + 1} / ${testCases.size}: $name ($path)...")
        result match {
          case Success(source, _) =>
            assert(expectedErrors.isEmpty, "Expected: Compilation failure. Actual: Compilation success.")
            onSuccess(source)
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
                assertEqualSets(errors.map(_.toString).toSet, expectedErrors)
            }
        }
        expectedWarnings match {
          case Some(expectedWarnings) =>
            assertEqualSets(
              result.warnings.map(_.toString).toSet,
              testSource(expectedWarnings).getLines().toSet
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
              includeResolver = IncludeResolvers.fromResource(getClass, root)
            )
            val src = testStream(testCase.spec)
            val compiler = new Compiler(options)

            testCase.input match {
              case Some(input) =>
                val code = CompilerTests.compileChain(src, compiler)

                handleResult(code, testCase.expectedErrors, testCase.expectedWarnings) { code =>
                  CompilerTests.compile(fsPath, code)
                  val (output, errors, runtimeError) = CompilerTests.execute(fsPath, testInputStream(input))
                  val expectedOutput = testSource(testCase.expectedOutput.get).getLines().toSet
                  val actualOutput = output.toSet

                  if (runtimeError) {
                    assert(
                      testCase.expectedRuntimeErrors.isDefined,
                      s"Expected: success. Actual: Runtime error\n${errors.mkString("\n")}"
                    )
                  } else {
                    assert(
                      testCase.expectedRuntimeErrors.isEmpty,
                      s"Expected: Runtime error. Actual: success\n${testCase.expectedRuntimeErrors}"
                    )

                    assertEqualSets(
                      actualOutput.map(splitOutput),
                      expectedOutput.map(splitOutput),
                      unsplitOutput
                    )
                  }
                }
              case None =>
                handleResult(
                  CompilerTests.compileChain(src, compiler),
                  testCase.expectedErrors,
                  testCase.expectedWarnings
                )(_ => ())
            }
          }
      }
  }
}

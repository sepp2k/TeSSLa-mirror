package de.uni_luebeck.isp.tessla

import java.nio.file.{Files, Paths}

import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Failure, Result, Success}
import spray.json._
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers, TranslationPhase}
import org.scalatest.funsuite.AnyFunSuite

import scala.jdk.CollectionConverters._
import java.nio.file.FileSystems
import java.nio.file.PathMatcher

import de.uni_luebeck.isp.tessla.core.Errors.TesslaError

import scala.io.Source
import scala.util.Using

object AbstractTestRunner {}

abstract class AbstractTestRunner[T](runnerName: String) extends AnyFunSuite {

  // (root, (path, name))
  type TestSource = (String, (String, String))

  // Override to set other test roots, or to override existing tests, e.g. Seq("common", "mytests")
  // Every test defined in "common" which also exists in "mytest" is overridden
  def roots = Seq("common/")

  // The translation phase to apply after the core compiler
  def translation(testCase: TestConfig): TranslationPhase[Core.Specification, T]

  // Called after unwrapping the compiler result if an 'input' is defined
  // should run the input on the specification
  // and return a tuple of (output, errors)
  def run(spec: T, inputFile: String, testCase: TestConfig, resolver: PathResolver): (String, String) =
    throw new UnsupportedOperationException

  // Gets called if a 'expectedCompilerResult' is defined, override to implement
  // checks between expected and actual result
  def compareCompilerResult(compilerResult: T, expectedResult: String): Unit =
    throw new UnsupportedOperationException

  // Override this to define some tests to be ignored dynamically (another option is to use the 'skip' flag in
  // the Json configuration of a test case)
  def shouldIgnoreTest(t: TestSource): Boolean = false

  final def testCases(): Seq[TestSource] = roots
    .map { root =>
      val rootPath = Paths.get(AbstractTestRunner.getClass.getResource(root).toURI)
      val matcher: PathMatcher = FileSystems.getDefault.getPathMatcher("glob:**.json")
      Files
        .walk(rootPath)
        .iterator()
        .asScala
        .to(LazyList)
        .collect {
          case path if matcher.matches(path) =>
            val p = rootPath.relativize(path.getParent).toString.replace('\\', '/')
            val file = path.getFileName.toString.replaceFirst("""\.[^.]+$""", "")
            (if (p.isEmpty) "" else s"$p/", file) -> root
        }
        .toMap
    }
    .fold(Map())(_ ++ _)
    .toSeq
    .map(_.swap)
    .sorted

  testCases().zipWithIndex.foreach({
    case ((root, (path, name)), index) =>
      val testCase = parseTestCase(s"$root$path$name")
      val mode = if (testCase.skip || shouldIgnoreTest((root, (path, name)))) ignore _ else test _
      mode(s"($runnerName) $root$path$name", Seq()) {
        val pathResolver = new PathResolver(root, path, name)
        import pathResolver._

        val options = Compiler.Options(
          baseTimeString = testCase.baseTime,
          includeResolver = IncludeResolvers.fromResource(AbstractTestRunner.getClass, root)
        )

        val spec = charStream(testCase.spec)
        val expErr = testCase.expectedErrors.toSet.flatMap(grouped)
        val expWarn = testCase.expectedWarnings.toSet.flatMap(grouped)
        val result =
          try {
            Compiler.compile(spec, options).andThen(translation(testCase))
          } catch {
            // Tessla errors may be thrown outside of translation phases, e.g. when evaluating the base time
            // Catch and wrap those into the result here
            case e: TesslaError => Failure(Seq(e), Seq())
          }

        val expCompilerResult = testCase.expectedCompilerResult.map(string)

        handleCompilerResult(result, expErr, expWarn, expCompilerResult)(
          onCompilerSuccess(_, testCase, pathResolver)
        )

      }
  })

  def handleCompilerResult(
    result: Result[T],
    expectedErrors: => Set[String],
    expectedWarnings: => Set[String],
    expectedCompilerResult: => Option[String]
  )(onSuccess: T => Unit): Unit = {
    assertEqualSets(result.warnings.map(_.toString).toSet, expectedWarnings)
    result match {
      case Success(_, _) if expectedErrors.nonEmpty =>
        fail("Expected: Compilation failure. Actual: Compilation success.")
      case Success(value, _) =>
        expectedCompilerResult.foreach(compareCompilerResult(value, _))
        onSuccess(value)
      case Failure(errors, _) if expectedErrors.isEmpty =>
        fail(s"Expected: Compilation success. Actual: Compilation failure:\n${errors.mkString("\n")}")
      case Failure(errors, _) =>
        assertEqualSets(errors.map(_.toString).toSet, expectedErrors)
    }
  }

  def onCompilerSuccess(spec: T, testCase: TestConfig, resolver: PathResolver): Unit = {
    import resolver._

    testCase.input match {
      case Some(input) =>
        val (output, errors) = run(spec, input, testCase, resolver)
        val expectedOutput = testCase.expectedOutput.map(string).getOrElse("")
        val expectedErrors = testCase.expectedRuntimeErrors.map(string).getOrElse("")
        compareRunResult(output, errors, expectedOutput, expectedErrors)
      case None =>
    }
  }

  def compareRunResult(actualOut: String, actualErr: String, expectedOut: String, expectedErr: String): Unit = {
    val expectedOutput = expectedOut.linesIterator.toSet.filterNot(_.isBlank)
    val expectedErrors = expectedErr.split("\n(?! )").toSet.filterNot(_.isBlank)
    val output = actualOut.linesIterator.toSet.filterNot(_.isBlank)
    val errors = actualErr.split("\n(?! )").toSet.filterNot(_.isBlank)

    assertTrue(!(errors.isEmpty && expectedErrors.nonEmpty), "Expected: Runtime error. Actual: success")
    assertTrue(
      !(errors.nonEmpty && expectedErrors.isEmpty),
      s"Expected: success, Actual: Runtime error:\n${errors.mkString("\n")}"
    )

    assertEqualSets(errors, expectedErrors)
    assertEqualSets(output.map(splitOutput), expectedOutput.map(splitOutput), unsplitOutput)
  }

  def splitOutput(line: String): (BigInt, String) = {
    val parts = line.split(":", 2)
    (BigInt(parts(0)), parts(1))
  }

  def unsplitOutput(pair: (BigInt, String)): String = s"${pair._1}:${pair._2}"

  def parseTestCase(path: String): TestConfig = {
    Using(Source.fromInputStream(AbstractTestRunner.getClass.getResourceAsStream(s"$path.json")))(
      _.mkString.parseJson.convertTo[TestConfig]
    ).get
  }

  def assertEqualSets[U: Ordering](
    actual: Set[U],
    expected: Set[U],
    stringify: U => String = (x: U) => x.toString
  ): Unit = {
    assertTrue(
      actual == expected,
      s"Actual\n${actual.toSeq.sorted.map(stringify).mkString("\n")}\n" +
        s"did not match expected\n${expected.toSeq.sorted.map(stringify).mkString("\n")}"
    )
  }

  def assertTrue(check: => Boolean, msg: => String): Unit = if (!check) fail(msg)
}

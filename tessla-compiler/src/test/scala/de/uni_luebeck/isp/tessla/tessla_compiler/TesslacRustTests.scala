/*
 * Copyright 2022 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.preprocessing._
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.{RustCompiler, RustFiles, TesslaCoreToRust}
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{InliningAnalysis, UsageAnalysis}
import de.uni_luebeck.isp.tessla.{AbstractTestRunner, TestCase}
import org.antlr.v4.runtime.CharStream
import org.scalatest.BeforeAndAfterAll

import java.lang.ProcessBuilder.Redirect
import java.nio.file.{Files, Path}
import scala.concurrent.duration.SECONDS
import scala.io.Source
import scala.reflect.io.Directory

class TesslacRustTests extends AbstractTestRunner[RustFiles]("Tessla Rust Compiler") with BeforeAndAfterAll {

  val fsPath: Path = Files.createTempDirectory("TesslaRustCompilerTests")

  override def roots = Seq("common/", "tesslac/", "tesslac_rust/") // TODO remove tesslac

  override def translation(
    testCase: TestConfig,
    resolver: PathResolver
  ): TranslationPhase[Core.Specification, RustFiles] =
    TesslacRustTests.pipeline(testCase, resolver)

  override def stdlibResolver: String => Option[CharStream] =
    CompilerStdLibIncludeResolver.fromCompilerStdlibResource

  override def run(
    spec: RustFiles,
    inputFile: String,
    testCase: TestCase.TestConfig,
    resolver: TestCase.PathResolver
  ): (String, String) = {
    val (output, errors) = TesslacRustTests.execute(fsPath, spec, resolver.inStreamFile(inputFile))
    (output.mkString("\n"), errors.mkString("\n"))
  }

  override def beforeAll(): Unit = {
    RustCompiler.exportLibrary(fsPath.resolve("rustlib"))
    Files.createDirectories(fsPath.resolve("src"))

    Files.writeString(
      fsPath.resolve("Cargo.toml"),
      s"""[workspace]
         |
         |[package]
         |name = "tessla_test"
         |version = "0.0.0"
         |
         |[dependencies]
         |tessla_stdlib = { path = "./rustlib" }
         |
         |[lib]
         |name = "monitor"
         |path = "src/monitor.rs"
         |""".stripMargin
    )
  }

  override def afterAll(): Unit = {
    Directory(fsPath.toFile).deleteRecursively()
  }

  override def compareRunResult(
    actualOut: String,
    actualErr: String,
    expectedOut: String,
    expectedErr: String
  ): Unit = {
    val expectedOutput = expectedOut.linesIterator.toSet.filterNot(_.isBlank)
    val expectedErrors = expectedErr.split("\n(?! )").toSet.filterNot(_.isBlank)
    val output = actualOut.linesIterator.toSet.filterNot(_.isBlank)
    val errors = actualErr.split("\n(?! )").toSet.filterNot(_.isBlank)

    assertTrue(!(errors.isEmpty && expectedErrors.nonEmpty), "Expected: Runtime error. Actual: success")
    assertTrue(
      !(errors.nonEmpty && expectedErrors.isEmpty),
      s"Expected: success, Actual: Runtime error:\n${errors.mkString("\n")}"
    )

    // Extract the error message since Rust prints additional stuff like the backtrace
    val expErrMsg = scala.collection.mutable.Set[String]()
    errors.foreach(ln => {
      if (!ln.startsWith("note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace")) {
        if (ln.startsWith("thread 'main' panicked at '")) {
          val spl = ln.split("',")(0).split("at '")(1)
          expErrMsg.add(spl)
        }
      }
    })

    assertEqualSets(expErrMsg.toSet, expectedErrors)
    assertEqualSets(output.map(splitOutput), expectedOutput.map(splitOutput), unsplitOutput)
  }
}

object TesslacRustTests {
  def pipeline(testCase: TestConfig, resolver: PathResolver): TranslationPhase[Core.Specification, RustFiles] = {
    val consoleInterface = !testCase.options.contains("no-console")
    if (!consoleInterface) {
      throw Diagnostics.NotYetImplementedError("Rust tests without the I/O interface are currently not supported")
    }
    val additionalSource = testCase.externalSource.map(resolver.string).getOrElse("")

    (SanitizeIdentifiers
      andThen new ExtractAndWrapFunctions
      andThen FormatStringMangler
      andThen GenerateStructDefinitions
      andThen UsageAnalysis
      andThen InliningAnalysis
      andThen InferGenericTypeTraits
      andThen new TesslaCoreToRust(additionalSource))
  }

  def execute(dirPath: Path, sourceCode: RustFiles, inputTrace: java.io.File): (Seq[String], Seq[String]) = {
    Files.writeString(dirPath.resolve("src/monitor.rs"), sourceCode.monitor)
    Files.writeString(dirPath.resolve("src/main.rs"), sourceCode.main)

    val cargoRun = new ProcessBuilder(
      "cargo",
      "run",
      "--quiet",
      "--manifest-path",
      dirPath.resolve("Cargo.toml").toAbsolutePath.toString,
      "--release"
    )

    // Which warnings we want to allow, any not specified here, will result in a failed test
    cargoRun.environment().put("RUSTFLAGS", RustCompiler.ALLOWED_WARNINGS)

    cargoRun.redirectInput(Redirect.from(inputTrace))

    try {
      val process = cargoRun.start()

      val out = process.getInputStream
      val err = process.getErrorStream

      process.waitFor(60, SECONDS)

      val output = Source.fromInputStream(out).getLines().toSeq
      val errors = Source.fromInputStream(err).getLines().toSeq

      (output, errors)
    } catch {
      case e: Exception =>
        // If this fails it failed to run the file, ergo no output
        (Seq(), Seq(e.getMessage))
    }
  }

}

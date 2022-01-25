/*
 * Copyright 2021 The TeSSLa Community
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

package de.uni_luebeck.isp.tessla.tessla_compiler;

import java.nio.file.{Files, Path}
import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.{RustCompiler, TesslaCoreToRust}
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{
  ExtractAndWrapFunctions,
  InliningAnalysis,
  UsageAnalysis
}
import de.uni_luebeck.isp.tessla.{AbstractTestRunner, TestCase}
import org.antlr.v4.runtime.CharStream
import org.scalatest.BeforeAndAfterAll

import java.lang.ProcessBuilder.Redirect
import scala.io.Source
import scala.reflect.io.Directory

class TesslacRustTests extends AbstractTestRunner[String]("Tessla Rust Compiler") with BeforeAndAfterAll {

  val fsPath: Path = Files.createTempDirectory("TesslaRustCompilerTests")

  override def roots = Seq("common/", "tesslac/")

  override def translation(testCase: TestConfig, resolver: PathResolver): TranslationPhase[Core.Specification, String] =
    TesslacRustTests.pipeline(testCase, resolver)

  override def stdlibResolver: String => Option[CharStream] =
    CompilerStdLibIncludeResolver.fromCompilerStdlibResource

  override def run(
    spec: String,
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
         |""".stripMargin
    )
  }

  override def afterAll(): Unit = {
    Directory(fsPath.toFile).deleteRecursively()
  }

}

object TesslacRustTests {
  def pipeline(testCase: TestConfig, resolver: PathResolver): TranslationPhase[Core.Specification, String] = {
    val consoleInterface = !testCase.options.contains("no-console")
    // FIXME: any additional source here would be in scala?
    val additionalSource = testCase.externalSource.map(resolver.string).getOrElse("")

    (new ExtractAndWrapFunctions)
      .andThen(UsageAnalysis)
      .andThen(InliningAnalysis)
      .andThen(FormatStringMangler)
      .andThen(new TesslaCoreToRust(consoleInterface))
  }

  def execute(dirPath: Path, sourceCode: String, inputTrace: java.io.File): (Seq[String], Seq[String]) = {
    val sourcePath = dirPath.resolve("src/main.rs")
    Files.writeString(sourcePath, sourceCode)

    val cargoRun = new ProcessBuilder(
      "cargo",
      "run",
      "--quiet",
      "--manifest-path",
      dirPath.resolve("Cargo.toml").toAbsolutePath.toString,
      "--release"
    )

    // Which warnings we want to allow, any not specified here, will result in a failed test
    cargoRun
      .environment()
      .put(
        "RUSTFLAGS",
        "-A unused_parens -A unused_variables -A non_snake_case -A non_camel_case_types -A uncommon_codepoints -A non_upper_case_globals"
      )

    cargoRun.redirectInput(Redirect.from(inputTrace))

    try {
      val process = cargoRun.start()

      val out = process.getInputStream
      val err = process.getErrorStream

      process.waitFor()

      out.markSupported()

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

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
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.TesslaCoreToRust
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{InliningAnalysis, UsageAnalysis}
import de.uni_luebeck.isp.tessla.{AbstractTestRunner, TestCase}
import org.antlr.v4.runtime.CharStream
import org.scalatest.BeforeAndAfterAll

import java.lang.ProcessBuilder.Redirect
import scala.io.Source
import scala.reflect.io.Directory

class TesslacRustTests extends AbstractTestRunner[String]("Tessla Rust Compiler") with BeforeAndAfterAll {

  val fsPath: Path = Files.createTempDirectory("TesslaRustCompilerTests")

  var staticLibrary: String = _

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
    TesslacRustTests.compile(fsPath, spec, staticLibrary)
    val (output, errors) = TesslacRustTests.execute(fsPath, resolver.inStreamFile(inputFile))
    (output.mkString("\n"), errors.mkString("\n"))
  }

  override def beforeAll(): Unit = {
    // bundle rust library
    val ioLib = Source.fromResource("de/uni_luebeck/isp/tessla/stdlib/rust/src/io.rs").getLines()
    val valueLib = Source.fromResource("de/uni_luebeck/isp/tessla/stdlib/rust/src/value.rs").getLines()
    val streamLib = Source
      .fromResource("de/uni_luebeck/isp/tessla/stdlib/rust/src/stream.rs")
      .getLines()
      .filterNot(l => l.startsWith("use crate"))

    staticLibrary = (ioLib ++ valueLib ++ streamLib).mkString("\n")
  }

  override def afterAll(): Unit = {
    Directory(fsPath.toFile).deleteRecursively()
  }

}

object TesslacRustTests {
  def pipeline(testCase: TestConfig, resolver: PathResolver): TranslationPhase[Core.Specification, String] = {
    val consoleInterface = !testCase.options.contains("no-console")
    // FIXME: any additional source here would be in scala
    val additionalSource = testCase.externalSource.map(resolver.string).getOrElse("")
    assert(additionalSource == "")

    UsageAnalysis
      .andThen(InliningAnalysis)
      .andThen(new TesslaCoreToRust(consoleInterface))
  }

  // Search for Main$ in the provided path and execute Main$.main with the input trace.
  def execute(path: Path, inputTrace: java.io.File): (Seq[String], Seq[String]) = {
    val runner = new ProcessBuilder(path.resolve("out").toString)

    runner.redirectInput(Redirect.from(inputTrace))
    runner.redirectError(Redirect.PIPE)
    runner.redirectOutput(Redirect.PIPE)

    try {
      val process = runner.start()
      process.waitFor()

      val out = process.getInputStream // yes this is the correct one
      val err = process.getErrorStream

      val output = Source.fromInputStream(out).getLines().toSeq
      val errors = Source.fromInputStream(err).getLines().toSeq

      (output, errors)
    } catch {
      case e: Exception =>
        // If this fails it failed to run the file, ergo no output
        (Seq(), Seq(e.getMessage))
    }
  }

  def compile(dirPath: Path, sourceCode: String, libraryCode: String): Unit = {
    val standaloneCode = sourceCode.replace("extern crate tessla_stdlib;\nuse tessla_stdlib::*;", libraryCode)

    val sourcePath = dirPath.resolve("out.rs")
    Files.writeString(sourcePath, standaloneCode)

    val rustc = new ProcessBuilder("rustc", sourcePath.toString).start()
    //val rustc = new ProcessBuilder("cargo", "build", "--bin", "out"). /* TODO: ?? */ inheritIO().start()
    rustc.waitFor()
  }

}

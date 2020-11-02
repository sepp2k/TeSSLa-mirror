package de.uni_luebeck.isp.tessla.tessla_compiler

import java.nio.file.{Files, Path}

import de.uni_luebeck.isp.tessla.AbstractTestRunner
import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend.ScalaCompiler
import org.scalatest.BeforeAndAfterAll

import scala.reflect.io.Directory

/**
 * Tests the generation of an executable jar from Scala code
 */
class JarCreationTest extends AbstractTestRunner[Unit]("Jar") with BeforeAndAfterAll {

  val fsPath: Path = Files.createTempDirectory("TesslaCompilerJarCreationTests")

  override def roots: Seq[String] = Seq("jar/")

  override def translation(testCase: TestConfig): TranslationPhase[Core.Specification, Unit] =
    TesslacTests
      .pipeline(testCase)
      .andThen(new ScalaCompiler(fsPath, "monitor.jar", false)(s => {
        s.usejavacp.value = false
        s.classpath.value = Predef.getClass.getProtectionDomain.getCodeSource.getLocation.getPath
      }))

  override def run(
    spec: Unit,
    inputFile: String,
    testCase: TestConfig,
    resolver: PathResolver
  ): (String, String) = {

    val input = resolver.inStream(inputFile)
    val (output, errors) = TesslacTests.execute(fsPath.resolve("monitor.jar"), input)
    (output.mkString("\n"), errors.mkString("\n"))
  }

  override def afterAll(): Unit = {
    Directory(fsPath.toFile).deleteRecursively()
  }
}

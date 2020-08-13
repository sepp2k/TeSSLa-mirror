package de.uni_luebeck.isp.tessla.tessla_compiler

import java.io.File
import java.nio.file.{Files, Path}

import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend.ScalaCompiler
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers}
import org.scalatest.funsuite.AnyFunSuite

import scala.sys.process.Process

/**
 * Tests the generation of an executable jar from Scala code
 */
class JarCreationTest extends AnyFunSuite {

  test("JarCreationTest (Compiler, Scala)") {

    val fsPath: Path = Files.createTempDirectory("TesslaCompilerJarCreationTests")

    val options = Compiler.Options(
      baseTimeString = None,
      includeResolver = IncludeResolvers.empty,
    )

    //We just perform a simple testcase to see if the jar generation works
    CompilerTests
      .compileChain(
        IncludeResolvers.fromResource(getClass, "tests/")(s"operators/lifted-functions/integer/addition.tessla").get,
        new Compiler(options)
      )
      .andThen(new ScalaCompiler(fsPath, "monitor.jar"))

    val executionLogger = new CompilerTests.TestProcessLogger
    CompilerTests.writeToFile(fsPath.toAbsolutePath.toString + "/input", "0:x=3\n3:y=4")

    val run = Process("cat input", new File(fsPath.toAbsolutePath.toString)) #|
      Process(s"java -jar monitor.jar", new File(fsPath.toAbsolutePath.toString))
    val rc = run.!(executionLogger)

    assert(rc == 0)
    assert(executionLogger.output == Seq("3: z = 7"))
  }

}

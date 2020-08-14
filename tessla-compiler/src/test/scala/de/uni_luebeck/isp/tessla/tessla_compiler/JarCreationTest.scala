package de.uni_luebeck.isp.tessla.tessla_compiler

import java.nio.file.{Files, Path}

import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend.ScalaCompiler
import de.uni_luebeck.isp.tessla.core.{Compiler, IncludeResolvers}
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.tools.nsc.Settings

/**
 * Tests the generation of an executable jar from Scala code
 */
class JarCreationTest extends AnyFunSuite {

  test("JarCreationTest (Compiler, Scala)") {

    val fsPath: Path = Files.createTempDirectory("TesslaCompilerJarCreationTests")

    val options = Compiler.Options(
      baseTimeString = None,
      includeResolver = IncludeResolvers.empty
    )

    //We just perform a simple testcase to see if the jar generation works
    val settings = new Settings()

    CompilerTests
      .compileChain(
        CharStreams.fromStream(getClass.getResourceAsStream("jar/jar.tessla")),
        new Compiler(options)
      )
      .andThen(new ScalaCompiler(fsPath, "monitor.jar", false)(s => {
        s.usejavacp.value = false
        s.classpath.value = Predef.getClass.getProtectionDomain.getCodeSource.getLocation.getPath
      }))

    val input = getClass.getResourceAsStream("jar/jar.in")
    val (output, _, isError) = CompilerTests.execute(fsPath.resolve("monitor.jar"), input)

    val expected = Source.fromInputStream(getClass.getResourceAsStream("jar/jar.out")).getLines().toSeq
    assert(!isError)
    assert(output == expected)
  }

}

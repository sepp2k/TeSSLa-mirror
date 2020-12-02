/*
 * Copyright 2020 The TeSSLa Community
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

import java.io.{ByteArrayOutputStream, InputStream, PrintStream}
import java.lang.reflect.InvocationTargetException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import de.uni_luebeck.isp.tessla.TestCase.{PathResolver, TestConfig}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.tessla_compiler.NoExitSecurityManager.SystemExitException
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend.{
  ScalaBackend,
  ScalaCompiler,
  TesslaCompilerReporter
}
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.{Laziness, UsageAnalysis}
import de.uni_luebeck.isp.tessla.{AbstractTestRunner, TestCase}
import org.scalatest.BeforeAndAfterAll

import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.reflect.io.Directory
import scala.tools.nsc.Global

class TesslacTests extends AbstractTestRunner[String]("Tessla Compiler") with BeforeAndAfterAll {

  val fsPath: Path = Files.createTempDirectory("TesslaCompilerTests")

  override def roots = Seq("common/", "tesslac/")

  override def translation(testCase: TestConfig, resolver: PathResolver): TranslationPhase[Core.Specification, String] =
    TesslacTests.pipeline(testCase, resolver)

  override def run(
    spec: String,
    inputFile: String,
    testCase: TestCase.TestConfig,
    resolver: TestCase.PathResolver
  ): (String, String) = {
    TesslacTests.compile(fsPath, spec)
    val (output, errors) = TesslacTests.execute(fsPath, resolver.inStream(inputFile))
    (output.mkString("\n"), errors.mkString("\n"))
  }

  override def afterAll(): Unit = {
    Directory(fsPath.toFile).deleteRecursively()
  }

}

object TesslacTests {
  def pipeline(testCase: TestConfig, resolver: PathResolver): TranslationPhase[Core.Specification, String] = {
    val consoleInterface = !testCase.options.contains("no-console")
    val additionalSource = testCase.externalSource.map(resolver.string).getOrElse("")

    UsageAnalysis
      .andThen(Laziness)
      .andThen(new TesslaCoreToIntermediate(consoleInterface))
      .andThen(UnusedVarRemove)
      .andThen(new ScalaBackend(consoleInterface, additionalSource))
  }

  // Redirect both Scala and Java Console IO to the provided streams, and revert afterwards
  // TODO: This is required since the tessla-compiler currently only supports IO through stdin/stdout/stderr
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
  def execute(path: Path, inputTrace: InputStream): (Seq[String], Seq[String]) = {
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
      withRedirectedIO(inputTrace, new PrintStream(out), new PrintStream(err))(
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

    (output, filtered)
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

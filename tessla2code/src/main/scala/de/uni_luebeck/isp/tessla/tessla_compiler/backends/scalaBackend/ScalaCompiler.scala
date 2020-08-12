package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import java.io.{FileOutputStream, InputStream, OutputStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import java.util.jar.{Attributes, JarEntry, JarInputStream, JarOutputStream}

import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics.CompilationWarning
import de.uni_luebeck.isp.tessla.{Diagnostic, Errors, TranslationPhase}

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.{FilteringReporter, Reporter}
import scala.tools.nsc.{Global, Settings}
import scala.util.Using
import scala.reflect.internal.util.Position.formatMessage

/**
 * [[TranslationPhase]] generating a monitor as fat jar from Scala code
 * @param outDir The directory where the jar shall be created.
 *               All temporary files will aslo be created in this directory.
 *               This directory has to exist.
 * @param jarName The name of the generated jar file
 */
class ScalaCompiler(outDir: Path, jarName: String) extends TranslationPhase[String, Unit] {

  /**
   * Function triggering the translation from a Scala source string to a jar archive which is created at the given
   * location
   * @param sourceCode The source code to be compiled
   * @return Unit wrapped in a Result data structure
   */
  def translate(sourceCode: String): Result[Unit] = {

    Files.createDirectories(outDir)

    // Generated source files go here
    val sourceDir = Files.createTempDirectory(outDir, "source")

    // Compiled and unzipped .class files go here
    val compileDir = Files.createTempDirectory(outDir, "compile")

    deleteOnExit(compileDir)
    deleteOnExit(sourceDir)

    val settings = createSettings(compileDir)
    val reporter = new TesslaCompilerReporter(settings)

    val sourcePath = Path.of(URI.create(outDir.toUri.toString + "Main.scala"))
    deleteOnExit(sourcePath)

    writeCode(sourcePath, sourceCode)
    compileCode(sourcePath, settings, reporter)

    unzipJar(compileDir)
    makeJar(compileDir, outDir.resolve(jarName), "Main")

    Success((), reporter.warnings.toSeq)
  }

  /**
   * Creates a [[Settings]] instance for the scala compilation
   * @param compileDir The working directory of the compilation
   * @return [[Settings]] to be used for the compilation
   */
  private def createSettings(compileDir: Path): Settings = {

    val settings = new Settings()
    settings.usejavacp.value = true
    settings.outputDirs.setSingleOutput(compileDir.toAbsolutePath.toString)
    settings.optimise.value = true

    settings
  }

  /**
   * Method triggering compilation of a single Scala source file to class file(s)
   * @param source The path of the source file to be compiled
   * @param settings The compilation settings
   * @param reporter The reporter getting attached to the compiler
   */
  private def compileCode(source: Path, settings: Settings, reporter: Reporter): Unit = {
    val compiler = new Global(settings, reporter)
    (new compiler.Run) compile List(source.toAbsolutePath.toString)
    reporter.finish()
  }

  /**
   * Writes the content of a string to a file
   * @param path The file to be written
   * @param source The file's content
   */
  private def writeCode(path: Path, source: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.write(path, source.getBytes(StandardCharsets.UTF_8))
  }

  /**
   * Unzips all files starting with scala (those in the scala directory/package) from the scala-library.jar
   * to a certain directory
   *
   * @param dir Directory where files are extracted to
   */
  private def unzipJar(dir: Path): Unit = {
    // Fetch the scala-library.jar location
    val source = Predef.getClass.getProtectionDomain.getCodeSource
    if (source == null) {
      throw Errors.InternalError("Resource scala-library.jar could not be loaded. Monitor generation failed.")
    }
    Using(source.getLocation.openStream)(unzipJar(dir, _))
  }

  /**
   * Extracts all files starting with scala (those in the scala directory/package) from a jar file to a directory on
   * the system
   * @param dir Directory where files are extracted to
   * @param source The input stream of the jar file
   */
  private def unzipJar(dir: Path, source: InputStream): Unit = {
    val in = new JarInputStream(source)
    LazyList
      .continually(in.getNextJarEntry)
      .takeWhile(_ != null)
      .filterNot(_.isDirectory)
      // This filters only scala sources, as on the assembled
      // variant the source would resolve to the assembly itself, thus repackaging itself into the
      // generated code
      // TODO This is only a quick and ugly workaround, especially since this drops license information!
      // Solution 1: More fine-grained filter to only include what's needed (this also requires manually filtering out
      //   scala-compiler and scala-reflect classes
      // Solution 2: Include scala-library as separate resource, easier to include but decoupled version of library and compiler
      //   and scala-library effectively contained twice in the assembled compiler (not in the generated result of course)
      .filter(_.getName.startsWith("scala"))
      .foreach { file =>
        val f = Paths.get(file.getName)
        val path = dir.resolve(f)
        Files.createDirectories(path.getParent)
        val out = new FileOutputStream(path.toFile)
        transfer(in, out)
      }
  }

  /**
   * Helper method that pipes from an input to an output stream
   * @param in The input stream
   * @param out The output stream
   * @param bufSize The size of the used buffer
   */
  private def transfer(in: InputStream, out: OutputStream, bufSize: Int = 1024) = {
    val buffer = new Array[Byte](bufSize)
    LazyList.continually(in.read(buffer)).takeWhile(_ != -1).foreach(out.write(buffer, 0, _))
  }

  /**
   * Packs (compiled) sources to an executable jar file
   * @param sources The path where the sources are located
   * @param outPath The path of the generated jar file
   * @param mainClass The main class to be executed when launching the jar
   */
  private def makeJar(sources: Path, outPath: Path, mainClass: String): Unit = {
    // Create manifest and its attributes
    val manifest = new java.util.jar.Manifest
    val attributes = manifest.getMainAttributes.asScala
    attributes.put(Attributes.Name.MAIN_CLASS, mainClass)
    attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")

    val out = new JarOutputStream(Files.newOutputStream(outPath), manifest)

    // Walk filesystem recursively and write to the archive
    // Skip first since that's the root itself
    Files.walk(sources).skip(1).forEach { f =>
      val rel = sources.relativize(f).toString.replace('\\', '/')
      if (f.toFile.isDirectory) {
        out.putNextEntry(new JarEntry(s"$rel/"))
        out.closeEntry()
      } else {
        val content = Files.readAllBytes(f)
        val e = new JarEntry(rel)
        out.putNextEntry(e)
        out.write(content, 0, content.length)
        out.closeEntry()
      }
    }
    out.close()
  }

  /**
   * Deletes a file on program shutdown.
   * @param path Path of the file to be deleted
   */
  private def deleteOnExit(path: Path): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      Files.walk(path).sorted(Comparator.reverseOrder).forEach(Files.delete _)
    }))
  }

  /**
   * A [[Reporter]] implementation raising TeSSLa compiler errors if the supervised Scala compiler raises an error
   * and collects TeSSLa warnings if it raises warnings or information
   * @param settings Settings passed to the extended [[FilteringReporter]]
   */
  class TesslaCompilerReporter(val settings: Settings) extends FilteringReporter {

    val warnings: collection.mutable.ArrayBuffer[Diagnostic] = collection.mutable.ArrayBuffer()

    override def doReport(pos: Position, msg: String, severity: Severity): Unit = {
      val combMsg = formatMessage(pos, msg, false)
      severity match {
        case reflect.internal.Reporter.INFO    => warnings += CompilationWarning(combMsg, "scalac", "info")
        case reflect.internal.Reporter.WARNING => warnings += CompilationWarning(combMsg, "scalac", "warning")
        case reflect.internal.Reporter.ERROR =>
          throw Errors.InternalError(s"Scala Compilation raised error, compilation aborted:\n$combMsg")
        case _ =>
          throw Errors.InternalError(
            s"Scala Compilation raised error of unknown Severity, compilation aborted:\n$combMsg"
          )
      }
    }
  }

}

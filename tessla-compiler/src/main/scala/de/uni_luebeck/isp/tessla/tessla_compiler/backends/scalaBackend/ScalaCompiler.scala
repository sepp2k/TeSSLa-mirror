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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import java.io.{FileOutputStream, InputStream, OutputStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{Attributes, JarEntry, JarInputStream, JarOutputStream}

import de.uni_luebeck.isp.tessla.core.{Errors, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}

import scala.jdk.CollectionConverters._
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.{Global, Settings}
import scala.util.Using
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend.ScalaCompiler._

import scala.reflect.io.Directory

object ScalaCompiler {

  /**
   * Creates a Settings instance for the scala compilation
   * @param compileDir The working directory of the compilation
   * @return Settings to be used for the compilation
   */
  def defaultSettings(compileDir: Path, debug: Boolean): Settings = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.outputDirs.setSingleOutput(compileDir.toAbsolutePath.toString)
    settings.opt.enable(settings.optChoices.lDefault)
    settings.opt.enable(settings.optChoices.lMethod)
    if (debug) {
      settings.verbose.value = true
      settings.Ylogcp.value = true
    }

    settings
  }

  /**
   * Method triggering compilation of a single Scala source file to class file(s)
   * @param source The path of the source file to be compiled
   * @param settings The compilation settings
   * @param reporter The reporter getting attached to the compiler
   */
  def compileCode(source: Path, settings: Settings, reporter: Reporter): Unit = {
    val compiler = new Global(settings, reporter)
    (new compiler.Run) compile List(source.toAbsolutePath.toString)
    reporter.finish()
  }

  /**
   * Unzips all files from the scala-library.jar
   * to a certain directory
   *
   * @param dir Directory where files are extracted to
   */
  def unzipScalaLibrary(dir: Path): Unit = {
    // Fetch the scala-library.jar location
    val source = getClass.getClassLoader.getResource("scala-library.jar")
    if (source == null) {
      throw Errors.InternalError("Resource scala-library.jar could not be loaded. Monitor generation failed.")
    }
    Using(source.openStream)(unzipJar(dir, _))
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
      .foreach { file =>
        val f = Paths.get(file.getName)
        val path = dir.resolve(f)
        Files.createDirectories(path.getParent)
        val out = new FileOutputStream(path.toFile)
        transfer(in, out)
      }
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

    if (mainClass != "")
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
   * Helper method that pipes from an input to an output stream
   * @param in The input stream
   * @param out The output stream
   * @param bufSize The size of the used buffer
   */
  private def transfer(in: InputStream, out: OutputStream, bufSize: Int = 1024): Unit = {
    val buffer = new Array[Byte](bufSize)
    LazyList.continually(in.read(buffer)).takeWhile(_ != -1).foreach(out.write(buffer, 0, _))
  }
}

/**
 * TranslationPhase generating a monitor as fat jar from Scala code
 * @param outDir The directory where the jar shall be created.
 *               All temporary files will aslo be created in this directory.
 *               This directory has to exist.
 * @param jarName The name of the generated jar file
 * @param debug Make compilation more verbose
 * @param executeableCode Indicates whether passed source contains a Main.main method which is added to the jar's
 *                        manifest
 * @param settingsModifier Function adjusting the settings for compilation
 */
class ScalaCompiler(outDir: Path, jarName: String, debug: Boolean, executeableCode: Boolean)(
  settingsModifier: Settings => Unit = _ => ()
) extends TranslationPhase[String, Unit] {

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

    val settings = defaultSettings(compileDir, debug)
    settingsModifier(settings)
    val reporter = new ConsoleReporter(settings)

    val sourcePath = Path.of(URI.create(outDir.toUri.toString + "Main.scala"))
    deleteOnExit(sourcePath)

    //println(s"Write code to $sourcePath ...")
    writeCode(sourcePath, sourceCode)
    compileCode(sourcePath, settings, reporter)

    //println(s"Unzip scala library to $compileDir ...")
    unzipScalaLibrary(compileDir)
    //println(s"Pack $compileDir into jar ...")
    val jar = outDir.resolve(jarName)
    makeJar(compileDir, jar, if (executeableCode) "Main" else "")
    //println(s"Successfully generated jar file at $jar.")

    Success((), Seq())
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
   * Deletes a file on program shutdown.
   * @param path Path of the file to be deleted
   */
  private def deleteOnExit(path: Path): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      Directory(path.toFile).deleteRecursively()
    }))
  }

}

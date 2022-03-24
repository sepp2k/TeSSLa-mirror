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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

import de.uni_luebeck.isp.tessla.core.Errors.{mkTesslaError, TesslaError}
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._
import java.util.Collections
import scala.io.Source
import scala.reflect.io.Directory

/**
 * Invokes rustc to generate a binary artifact from Rust code.
 *
 * @param outDir The directory where the binary artifact shall be created.
 * @param binaryArtifactName The name of the generated artifact.
 * @param executableCode Indicates whether passed source contains a main method
 */
class RustCompiler(outDir: Path, binaryArtifactName: String, executableCode: Boolean)
    extends TranslationPhase[String, Unit] {

  /**
   * Invokes rustc to translate a Rust source string into a binary artifact.
   * @param sourceCode The source code to compile.
   * @return Unit wrapped in a Result data structure.
   */
  override def translate(sourceCode: String): TranslationPhase.Result[Unit] = {
    Files.createDirectories(outDir)

    val cargoDir = outDir.resolve(s"build-$binaryArtifactName")
    Files.createDirectories(cargoDir)
    deleteOnExit(cargoDir)

    val libPath = cargoDir.resolve("rustlib")
    if (!Files.exists(libPath)) {
      RustCompiler.exportLibrary(libPath)
    }

    val sourcePath = cargoDir.resolve("src")
    Files.createDirectories(sourcePath)

    val targetName = if (executableCode) "main.rs" else "lib.rs"
    writeCode(sourcePath.resolve(targetName), sourceCode)

    val cargoPath = cargoDir.resolve("Cargo.toml")
    Files.writeString(
      cargoPath,
      s"""[workspace]
         |
         |[package]
         |name = "tessla_monitor"
         |version = "0.0.0"
         |
         |[dependencies]
         |tessla_stdlib = { path = "./rustlib" }
         |""".stripMargin
    )

    val cargoManifest = cargoPath.toAbsolutePath.toString
    val cargoBuild = if (executableCode) {
      new ProcessBuilder(
        "cargo",
        "build",
        "--manifest-path",
        cargoManifest,
        "--release"
      )
    } else {
      new ProcessBuilder(
        "cargo",
        "build",
        "--manifest-path",
        cargoManifest,
        "--lib",
        "--release"
      )
    }

    cargoBuild.redirectInput()
    cargoBuild.redirectOutput()
    cargoBuild.redirectError()

    try {
      val cargoProcess = cargoBuild.start()
      val returnCode = cargoProcess.waitFor()

      if (returnCode != 0) {
        val errors = Source.fromInputStream(cargoProcess.getErrorStream).getLines().toSeq
        val error = mkTesslaError(s"Cargo build failed:\n${errors.mkString("\n")}")
        throw error
      }
    } catch {
      case e: TesslaError => throw e
      case e: Exception =>
        val error = mkTesslaError(s"Cargo failed to run: ${e.getMessage}")
        error.setStackTrace(e.getStackTrace)
        throw error
    }

    try {
      Files.copy(
        cargoDir.resolve("target/release/tessla_monitor"),
        outDir.resolve(binaryArtifactName),
        StandardCopyOption.REPLACE_EXISTING
      )
    } catch {
      case e: Exception =>
        val error = mkTesslaError(s"Failed to copy binary: ${e.getMessage}")
        error.setStackTrace(e.getStackTrace)
        throw error
    }

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

object RustCompiler {

  /**
   * Exports the rust library crate from resources into an external folder
   * @param destination the destination folder
   */
  def exportLibrary(destination: Path): Unit = {
    val libraryLocation = "de/uni_luebeck/isp/tessla/rust/stdlib"
    val libraryURI = getClass.getClassLoader.getResource(libraryLocation).toURI

    if ("jar".equals(libraryURI.getScheme)) {
      var jarFS: FileSystem = null
      try {
        jarFS = FileSystems.newFileSystem(libraryURI, Collections.emptyMap(), getClass.getClassLoader)
        val libraryPath = jarFS.getPath(libraryLocation)
        Files.walkFileTree(libraryPath, new CopyFileVisitor(libraryPath, destination))
      } catch {
        case e: Exception => e.printStackTrace()
      } finally {
        jarFS.close()
      }
    } else {
      val libraryPath = Path.of(libraryURI)
      Files.walkFileTree(libraryPath, new CopyFileVisitor(libraryPath, destination))
    }
  }

  /**
   * Helper class to copy a file tree into another folder with [[Files.walkFileTree]]
   * @param sourceBase the source path
   * @param destination the destination path
   * @throws FileAlreadyExistsException if any of the files already exist in the destination
   */
  private class CopyFileVisitor(sourceBase: Path, destination: Path) extends SimpleFileVisitor[Path] {
    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
      val check = super.preVisitDirectory(dir, attrs)
      val destinationDir = destination.resolve(sourceBase.relativize(dir).toString)
      Files.createDirectory(destinationDir)
      check
    }

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      val check = super.visitFile(file, attrs)
      val destinationFile = destination.resolve(sourceBase.relativize(file).toString)
      Files.copy(file, destinationFile)
      check
    }
  }
}

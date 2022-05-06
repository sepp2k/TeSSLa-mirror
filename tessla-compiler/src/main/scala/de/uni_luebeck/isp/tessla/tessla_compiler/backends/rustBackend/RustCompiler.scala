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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

import de.uni_luebeck.isp.tessla.core.Errors.{mkTesslaError, TesslaError}
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

import java.io.{File, IOException}
import java.lang.ProcessBuilder.Redirect
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.Collections
import scala.io.Source

/**
 * Invokes cargo to generate a binary artifact from Rust code.
 *
 * @param artifactPath A path where the final monitor binary will be copied to.
 */
class RustCompiler(artifactPath: Path) extends TranslationPhase[RustFiles, Unit] {

  /**
   * Invokes cargo to translate a collection of [[RustFiles]] into a binary artifact.
   * @param sourceCode The source code to compile.
   * @return Unit wrapped in a Result data structure.
   */
  override def translate(sourceCode: RustFiles): TranslationPhase.Result[Unit] = {
    val (artifactDir, artifactName) = artifactPath match {
      case path if path.toFile.isDirectory =>
        (path, "tessla_monitor")
      case path =>
        (path.getParent, path.toFile.getName)
    }

    val workspaceDir = Files.createTempDirectory("build-tessla-rust")

    RustCompiler.createCargoWorkspace(workspaceDir, artifactName, sourceCode, exportMain = true)

    val cargoBuild = new ProcessBuilder(
      "cargo",
      "build",
      "--manifest-path",
      workspaceDir.resolve("Cargo.toml").toAbsolutePath.toString,
      "--workspace",
      "--release"
    )

    // Which warnings we want to allow, to de-clutter the cargo output
    cargoBuild.environment().put("RUSTFLAGS", RustCompiler.ALLOWED_WARNINGS)

    // this is equivalent to Redirect.DISCARD, available from from Java 9+
    cargoBuild.redirectOutput(Redirect.to(RustCompiler.NULL_FILE))
    cargoBuild.redirectError(Redirect.PIPE)

    // Run cargo build
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

    // Copy the resulting artifact
    try {
      // When "cargo build --out-dir <PATH>" is finalized this step can be removed
      // https://github.com/rust-lang/cargo/issues/6790
      Files.copy(
        workspaceDir.resolve(s"target/release/$artifactName"),
        artifactDir.resolve(artifactName),
        StandardCopyOption.REPLACE_EXISTING
      )
    } catch {
      case e: Exception =>
        val error = mkTesslaError(s"Failed to copy binary: ${e.getMessage}")
        error.setStackTrace(e.getStackTrace)
        throw error
    }

    // delete the workspace dir, if no errors occurred
    Files.walkFileTree(workspaceDir, RustCompiler.DeleteFileVisitor)

    Success((), Seq())
  }
}

object RustCompiler {

  /**
   * A set of warnings that we know are present in the generated output
   * Some of these are merely stylistic (variable names) and some are more interesting,
   * but very hard to eliminate (too many parentheses, unused variables, dead code)
   */
  final var ALLOWED_WARNINGS: String =
    "-A unused_parens -A unused_variables -A non_snake_case -A non_camel_case_types -A uncommon_codepoints -A non_upper_case_globals -A dead_code"

  final private val NULL_FILE: File = new File(
    if (System.getProperty("os.name").startsWith("Windows")) "NUL" else "/dev/null"
  )

  /**
   * Creates a complete cargo workspace with all files necessary to expand it with your own code.
   *
   * The Cargo.toml only gets written if it doesn't exist.
   *
   * @param workspaceDir The directory the workspace is put into.
   * @param artifactName The name of the project and thus the name of the default src/main.rs binary
   * @param sourceCode The source code to be exported.
   * @param exportMain Whether the default main.rs IO interface should be created
   */
  def createCargoWorkspace(
    workspaceDir: Path,
    artifactName: String,
    sourceCode: RustFiles,
    exportMain: Boolean
  ): Unit = {
    Files.createDirectories(workspaceDir)

    val libPath = workspaceDir.resolve("lib_tessla")
    RustCompiler.exportLibrary(libPath)

    val sourcePath = workspaceDir.resolve("src")
    Files.createDirectories(sourcePath)

    Files.writeString(
      sourcePath.resolve("monitor.rs"),
      sourceCode.monitor,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.CREATE
    )

    if (exportMain) {
      // This is the default binary path, therefore it is not specifically configured in the Cargo.toml
      // https://doc.rust-lang.org/cargo/reference/cargo-targets.html#binaries

      Files.writeString(
        sourcePath.resolve("main.rs"),
        sourceCode.main,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.CREATE
      )
    }

    val manifestPath = workspaceDir.resolve("Cargo.toml")
    if (!manifestPath.toFile.exists()) {
      Files.writeString(
        manifestPath,
        s"""[workspace]
           |
           |[package]
           |name = "$artifactName"
           |version = "0.0.0"
           |
           |[dependencies]
           |tessla_stdlib = { path = "./lib_tessla" }
           |
           |[lib]
           |name = "monitor"
           |path = "src/monitor.rs"
           |""".stripMargin
      )
    }
  }

  /**
   * Exports the rust library crate from resources into an external folder
   * @param destination the destination folder
   */
  def exportLibrary(destination: Path): Unit = {
    val libraryLocation = "de/uni_luebeck/isp/tessla/rust/stdlib"
    val libraryURI = getClass.getClassLoader.getResource(libraryLocation).toURI

    def copyLibToDest(sourcePath: Path): Unit = {
      // delete any previously existing files and folders
      if (destination.toFile.exists()) {
        Files.walkFileTree(destination, DeleteFileVisitor)
      }
      Files.walkFileTree(sourcePath, new CopyFileVisitor(sourcePath, destination))
    }

    if ("jar".equals(libraryURI.getScheme)) {
      var jarFS: FileSystem = null
      try {
        jarFS = FileSystems.newFileSystem(libraryURI, Collections.emptyMap(), getClass.getClassLoader)
        copyLibToDest(jarFS.getPath(libraryLocation))
      } catch {
        case e: Exception => e.printStackTrace()
      } finally {
        jarFS.close()
      }
    } else {
      copyLibToDest(Path.of(libraryURI))
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
      Files.copy(
        file,
        destinationFile,
        StandardCopyOption.REPLACE_EXISTING,
        StandardCopyOption.COPY_ATTRIBUTES
      )
      check
    }
  }

  /**
   * Helper class to delete a file tree with [[Files.walkFileTree]]
   */
  private object DeleteFileVisitor extends SimpleFileVisitor[Path] {
    override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
      val check = super.postVisitDirectory(dir, exc)
      Files.delete(dir)
      check
    }

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      val check = super.visitFile(file, attrs)
      Files.delete(file)
      check
    }
  }
}

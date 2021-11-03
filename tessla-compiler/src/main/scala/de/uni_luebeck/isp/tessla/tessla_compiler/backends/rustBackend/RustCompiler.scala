package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.reflect.io.Directory

/**
 * Invokes rustc to generate a binary artifact from Rust code.
 *
 * @param outDir The directory where the binary artifact shall be created.
 * @param binaryArtifactName The name of the generated artifact.
 */
class RustCompiler(outDir: Path, binaryArtifactName: String) extends TranslationPhase[String, Unit] {

  /**
   * Invokes rustc to translate a Rust source string into a binary artifact.
   * @param sourceCode The source code to compile.
   * @return Unit wrapped in a Result data structure.
   */
  override def translate(sourceCode: String): TranslationPhase.Result[Unit] = {
    Files.createDirectories(outDir)

    val sourcePath = Path.of(URI.create(outDir.toUri.toString + "main.rs"))
    deleteOnExit(sourcePath)

    writeCode(sourcePath, sourceCode)

    new ProcessBuilder("rustc", sourcePath.toString).inheritIO().start().waitFor()

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
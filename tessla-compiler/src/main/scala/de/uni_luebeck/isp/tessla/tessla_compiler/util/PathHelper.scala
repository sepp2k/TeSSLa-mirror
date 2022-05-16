package de.uni_luebeck.isp.tessla.tessla_compiler.util

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

/**
 * Helper class for recursive path deletion
 */
object PathHelper {

  /**
   * Deletes a path recursively
   * @param path Path of the file to be deleted
   */
  def deleteRecursively(path: Path): Unit = Files.walkFileTree(path, DeleteFileVisitor)

  /**
   * Deletes a path recursively on program shutdown.
   * @param path Path of the file to be deleted
   */
  def deleteOnExit(path: Path): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread(() => deleteRecursively(path)))
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

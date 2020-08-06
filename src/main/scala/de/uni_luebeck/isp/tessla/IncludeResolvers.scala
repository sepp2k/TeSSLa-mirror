package de.uni_luebeck.isp.tessla

import java.nio.channels.Channels
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.{CharStream, CharStreams}
import util._

/**
 * Provides multiple include resolvers, for use in [[Compiler.Options]]
 */

object IncludeResolvers {

  def fromFile(fileName: String): Option[CharStream] = {
    optionIf(Files.exists(Paths.get(fileName))) {
      CharStreams.fromFileName(fileName)
    }
  }

  def fromResource(klass: Class[_], basePath: String)(fileName: String): Option[CharStream] = {
    val fullPath = s"$basePath/$fileName"
    Option(klass.getResourceAsStream(fullPath.toString)).map { stream =>
      val channel = Channels.newChannel(stream)
      CharStreams.fromChannel(
        channel,
        StandardCharsets.UTF_8,
        4096,
        CodingErrorAction.REPLACE,
        fileName,
        -1
      )
    }
  }

  def fromStdlibResource: String => Option[CharStream] =
    fromResource(this.getClass, "/de/uni_luebeck/isp/tessla/stdlib")

  def empty(fileName: String): Option[CharStream] = None
}

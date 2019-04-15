package de.uni_luebeck.isp.tessla

import java.nio.channels.Channels
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.{CharStream, CharStreams}
import util._

object IncludeResolvers {
  def fromFile(fileName: String): Option[CharStream] = {
    optionIf(Files.exists(Paths.get(fileName))) {
      CharStreams.fromFileName(fileName)
    }
  }

  def fromResource(klass: Class[_], basePath: String)(fileName: String) = {
    val fullPath = s"$basePath/$fileName"
    Option(klass.getResourceAsStream(fullPath.toString)).map { stream =>
      val channel = Channels.newChannel(stream)
      CharStreams.fromChannel(channel, StandardCharsets.UTF_8, 4096, CodingErrorAction.REPLACE, fileName, -1)
    }
  }

  def empty(fileName: String) = None
}

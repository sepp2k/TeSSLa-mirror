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

package de.uni_luebeck.isp.tessla.core

import java.nio.channels.Channels
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Paths}

import org.antlr.v4.runtime.{CharStream, CharStreams}

/**
 * Provides multiple include resolvers, for use in [[Compiler.Options]]
 */

object IncludeResolvers {

  def fromFile(fileName: String): Option[CharStream] = {
    Option.when(Files.exists(Paths.get(fileName))) {
      CharStreams.fromFileName(fileName)
    }
  }

  def fromResource(classPathSeq: Seq[(Class[_], String)])(fileName: String): Option[CharStream] = {
    classPathSeq.flatMap { case (c, p) => fromResource(c, p)(fileName) }.headOption
  }

  def fromResource(klass: Class[_], basePath: String)(fileName: String): Option[CharStream] = {
    val fullPath = if (basePath.isEmpty) fileName else s"$basePath/$fileName"
    Option(klass.getResourceAsStream(fullPath)).map { stream =>
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

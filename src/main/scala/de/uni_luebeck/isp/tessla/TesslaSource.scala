package de.uni_luebeck.isp.tessla

import java.io.InputStream

import io.Source

/**
  * Wrapper for tessla source code.
  */
class TesslaSource private(val src: Source, val path: String) {
  def getLines = src.getLines
  def mkString = src.mkString
}


/**
  * Convenience object for creating TesslaSource instances.
  */
object TesslaSource {
  def fromFile(path: String): TesslaSource = {
    new TesslaSource(Source.fromFile(path), path)
  }

  def fromString(str: String, path: String): TesslaSource = {
    new TesslaSource(Source.fromString(str), path)
  }

  def fromJavaStream(stream: InputStream, path: String): TesslaSource = {
    new TesslaSource(Source.fromInputStream(stream), path)
  }

  def fromScalaSource(source: Source, path: String) = {
    new TesslaSource(source, path)
  }

  def fromIterator(iterator: Iterator[Char], path: String) = {
    new TesslaSource(new Source { override val iter = iterator }, path)
  }

  val stdin = new TesslaSource(Source.stdin, "<stdin>")
}

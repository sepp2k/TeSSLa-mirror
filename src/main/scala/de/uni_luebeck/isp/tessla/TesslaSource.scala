package de.uni_luebeck.isp.tessla

import java.nio.file.Paths

import io.Source

/**
  * Wrapper for tessla source code.
  *
  * @param src
  */
class TesslaSource(val src: Source, val path: String = "") {}


/**
  * Convenience object for creating TesslaSource instances.
  */
object TesslaSource {
  def fromFile(name: String): TesslaSource = {
    new TesslaSource(Source.fromFile(name), name)
  }

  def fromString(str: String): TesslaSource = {
    new TesslaSource(Source.fromString(str))
  }
}

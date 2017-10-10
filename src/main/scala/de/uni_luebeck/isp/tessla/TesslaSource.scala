package de.uni_luebeck.isp.tessla

import io.Source

/**
  * Wrapper for tessla source code.
  */
class TesslaSource(val src: Source, val path: String) {}


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
}

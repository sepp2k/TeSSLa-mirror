package de.uni_luebeck.isp.tessla

import io.Source

/**
  * Wrapper for tessla source code.
  *
  * @param src
  */
class TesslaSource(val src: Source) {}


/**
  * Convenience object for creating TesslaSource instances.
  */
object TesslaSource {

  def fromFile(name: String): TesslaSource = {
    new TesslaSource(Source.fromFile(name))
  }

  def fromString(str: String): TesslaSource = {
    new TesslaSource(Source.fromString(str))
  }
}

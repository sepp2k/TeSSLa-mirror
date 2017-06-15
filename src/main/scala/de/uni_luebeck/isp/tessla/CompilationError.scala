package de.uni_luebeck.isp.tessla

abstract class CompilationError extends Exception {
  def loc: Location
  def message: String

  override def toString = s"$loc: $message"
}

package de.uni_luebeck.isp.tessla

trait Diagnostic {
  def loc: Location
  def message: String

  override def toString = s"$loc: $message"
}

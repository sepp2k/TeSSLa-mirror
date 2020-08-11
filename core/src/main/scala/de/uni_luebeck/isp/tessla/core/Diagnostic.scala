package de.uni_luebeck.isp.tessla.core

trait Diagnostic {
  def loc: Location

  def message: String

  override def toString = s"$loc: $message"
}

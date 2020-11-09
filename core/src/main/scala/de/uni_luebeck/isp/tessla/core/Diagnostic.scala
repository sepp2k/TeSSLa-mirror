/*

 */

package de.uni_luebeck.isp.tessla.core

/**
 * Describes a diagnostic component, consisting of a message and a location. Used for warning and error messages.
 */
trait Diagnostic {
  def loc: Location

  def message: String

  override def toString = s"$loc: $message"
}

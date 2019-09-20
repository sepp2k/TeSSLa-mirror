package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.Location

/**
  * Contains errors complementing those in de.uni_luebeck.isp.tessla.Errors by special compilation errors
  */
object Errors {

  case class CommandNotSupportedError(m: String, loc: Location = Location.unknown) extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The translation of the given command is not supported.\nDetails: $m"
  }

  case class NotYetImplementedError(m: String, loc: Location = Location.unknown) extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The translation of the given command is not supported, yet.\nDetails: $m"
  }

  case class DSLError(m: String, loc: Location = Location.unknown) extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The creation of the abstract imperative code failed [DSLError]: $m"
  }

}
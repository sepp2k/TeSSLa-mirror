package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.Location

/**
  * Class containing errors complementing those in de.uni_luebeck.isp.tessla.Errors by special compilation errors
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

  case class TypeError(m: String) extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"An expression in the intermediate code has invalid type.\nDetails: $m"
    override def loc = Location.unknown
  }

  case class CoreASTError(m: String, loc: Location = Location.unknown) extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The TeSSLa Core AST contains an error.\nDetails: $m"
  }

  case class CLIError(m: String) extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The compiler received unvalid commandline arguments.\nDetails: $m"
    override def loc = Location.unknown
  }

}

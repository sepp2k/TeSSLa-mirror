package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.Location

/**
 * Class containing errors complementing those in [[de.uni_luebeck.isp.tessla.Errors]] by special compilation errors
 */
object Errors {

  /**
   * Error thrown if a certain command (annotation, expression or statements) is not supported in the compiler backend
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class CommandNotSupportedError(m: String, loc: Location = Location.unknown)
      extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message =
      s"The translation of the given command is not supported by the compiler backend.\nDetails: $m"
  }

  /**
   * Error thrown if a certain command (annotation, expression or statements) is not implemented in the compiler backend
   * yet but planned to be implemented soon
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class NotYetImplementedError(m: String, loc: Location = Location.unknown)
      extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The translation of the given command is not supported, yet.\nDetails: $m"
  }

  /**
   * Error indicating that the generation or processing of the ImpLan AST went wrong
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class DSLError(m: String, loc: Location = Location.unknown)
      extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The intermediate code contains an error.\nDetails: $m"
  }

  /**
   * Error indicating that the generation or processing of the ImpLan AST caused a type error
   * @param m More detailed message
   */
  case class DSLTypeError(m: String) extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"An expression in the intermediate code has invalid type.\nDetails: $m"
    override def loc: Location = Location.unknown
  }

  /**
   * Error thrown if the TeSSLa Core AST from the frontend cannot be processed
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class CoreASTError(m: String, loc: Location = Location.unknown)
      extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The TeSSLa Core AST contains an error.\nDetails: $m"
  }

  /**
   * Error thrown if the compiler cannot be run with the given console arguments
   * @param m More detailed message
   */
  case class CLIError(m: String) extends de.uni_luebeck.isp.tessla.Errors.TesslaError {
    override def message = s"The compiler received unvalid commandline arguments.\nDetails: $m"
    override def loc: Location = Location.unknown
  }

}

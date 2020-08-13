package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.{Diagnostic, Errors, Location}

/**
 * Class containing errors complementing those in [[de.uni_luebeck.isp.tessla.core.Errors]] by special compilation errors
 * and additional compiler related warnings
 */
object Diagnostics {

  /**
   * Error thrown if a certain command (annotation, expression or statements) is not supported in the compiler backend
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class CommandNotSupportedError(m: String, loc: Location = Location.unknown)
      extends Errors.TesslaError {
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
      extends Errors.TesslaError {
    override def message = s"The translation of the given command is not supported, yet.\nDetails: $m"
  }

  /**
   * Error indicating that the generation or processing of the ImpLan AST went wrong
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class DSLError(m: String, loc: Location = Location.unknown)
      extends Errors.TesslaError {
    override def message = s"The intermediate code contains an error.\nDetails: $m"
  }

  /**
   * Error indicating that the generation or processing of the ImpLan AST caused a type error
   * @param m More detailed message
   */
  case class DSLTypeError(m: String) extends Errors.TesslaError {
    override def message = s"An expression in the intermediate code has invalid type.\nDetails: $m"
    override def loc: Location = Location.unknown
  }

  /**
   * Error thrown if the TeSSLa Core AST from the frontend cannot be processed
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class CoreASTError(m: String, loc: Location = Location.unknown)
      extends Errors.TesslaError {
    override def message = s"The TeSSLa Core AST contains an error.\nDetails: $m"
  }

  /**
   * Error thrown if the compiler cannot be run with the given console arguments
   * @param m More detailed message
   */
  case class CLIError(m: String) extends Errors.TesslaError {
    override def message = s"The compiler received unvalid commandline arguments.\nDetails: $m"
    override def loc: Location = Location.unknown
  }

  /**
   * Warning raised if the compilation of the generated code raises a warning or information
   * @param message The message of the base warning
   * @param compiler The compiler backend originally raising the warning
   * @param msgType Kind of the warning, e.g. warning, information, note ...
   */
  case class CompilationWarning(message: String, compiler: String, msgType: String) extends Diagnostic {
    override def loc: Location = Location.unknown
    override def toString(): String = s"$compiler raised $msgType :\n$message"
  }

}

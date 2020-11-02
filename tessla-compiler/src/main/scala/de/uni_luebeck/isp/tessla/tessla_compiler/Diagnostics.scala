/*
 * Copyright (c) 2020 Institute of Software Engineering and Programming Languages,
 * University of Lübeck, Germany
 *
 * Modified MIT license:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this binary (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software and the code which is
 * generated by the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.{Diagnostic, Errors, Location}

/**
 * Class containing errors complementing the core errors
 * and additional compiler related warnings
 */
object Diagnostics {

  /**
   * Error thrown if a certain command (annotation, expression or statements) is not supported in the compiler backend
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class CommandNotSupportedError(m: String, loc: Location = Location.unknown) extends Errors.TesslaError {
    override def message =
      s"The translation of the given command is not supported by the compiler backend.\nDetails: $m"
  }

  /**
   * Error thrown if a certain command (annotation, expression or statements) is not implemented in the compiler backend
   * yet but planned to be implemented soon
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class NotYetImplementedError(m: String, loc: Location = Location.unknown) extends Errors.TesslaError {
    override def message = s"$m is not yet implemented."
  }

  /**
   * Error indicating that the generation or processing of the ImpLan AST went wrong
   * @param m More detailed message
   * @param loc Location which code section in the original TeSSLa spec caused the error
   */
  case class DSLError(m: String, loc: Location = Location.unknown) extends Errors.TesslaError {
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
  case class CoreASTError(m: String, loc: Location = Location.unknown) extends Errors.TesslaError {
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

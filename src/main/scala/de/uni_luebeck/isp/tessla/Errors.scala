package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit
import de.uni_luebeck.isp.tessla.Types.Type
import de.uni_luebeck.isp.tessla.interpreter.Traces

object Errors {

  abstract class TesslaError extends Exception with Diagnostic

  case class TypeMismatch(expected: Type, found: Type, loc: Location) extends TesslaError {
    override def message = s"Type mismatch: Expected $expected, found $found"
  }

  case class TypeArityMismatch(name: String, expected: Int, actual: Int, loc: Location) extends TesslaError {
    def message = s"Wrong number of type arguments for $name. Expected: $expected, actual: $actual"
  }

  case class UnknownType(name: String, loc: Location) extends TesslaError {
    def message = s"Unknown type: $name"
  }

  case class StreamOfStreams(loc: Location) extends TesslaError {
    def message = "Streams may not contain other streams"
  }

  case class InfiniteRecursion(loc: Location) extends TesslaError {
    override def message = "Definition is infinitely recursive"
  }

  case class UndefinedVariable(id: Ast.Identifier) extends TesslaError {
    override def loc = id.loc

    override def message = s"Undefined variable ${id.name}"
  }

  case class UndefinedFunction(id: Ast.Identifier, arity: Int) extends TesslaError {
    override def loc = id.loc

    override def message = s"Undefined macro or operator ${id.name}/$arity"
  }

  case class UndefinedNamedArg(id: Ast.Identifier) extends TesslaError {
    override def loc = id.loc

    override def message = s"Undefined keyword argument ${id.name}"
  }

  case class MultipleDefinitionsError(id: Ast.Identifier, previousLoc: Location) extends TesslaError {
    override def loc = id.loc

    override def message = s"Multiple definitions of ${id.name} in same scope (previous definition at $previousLoc)"
  }

  case class InternalError(m: String, loc: Location = UnknownLoc) extends TesslaError {
    def message = s"Internal error: $m"
  }

  case class UnknownTimeUnit(name: String, loc: Location) extends TesslaError {
    def message = s"Unknown time unit: $name. " +
      "Allowed time units: ns, us, ms, s, m, h, d"
  }

  case class UndefinedTimeUnit(loc: Location) extends TesslaError {
    def message = s"No time unit defined in trace file"
  }

  case class TimeUnitConversionError(from: TimeUnit, to: TimeUnit, loc: Location) extends TesslaError {
    def message = s"Cannot convert from $from to $to"
  }

  case class DivideByZero(loc: Location) extends TesslaError {
    def message = "Division by zero"
  }

  case class ParserError(m: String, loc: Location) extends TesslaError {
    def message = s"Invalid syntax: $m"
  }

  case class NotAnEventError(line: Traces.Line) extends TesslaError {
    def loc: Location = line.loc

    def message: String = s"Input $line is not an event"
  }

  case class UndeclaredInputStreamError(streamName: String, loc: Location) extends TesslaError {
    def message: String = s"Undeclared input stream: $streamName"
  }

  case class DecreasingTimeStampsError(first: BigInt, second: BigInt, loc: Location) extends TesslaError {
    def message: String = s"Decreasing time stamps: first = $first, second = $second"
  }

  case class ProvideAfterPropagationError(time: BigInt, loc: Location) extends TesslaError {
    def message: String = s"Tried to provide inputs after their propagation at time $time"
  }

  case class NegativeDelayError(value: BigInt, loc: Location) extends TesslaError {
    def message: String = s"Negative delay $value"
  }

  case class InputTypeMismatch(value: TesslaCore.Value, streamName: String, streamType: Types.ValueType, loc: Location) extends TesslaError {
    def message: String = s"Tried to provide value of type ${value.typ} ($value) to input stream '$streamName' of type $streamType"
  }

}
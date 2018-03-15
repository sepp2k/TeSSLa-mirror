package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit
import de.uni_luebeck.isp.tessla.Types.Type

object Errors {

  case class TesslaErrorWithTimestamp(error: TesslaError, timestamp: BigInt) extends TesslaError {
    override def loc: Location = error.loc

    override def message: String = s"${error.message} (t = $timestamp)"
  }

  abstract class TesslaError extends Exception with Diagnostic

  case class TypeMismatch(expected: Type, found: Type, loc: Location) extends TesslaError {
    override def message = s"Type mismatch: Expected $expected, found $found"
  }

  case class UnliftedUseOfPartialFunction(name: String, loc: Location) extends TesslaError {
    override def message = s"The partial function $name was used unlifted"
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

  case class UndefinedVariable(id: Tessla.Identifier) extends TesslaError {
    override def loc = id.loc

    override def message = s"Undefined variable ${id.name}"
  }

  case class UndefinedFunction(id: Tessla.Identifier, arity: Int) extends TesslaError {
    override def loc = id.loc

    override def message = s"Undefined macro or operator ${id.name}/$arity"
  }

  case class UndefinedNamedArg(id: Tessla.Identifier) extends TesslaError {
    override def loc = id.loc

    override def message = s"Undefined keyword argument ${id.name}"
  }

  case class MultipleDefinitionsError(id: Tessla.Identifier, previousLoc: Location) extends TesslaError {
    override def loc = id.loc

    override def message = s"Multiple definitions of ${id.name} in same scope (previous definition at $previousLoc)"
  }

  case class InternalError(m: String, loc: Location = Location.unknown) extends TesslaError {
    def message = s"Internal error: $m"
  }

  case class UnknownTimeUnit(name: String, loc: Location) extends TesslaError {
    def message = s"Unknown time unit: $name. " +
      "Allowed time units: fs, ps, ns, us, ms, s, m, h, d"
  }

  case class UndefinedTimeUnit(loc: Location) extends TesslaError {
    def message = s"No time unit set"
  }

  case class TimeUnitConversionError(from: TimeUnit, to: TimeUnit) extends TesslaError {
    // This error happens when applying the unit `from` to a number in a spec that uses the unit `to`
    // (where `from` can't be converted to `to`), so we want to use `from`'s location to have it
    // marked as the problem.
    def loc = from.loc

    def message = s"Cannot convert from $from to $to"
  }

  case class DivideByZero(loc: Location) extends TesslaError {
    def message = "Division by zero"
  }

  case class ParserError(m: String, loc: Location) extends TesslaError {
    def message = s"Invalid syntax: $m"
  }

  case class NotAnEventError(line: String, loc: Location) extends TesslaError {
    def message: String = s"Input $line is not an event"
  }

  case class UndeclaredInputStreamError(streamName: String, loc: Location) extends TesslaError {
    def message: String = s"Undeclared input stream: $streamName"
  }

  case class DecreasingTimeStampsError(first: BigInt, second: BigInt, loc: Location) extends TesslaError {
    def message: String = s"Decreasing time stamps: first = $first, second = $second"
  }

  def ProvideAfterPropagationError(time: BigInt, loc: Location = Location.unknown) = {
    InternalError(s"Tried to provide inputs after their propagation at time $time", loc)
  }

  case class NegativeDelayError(value: BigInt, loc: Location) extends TesslaError {
    def message: String = s"Negative delay $value"
  }

  case class InputTypeMismatch(value: TesslaCore.Value, streamName: String, streamType: Types.ValueType, loc: Location) extends TesslaError {
    def message: String = s"Tried to provide value of type ${value.typ} ($value) to input stream '$streamName' of type $streamType"
  }

  case class TracesOperationError(loc: Location, op: String, args: TesslaCore.LiteralValue*) extends TesslaError {
    def message: String = s"Operation $op not defined for ${args.mkString(", ")}."
  }

  case class TracesUnknownIdentifierError(loc: Location, name: String) extends TesslaError {
    def message: String = s"Unknown identifier in traces: $name."
  }

  case class NegativeStepError(loc: Location, value: BigInt) extends TesslaError {
    def message: String = s"Negative step in time stamp range: $value."
  }

  case class KeyNotFound(key: TesslaCore.Value, map: Map[_, _], loc: Location) extends TesslaError {
    def message: String = s"Key $key was not found in $map"
  }

}
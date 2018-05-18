package de.uni_luebeck.isp.tessla

object Errors {

  abstract class TesslaError extends Exception with Diagnostic

  case class WithStackTrace(inner: TesslaError, stackTrace: Seq[Location]) extends TesslaError {
    override def loc = inner.loc

    override def message = inner.message

    def stackTraceString = stackTrace.map(loc => s"\n    called from $loc").mkString("")

    override def toString = super.toString() + stackTraceString
  }

  case class TypeMismatch(expected: String, found: TypedTessla.Type, loc: Location) extends TesslaError {
    override def message = s"Type mismatch: Expected $expected, found $found"
  }

  object TypeMismatch {
    def apply(expected: TypedTessla.Type, found: TypedTessla.Type, loc: Location): TypeMismatch = {
      TypeMismatch(expected.toString, found, loc)
    }
  }

  case class TypeArityMismatch(name: String, expected: Int, actual: Int, loc: Location) extends TesslaError {
    override def message = s"Wrong number of type arguments for $name. Expected: $expected, actual: $actual"
  }

  case class TypeArgumentsNotInferred(name: String, loc: Location) extends TesslaError {
    override def message = s"Explicit type arguments needed for $name"
  }

  case class ArityMismatch(name: String, expected: Int, actual: Int, loc: Location) extends TesslaError {
    override def message = s"Wrong number of arguments for $name. Expected: $expected, actual: $actual"
  }

  case class UndefinedType(name: String, loc: Location) extends TesslaError {
    override def message = s"Undefined type: $name"
  }

  case class MissingTypeAnnotationRec(name: String, loc: Location) extends TesslaError {
    override def message = s"Recursive definition $name needs a type annotation"
  }

  case class MissingTypeAnnotationParam(name: String, loc: Location) extends TesslaError {
    override def message = s"Parameter $name needs a type annotation"
  }

  case class StreamOfNonValueType(loc: Location) extends TesslaError {
    override def message = "Streams may only contain value types; not other streams or functions"
  }

  case class InfiniteRecursion(loc: Location) extends TesslaError {
    override def message = "Infinite recursion detected"
  }

  case class UndefinedVariable(id: Tessla.Identifier) extends TesslaError {
    override def loc = id.loc

    override def message = s"Undefined variable ${id.name}"
  }

  case class UndefinedFunction(id: Tessla.Identifier, arity: Int) extends TesslaError {
    override def loc = id.loc

    override def message = s"Undefined macro or operator ${id.name}/$arity"
  }

  case class UndefinedNamedArg(name: String, loc: Location) extends TesslaError {

    override def message = s"Undefined keyword argument $name"
  }

  case class MultipleDefinitionsError(id: Tessla.Identifier, previousLoc: Location) extends TesslaError {
    override def loc = id.loc

    override def message =
      s"Multiple definitions of ${id.name} in same scope\n" +
        s"    Previous definition at: $previousLoc"
  }

  case class InternalError(m: String, loc: Location = Location.unknown) extends TesslaError {
    override def message = s"Internal error: $m"
  }

  case class UnknownTimeUnit(name: String, loc: Location) extends TesslaError {
    override def message = s"Unknown time unit: $name. " +
      "Allowed time units: fs, ps, ns, us, ms, s, m, h, d"
  }

  case class UndefinedTimeUnit(loc: Location) extends TesslaError {
    override def message = s"Use of time units is only allowed when a base time unit is set for the data"
  }

  case class TimeUnitConversionError(from: TimeUnit, to: TimeUnit) extends TesslaError {
    // This error happens when applying the unit `from` to a number in a spec that uses the unit `to`
    // (where `from` can't be converted to `to`), so we want to use `from`'s location to have it
    // marked as the problem.
    override def loc = from.loc

    override def message = s"Cannot convert from $from to $to"
  }

  case class DivideByZero(loc: Location) extends TesslaError {
    override def message = "Division by zero"
  }

  case class ParserError(m: String, loc: Location) extends TesslaError {
    override def message = s"Invalid syntax: $m"
  }

  case class NotAnEventError(line: String, loc: Location) extends TesslaError {
    override def message: String = s"Input $line is not an event"
  }

  case class UndeclaredInputStreamError(streamName: String, loc: Location) extends TesslaError {
    override def message: String = s"Undeclared input stream: $streamName"
  }

  case class DecreasingTimeStampsError(first: BigInt, second: BigInt, loc: Location) extends TesslaError {
    override def message: String = s"Decreasing time stamps: first = $first, second = $second"
  }

  def ProvideAfterPropagationError(time: BigInt, loc: Location = Location.unknown) = {
    InternalError(s"Tried to provide inputs after their propagation at time $time", loc)
  }

  case class NegativeDelayError(value: BigInt, loc: Location) extends TesslaError {
    override def message: String = s"Negative delay $value"
  }

  case class InputTypeMismatch(value: TesslaCore.Value, streamName: String, streamType: TesslaCore.Type, loc: Location) extends TesslaError {
    override def message: String = s"Tried to provide value of type ${value.typ} ($value) to input stream '$streamName' of type $streamType"
  }

  case class TracesOperationError(loc: Location, op: String, args: TesslaCore.Value*) extends TesslaError {
    override def message: String = s"Operation $op not defined for ${args.mkString(", ")}."
  }

  case class TracesUnknownIdentifierError(loc: Location, name: String) extends TesslaError {
    override def message: String = s"Unknown identifier in traces: $name."
  }

  case class NegativeStepError(loc: Location, value: BigInt) extends TesslaError {
    override def message: String = s"Negative step in time stamp range: $value."
  }

  case class KeyNotFound(key: TesslaCore.Value, map: Map[_, _], loc: Location) extends TesslaError {
    override def message: String = s"Key $key was not found in $map"
  }

  case class StackOverflow(loc: Location) extends TesslaError {
    override def message = "Stack overflow"
  }

  case class UndefinedAnnotation(id: Tessla.Identifier) extends TesslaError {
    override def message = s"Undefined annotation $id; currently the only supported annotation is @liftable"

    override def loc = id.loc
  }

  case class MacroAnnotationOnNonMacro(annotationId: Tessla.Identifier, defName: String) extends TesslaError {
    override def message = s"The annotation $annotationId can only be applied to macro definitions, which $defName is not"

    override def loc = annotationId.loc
  }

  case class UnliftableMacroType(loc: Location) extends TesslaError {
    override def message = "Macro has been annotated with @liftable, but its type does not allow being lifted"
  }

}
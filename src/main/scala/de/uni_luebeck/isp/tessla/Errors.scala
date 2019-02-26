package de.uni_luebeck.isp.tessla

object Errors {
  abstract class TesslaError extends Exception with Diagnostic

  case class TesslaErrorWithTimestamp(error: TesslaError, timestamp: BigInt) extends TesslaError {
    override def loc: Location = error.loc

    override def message: String = s"${error.message} (t = $timestamp)"
  }

  case class WithStackTrace(inner: TesslaError, stackTrace: Seq[Location]) extends TesslaError {
    override def loc = inner.loc

    override def message = inner.message

    def stackTraceString = stackTrace.map(loc => s"\n    called from $loc").mkString("")

    override def toString = super.toString() + stackTraceString
  }

  case class MissingBody(id: Tessla.Identifier) extends TesslaError {
    override def loc = id.loc

    override def message =
      s"Member definition $id needs a body. Eliding the body is only allowed if the definition" +
      " consists of an identifier and nothing else"
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

  case class InputStreamMustHaveStreamType(loc: Location) extends TesslaError {
    override def message = "Input streams must be defined to have stream types"
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

  case class UndefinedNamedArg(arg: TypedTessla.NamedArgument) extends TesslaError {
    override def loc = arg.idLoc.loc

    override def message = s"Undefined keyword argument ${arg.name}"
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

  case class DecreasingTimeStampsError(first: BigInt, second: BigInt, loc: Location) extends TesslaError {
    override def message: String = s"Decreasing time stamps: first = $first, second = $second"
  }

  case class SameTimeStampError(timestamp: BigInt, eventName: String, loc: Location) extends TesslaError {
    override def message: String = s"Multiple events $eventName at timestamp $timestamp"
  }

  def ProvideAfterPropagationError(time: BigInt, loc: Location = Location.unknown) = {
    InternalError(s"Tried to provide inputs after their propagation at time $time", loc)
  }

  case class NegativeDelayError(value: BigInt, loc: Location) extends TesslaError {
    override def message: String = s"Negative delay $value"
  }

  case class InputTypeMismatch(value: TesslaCore.Value, valueType: String, streamName: String, streamType: TesslaCore.Type, loc: Location) extends TesslaError {
    override def message: String = s"Unexpected value of type $valueType ($value), expected type $streamType, in input stream '$streamName'"
  }

  case class NonPositiveStepError(value: BigInt, loc: Location) extends TesslaError {
    override def message: String = s"Non-positive step in time stamp range: $value"
  }

  case class KeyNotFound(key: TesslaCore.Value, map: Map[_, _], loc: Location) extends TesslaError {
    override def message: String = s"Key $key was not found in $map"
  }

  case class HeadOfEmptyList(loc: Location) extends TesslaError {
    override def message: String = "Head of empty list"
  }

  case class LastOfEmptyList(loc: Location) extends TesslaError {
    override def message: String = "Last of empty list"
  }

  case class CannotGetValueOfNone(loc: Location) extends TesslaError {
    override def message: String = s"Cannot get value of None"
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

  case class MemberNotDefined(ot: TypedTessla.ObjectType, member: String, loc: Location) extends TesslaError {
    override def message = s"Object of type $ot does not have a member named $member"
  }

  case class InvalidEscapeSequence(sequence: String, loc: Location) extends TesslaError {
    override def message = s"Invalid escape sequence '$sequence' in string"
  }

  case class StringInterpolationInInclude(loc: Location) extends TesslaError {
    override def message = "String interpolation is not allowed in include statement"
  }
}
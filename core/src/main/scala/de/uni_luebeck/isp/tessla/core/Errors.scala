/*
 * Copyright 2020 Institute of Software Engineering and Programming Languages,
 *                University of Lübeck, Germany
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.core

import java.util.IllegalFormatException

import de.uni_luebeck.isp.tessla.core.Tessla.TimeLiteral
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core

/**
 * Contains all errors used within this application
 */

object Errors {

  trait TesslaError extends Exception with Diagnostic

  def mkTesslaError(msg: String, location: Location = Location.unknown): TesslaError = new TesslaError {
    override def loc = location

    override def message = msg
  }

  case class TesslaErrorWithTimestamp(error: TesslaError, timestamp: BigInt) extends Exception(error) with TesslaError {
    override def loc: Location = error.loc

    override def message: String = s"${error.message} (t = $timestamp)"
  }

  case class WithStackTrace(inner: TesslaError, stackTrace: Seq[Location]) extends Exception(inner) with TesslaError {
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

  case class WrongType(expected: String, actual: Core.Type, loc: Location) extends TesslaError {
    override def message = s"Type mismatch: Expected $expected, but found $actual"
  }

  case class InstArgDoesNotExist(functionName: String, argumentIndex: Int, loc: Location) extends TesslaError {
    override def message = s"The function $functionName does have an $argumentIndex-th argument."
  }

  case class TypeArityMismatch(name: String, expected: Int, actual: Int, loc: Location) extends TesslaError {
    override def message =
      s"""Wrong number of type arguments for $name. Expected: $expected, actual: $actual"""
  }

  case class TypeArgumentsNotInferred(name: String, loc: Location) extends TesslaError {
    override def message = s"""Explicit type arguments needed for "$name""""
  }

  case class ArityMismatch(name: String, expected: Int, actual: Int, loc: Location) extends TesslaError {
    override def message =
      s"""Wrong number of arguments for $name. Expected: $expected, actual: $actual"""
  }

  case class UndefinedType(name: String, loc: Location) extends TesslaError {
    override def message = s"Undefined type: $name"
  }

  case class MissingTypeAnnotationRec(name: String, loc: Location) extends TesslaError {
    override def message = s"""Recursive definition "$name" needs a type annotation"""
  }

  case class MissingTypeAnnotationParam(name: String, loc: Location) extends TesslaError {
    override def message = s"""Parameter $name needs a type annotation"""
  }

  case class MissingTypeAnnotationExtern(name: String, loc: Location) extends TesslaError {
    override def message = s"""Extern definition "$name" needs a type annotation"""
  }

  case class InputStreamMustHaveStreamType(loc: Location) extends TesslaError {
    override def message = "Input streams must be defined to have stream types"
  }

  case class InfiniteRecursion(loc: Location) extends TesslaError {
    override def message = "Infinite recursion detected"
  }

  case class UndefinedVariable(id: Tessla.Identifier) extends TesslaError {
    override def loc = id.loc

    override def message = s"""Undefined variable: ${id.name}"""
  }

  case class UndefinedNamedArg(name: String, loc: Location) extends TesslaError {
    override def message = s"""Undefined keyword argument: $name"""
  }

  case class PosArgAfterNamedArg(loc: Location) extends TesslaError {
    override def message = s"""Unsupported use of positional argument: Cannot occur after named argument."""
  }

  case class MultipleDefinitionsError(id: Tessla.Identifier, previousLoc: Location) extends TesslaError {
    override def loc = id.loc

    override def message =
      s"""Multiple definitions of "${id.name}" in same scope\n""" +
        s"    Previous definition at: $previousLoc"
  }

  case class ImportAmbiguousDefinitionError(name: String, definition: String, loc: Location) extends TesslaError {
    override def message = s"""Failed to import "$definition" from "$name": "$definition" is already defined."""
  }

  case class InternalError(m: String, loc: Location = Location.unknown) extends TesslaError {
    override def message = s"Internal error: $m"
  }

  case class RuntimeError(m: String, loc: Location = Location.unknown) extends TesslaError {
    override def message = s"Runtime error: $m"
  }

  case class UnknownTimeUnit(name: String, loc: Location, options: List[String]) extends TesslaError {
    override def message = s"Unknown time unit: $name. " +
      s"Allowed time units are: ${options.mkString(", ")}"
  }

  case class UndefinedBaseTime(loc: Location) extends TesslaError {
    override def message = s"Use of time units is only allowed when a base time is set for the data"
  }

  case class UnsupportedOpenTypes(loc: Location) extends TesslaError {
    override def message = s"Open types are not (yet) supported"
  }

  case class TimeUnitConversionError(from: TimeUnit, to: TimeUnit) extends TesslaError {
    // This error happens when applying the unit `from` to a number in a spec that uses the unit `to`
    // (where `from` can't be converted to `to`), so we want to use `from`'s location to have it
    // marked as the problem.
    override def loc = from.loc

    override def message = s"Cannot convert from $from to $to"
  }

  case class TimeConversionError(value: TimeLiteral, base: TimeLiteral) extends TesslaError {
    override def loc = value.unit.loc

    override def message = s"Cannot represent $value in terms of base time $base."
  }

  case class ParserError(m: String, loc: Location) extends TesslaError {
    override def message = s"Invalid syntax: $m"
  }

  case class DecreasingTimeStampsError(first: BigInt, second: BigInt, loc: Location) extends TesslaError {
    override def message: String = s"Decreasing time stamps: first = $first, second = $second"
  }

  case class UndeclaredInputStream(name: String, loc: Location) extends TesslaError {
    override def message: String = s"Undeclared input stream $name"
  }

  case class SameTimeStampError(timestamp: BigInt, eventName: String, loc: Location) extends TesslaError {
    override def message: String = s"""Multiple events "$eventName" at timestamp $timestamp"""
  }

  def ProvideAfterPropagationError(time: BigInt, loc: Location = Location.unknown) = {
    InternalError(s"Tried to provide inputs after their propagation at time $time", loc)
  }

  case class NonPositiveDelayError(value: BigInt, loc: Location) extends TesslaError {
    override def message: String = s"Non-positive delay $value"
  }

  case class NonPositiveStepError(value: BigInt, loc: Location) extends TesslaError {
    override def message: String = s"Non-positive step in time stamp range: $value"
  }

  case class KeyNotFound(key: Any, map: Map[_, _], loc: Location) extends TesslaError {
    override def message: String = s"Key $key was not found in $map"
  }

  case class IndexOutOfRange(index: BigInt, list: IndexedSeq[_], loc: Location) extends TesslaError {
    override def message: String = s"Index $index is out of range for list of size ${list.length}"
  }

  case class StackOverflow(loc: Location) extends TesslaError {
    override def message = "Stack overflows"
  }

  case class UndefinedAnnotation(id: Tessla.Identifier) extends TesslaError {
    override def message = s"Undefined annotation: $id"

    override def loc: Location = id.loc
  }

  case class NonLiteralAnnotationParameter(name: String, exp: Core.ExpressionArg) extends TesslaError {
    override def message = s"Annotation $name has non-literal parameter $exp."

    override def loc: Location = exp.location
  }

  case class LiftableOnNonMacro(loc: Location, defName: String) extends TesslaError {
    override def message = s"""Non-macro "$defName" can not be liftable"""
  }

  case class UnliftableMacroType(loc: Location) extends TesslaError {
    override def message =
      "Macro has been defined as liftable, but its type does not allow being lifted"
  }

  case class MemberNotDefined(ot: TypedTessla.ObjectType, member: String, loc: Location) extends TesslaError {
    override def message = s"""Object of type $ot does not have a member named "$member""""
  }

  case class InvalidEscapeSequence(sequence: String, loc: Location) extends TesslaError {
    override def message = s"Invalid escape sequence '$sequence' in string"
  }

  case class StringInterpolationOrFormatInConstantString(loc: Location) extends TesslaError {
    override def message =
      "String interpolation or format specifiers are not allowed in include statements"
  }

  case class StringFormatError(error: IllegalFormatException, loc: Location) extends TesslaError {
    override def message =
      s"Error in format string: '${error.getClass.getSimpleName}: ${error.getMessage}'"
  }

  case class FormatNeedsArgument(format: String, loc: Location) extends TesslaError {
    override def message =
      s"The format specifier $format requires a value and thus can only be used after a string interpolation"
  }

  case class UnsupportedConversion(conversion: Char, loc: Location) extends TesslaError {
    override def message = s"TeSSLa does not support the conversion specifier $conversion"
  }

  case class FileNotFound(name: String, loc: Location) extends TesslaError {
    override def message = s"Could not find a TeSSLa file named $name"
  }

  case class AbsoluteIncludePath(loc: Location) extends TesslaError {
    override def message = "Absolute paths are not allowed in includes"
  }

  case class InOutStatementInModule(loc: Location) extends TesslaError {
    override def message = "Input and output statements are not allowed inside of modules"
  }

  case class AnnotationDefInModule(loc: Location) extends TesslaError {
    override def message = "Annotation definitions are not allowed inside of modules"
  }

  case class CtfInvalidPrefix(prefix: String, loc: Location) extends TesslaError {
    override def message =
      s"""Unknown prefix "${prefix}". Available prefixes are context, eventcontext, eventheader, packetcontext and fields"""
  }

  case class CtfTypeError(key: String, expectedType: String, loc: Location) extends TesslaError {
    override def message = s"""Value for key "$key" is not of type $expectedType"""
  }

  case class CtfKeyNotFound(key: String, loc: Location) extends TesslaError {
    override def message = s"""Key "$key" not found"""
  }

  case class InstrUnsupportedPlatform(platforms: Set[(String, String)]) extends TesslaError {
    override def message =
      s"""Unsupported platform ${sys.props("os.name")}-${sys.props("os.arch")} for C code instrumentation.
                              |Supported platforms are
                              |${platforms.map(p => p._1 + "-" + p._2).mkString("\n")}""".stripMargin
    val loc = Location.unknown
  }
}

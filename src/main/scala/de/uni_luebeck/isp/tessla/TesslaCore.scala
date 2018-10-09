package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.TesslaError
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition

object TesslaCore extends HasUniqueIdentifiers {
  final case class Specification(streams: Seq[(Identifier, Expression)],
                                 functions: Seq[(Identifier, Value)],
                                 inStreams: Seq[(String, StreamType, Location)],
                                 outStreams: Seq[(String, StreamRef, StreamType)]) {
    override def toString = {
      inStreams.map { case (name, typ, _) => s"in $name: $typ\n" }.mkString +
        functions.map { case (name, value) => s"const $name := $value\n" }.mkString +
        streams.map { case (name, expr) => s"def $name := $expr\n" }.mkString +
        outStreams.map { case (name, stream, typ) => s"out $stream : $typ as $name\n" }.mkString
    }
  }

  sealed abstract class Expression {
    def loc: Location
  }

  sealed abstract class StreamRef {
    def loc: Location
    def withLoc(loc: Location): StreamRef
  }

  final case class Stream(id: Identifier, loc: Location) extends StreamRef {
    override def toString = id.toString
    def withLoc(loc: Location): Stream = copy(loc = loc)
  }

  final case class InputStream(name: String, loc: Location) extends StreamRef {
    override def toString = s"input($name)"
    def withLoc(loc: Location): InputStream = copy(loc = loc)
  }

  final case class Nil(loc: Location) extends StreamRef {
    override def toString = "nil"
    def withLoc(loc: Location): Nil = copy(loc = loc)
  }

  final case class Default(stream: StreamRef, default: ValueOrError, loc: Location) extends Expression {
    override def toString = s"default($stream, $default)"
  }

  final case class DefaultFrom(valueStream: StreamRef, defaultStream: StreamRef, loc: Location) extends Expression {
    override def toString = s"defaultFrom($valueStream, $defaultStream)"
  }

  final case class Const(value: ValueOrError, stream: StreamRef, loc: Location) extends Expression {
    override def toString = s"const($value, $stream)"
  }

  final case class Time(stream: StreamRef, loc: Location) extends Expression {
    override def toString = s"time($stream)"
  }

  final case class Last(values: StreamRef, clock: StreamRef, loc: Location) extends Expression {
    override def toString = s"last($values, $clock)"
  }

  final case class DelayedLast(values: StreamRef, delays: StreamRef, loc: Location) extends Expression {
    override def toString = s"delayedLast($values, $delays)"
  }

  final case class Merge(stream1: StreamRef, stream2: StreamRef, loc: Location) extends Expression {
    override def toString = s"merge($stream1, $stream2)"
  }

  final case class Lift(f: Identifier, args: Seq[StreamRef], loc: Location) extends Expression {
    override def toString = {
      args.mkString(s"lift($f", ", ", ")")
    }
  }

  final case class SignalLift(f: Identifier, args: Seq[StreamRef], loc: Location) extends Expression {
    override def toString = {
      args.mkString(s"slift($f", ", ", ")")
    }
  }

  sealed abstract class ValueExpression {
    def loc: Location
  }

  final case class Function(parameters: Seq[Identifier],
                            scope: Map[Identifier, ValueExpression],
                            body: Identifier,
                            loc: Location) extends ValueExpression {
    override def toString = {
      val defs = scope.map {
        case (id, exp) => s"const $id := $exp\n"
      }.mkString
      s"(${parameters.mkString(", ")}) => {\n${defs}return $body\n}"
    }
  }

  case class Application(f: Identifier, args: Seq[Identifier], loc: Location) extends ValueExpression {
    override def toString = args.mkString("f(", ", ", ")")
  }

  case class Literal(value: ValueOrError, loc: Location) extends ValueExpression

  sealed trait ValueOrError {
    def forceValue: Value

    def mapValue(f: Value => Value): ValueOrError
  }

  final case class Error(error: TesslaError) extends ValueOrError {
    override def forceValue = throw error

    override def mapValue(f: Value => Value) = this
  }

  sealed abstract class Value extends ValueOrError {
    def loc: Location

    def withLoc(loc: Location): Value

    override def forceValue = this

    override def mapValue(f: Value => Value) = f(this)
  }

  sealed abstract class PrimitiveValue extends Value {
    def value: Any

    override def toString = value.toString

    override def equals(obj: Any) = obj match {
      case other: PrimitiveValue => value == other.value
      case _ => false
    }

    override def hashCode() = value.hashCode()
  }

  final case class IntValue(value: BigInt, loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): IntValue = copy(loc = loc)
  }

  final case class BoolValue(value: Boolean, loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): BoolValue = copy(loc = loc)
  }

  final case class StringValue(value: String, loc: Location) extends PrimitiveValue {
    override def toString = s""""$value""""
    override def withLoc(loc: Location): StringValue = copy(loc = loc)
  }

  final case class Unit(loc: Location) extends PrimitiveValue {
    override def value = ()
    override def withLoc(loc: Location): Unit = copy(loc = loc)
  }

  final case class TesslaOption(value: Option[Value], loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): TesslaOption = copy(loc = loc)
  }

  final case class TesslaMap(value: Map[Value, Value], loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): TesslaMap = copy(loc = loc)
  }

  final case class TesslaSet(value: Set[Value], loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): TesslaSet = copy(loc = loc)
  }

  final case class Ctf(value: ICompositeDefinition, loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): Ctf = copy(loc = loc)
  }

  case class BuiltInOperator(value: BuiltIn.PrimitiveOperator, loc: Location) extends PrimitiveValue {
    override def withLoc(loc: Location): BuiltInOperator = copy(loc = loc)
  }

  case class Closure(function: Function, capturedEnvironment: Map[Identifier, ValueOrError], loc: Location) extends Value {
    override def withLoc(loc: Location): Closure = copy(loc = loc)

    override def toString = {
      capturedEnvironment.map { case (id, v) => s"$id = $v"}.mkString(s"$function with {", ", ", "}")
    }
  }

  sealed abstract class Type

  sealed abstract class ValueType extends Type

  case object IntType extends ValueType {
    override def toString = "Int"
  }

  case object BoolType extends ValueType {
    override def toString = "Bool"
  }

  case object StringType extends ValueType {
    override def toString = "String"
  }

  case object UnitType extends ValueType {
    override def toString = "Unit"
  }

  case object FunctionType extends ValueType {
    override def toString = "? => ?"
  }

  case class OptionType(elementType: ValueType) extends ValueType {
    override def toString = s"Option[$elementType]"
  }

  case class MapType(keyType: ValueType, valueType: ValueType) extends ValueType {
    override def toString = s"Map[$keyType, $valueType]"
  }

  case class SetType(elementType: ValueType) extends ValueType {
    override def toString = s"Set[$elementType]"
  }

  case object CtfType extends ValueType {
    override def toString = "CTF"
  }

  case class StreamType(elementType: ValueType) extends Type {
    override def toString = s"Events[$elementType]"
  }
}

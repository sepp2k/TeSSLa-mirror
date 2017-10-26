package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.TesslaError

object TesslaCore {
  final case class Specification(streams: Map[String, Expression],
                                 inStreams: Seq[(String, Types.Stream, Location)],
                                 outStreams: Seq[(String, StreamRef)]) {
    override def toString = {
      inStreams.map { case (name, typ, _) => s"in $name: $typ\n" }.mkString +
        streams.map { case (name, expr) => s"def $name := $expr\n" }.mkString +
        outStreams.map { case (name, stream) => s"out $stream as $name\n" }.mkString
    }
  }

  sealed abstract class Expression {
    def loc: Location
  }

  sealed abstract class StreamRef {
    def loc: Location
    def name: String
    def withLoc(loc: Location): StreamRef
  }

  final case class Stream(name: String, loc: Location) extends StreamRef {
    override def toString = name
    def withLoc(loc: Location): Stream = copy(loc = loc)
  }

  final case class InputStream(name: String, loc: Location) extends StreamRef {
    override def toString = s"input($name)"
    def withLoc(loc: Location): InputStream = copy(loc = loc)
  }

  final case class Nil(loc: Location) extends StreamRef {
    override def name = "nil"
    override def toString = name
    def withLoc(loc: Location): Nil = copy(loc = loc)
  }

  final case class Default(stream: StreamRef, default: LiteralValue, loc: Location) extends Expression {
    override def toString = s"default($stream, $default)"
  }

  final case class DefaultFrom(valueStream: StreamRef, defaultStream: StreamRef, loc: Location) extends Expression {
    override def toString = s"defaultFrom($valueStream, $defaultStream)"
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

  final case class Lift(operator: PrimitiveOperators.PrimitiveOperator, args: Seq[StreamRef], loc: Location) extends Expression {
    override def toString = operator match {
      case _: PrimitiveOperators.PrefixOperator => s"$operator${args(0)}"
      case _: PrimitiveOperators.InfixOperator => s"${args(0)} $operator ${args(1)}"
      case PrimitiveOperators.IfThen => s"if ${args(0)} then ${args(1)}"
      case PrimitiveOperators.IfThenElse => s"if ${args(0)} then ${args(1)} else ${args(2)}"
      case PrimitiveOperators.Const(value) => args.mkString(s"const($value)(", ", ", ")")
      case PrimitiveOperators.First => args.mkString("first(", ", ", ")")
      case custom: PrimitiveOperators.CustomBuiltIn => args.mkString(s"$custom(", ",", ")")
    }
  }

  sealed abstract class Value {
    def loc: Location
    def withLoc(loc: Location): Value
    def typ: Types.ValueType
    def toLiteral: LiteralValue
    def value: Any
    def isError: Boolean

    override def toString = value.toString
  }

  final case class ErrorValue(err: TesslaError) extends Value {
    override def loc = err.loc
    override def withLoc(loc: Location): ErrorValue = this
    override def toLiteral = throw err
    override def typ = Types.Nothing
    override def value = throw err
    override def isError = true
  }

  sealed abstract class LiteralValue extends Value {
    override def toLiteral = this
    override def withLoc(loc: Location): LiteralValue
    override def isError = false
  }

  final case class IntLiteral(value: BigInt, loc: Location) extends LiteralValue {
    override def withLoc(loc: Location): IntLiteral = copy(loc = loc)
    override val typ = Types.Int
  }

  final case class BoolLiteral(value: Boolean, loc: Location) extends LiteralValue {
    override def withLoc(loc: Location): BoolLiteral = copy(loc = loc)
    override val typ = Types.Bool
  }

  final case class StringLiteral(value: String, loc: Location) extends LiteralValue {
    override def toString = s""""$value""""
    override def withLoc(loc: Location): StringLiteral = copy(loc = loc)
    override val typ = Types.String
  }

  final case class Unit(loc: Location) extends LiteralValue {
    override def value = ()
    override def withLoc(loc: Location): Unit = copy(loc = loc)
    override val typ = Types.Unit
  }

  abstract class CustomValue extends LiteralValue
}

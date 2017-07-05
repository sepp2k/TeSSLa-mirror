package de.uni_luebeck.isp.tessla

object TesslaCore {
  final case class Specification(streams: Map[String, Expression],
                                 inStreams: Seq[(String, Location)],
                                 outStreams: Seq[(String, StreamRef)]) {
    override def toString = {
      inStreams.map { case (name, _) => s"in $name\n" }.mkString +
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
  }

  final case class Stream(name: String, loc: Location) extends StreamRef {
    override def toString = name
  }

  final case class InputStream(name: String, loc: Location) extends StreamRef {
    override def toString = s"input($name)"
  }

  final case class Nil(loc: Location) extends StreamRef {
    override def name = "nil"
    override def toString = name
  }

  final case class Default(stream: StreamRef, default: LiteralValue, loc: Location) extends Expression {
    override def toString = s"default($stream, $default)"
  }

  final case class DefaultFrom(valueStream: StreamRef, defaultStream: StreamRef, loc: Location) extends Expression {
    override def toString = s"defaultFrom($valueStream, $defaultStream)"
  }

  final case class Const(value: LiteralValue, clock: StreamRef, loc: Location) extends Expression {
    override def toString = s"const($value, $clock)"
  }

  final case class Time(stream: StreamRef, loc: Location) extends Expression {
    override def toString = s"time($stream)"
  }

  final case class Last(values: StreamRef, clock: StreamRef, loc: Location) extends Expression {
    override def toString = s"last($values, $clock)"
  }

  final case class DelayedLast(values: StreamRef, delays: StreamRef, loc: Location) extends Expression {
    override def toString = s"deleayedLast($values, $delays)"
  }

  final case class Add(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs + $rhs"
  }

  final case class Sub(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs - $rhs"
  }

  final case class Mul(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs * $rhs"
  }

  final case class Div(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs / $rhs"
  }

  final case class BitAnd(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs & $rhs"
  }

  final case class BitOr(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs | $rhs"
  }

  final case class BitXor(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs ^ $rhs"
  }

  final case class LeftShift(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs << $rhs"
  }

  final case class RightShift(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs >> $rhs"
  }

  final case class BitFlip(arg: StreamRef, loc: Location) extends Expression {
    override def toString = s"~ $arg"
  }

  final case class Lt(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs < $rhs"
  }

  final case class Gt(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs > $rhs"
  }

  final case class Lte(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs <= $rhs"
  }

  final case class Gte(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs >= $rhs"
  }

  final case class Eq(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs == $rhs"
  }

  final case class Neq(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs != $rhs"
  }

  final case class And(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs && $rhs"
  }

  final case class Or(lhs: StreamRef, rhs: StreamRef, loc: Location) extends Expression {
    override def toString = s"$lhs || $rhs"
  }

  final case class Not(arg: StreamRef, loc: Location) extends Expression {
    override def toString = s"! $arg"
  }

  final case class IfThenElse(condition: StreamRef, thenCase: StreamRef, elseCase: StreamRef, loc: Location) extends Expression {
    override def toString = s"if $condition then $thenCase else $elseCase"
  }

  final case class IfThen(condition: StreamRef, thenCase: StreamRef, loc: Location) extends Expression {
    override def toString = s"if $condition then $thenCase"
  }

  sealed abstract class LiteralValue {
    def loc: Location
  }

  final case class IntLiteral(value: BigInt, loc: Location) extends LiteralValue {
    override def toString = value.toString
  }

  final case class BoolLiteral(value: Boolean, loc: Location) extends LiteralValue {
    override def toString = value.toString
  }

  final case class StringLiteral(value: String, loc: Location) extends LiteralValue {
    override def toString = s""""$value""""
  }

  final case class Unit(loc: Location) extends LiteralValue {
    override def toString = "()"
  }
}

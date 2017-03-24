package de.uni_luebeck.isp.tessla

object TesslaCore {
  sealed abstract class Expression {
    def loc: NestedLoc
  }

  final case class Var(name: String, loc: NestedLoc) extends Expression {
    override def toString = name
  }

  final case class Input(name: String, loc: NestedLoc) extends Expression {
    override def toString = s"input($name)"
  }

  final case class Default(stream: Expression, default: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"default($stream, $default)"
  }

  final case class DefaultFrom(valueStream: Expression, defaultStream: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"defaultFrom($valueStream, $defaultStream)"
  }

  final case class Const(value: Expression, clock: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"const($value, $clock)"
  }

  final case class Time(stream: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"time($stream)"
  }

  final case class Last(values: Expression, clock: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"last($values, $clock)"
  }

  final case class DelayedLast(values: Expression, delays: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"deleayedLast($values, $delays)"
  }

  final case class Nil(loc: NestedLoc) extends Expression {
    override def toString = "nil"
  }

  final case class Add(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs + $rhs)"
  }

  final case class Sub(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs - $rhs)"
  }

  final case class Mul(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs * $rhs)"
  }

  final case class Lt(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs < $rhs)"
  }

  final case class Gt(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs > $rhs)"
  }

  final case class Lte(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs <= $rhs)"
  }

  final case class Gte(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs >= $rhs)"
  }

  final case class Eq(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs == $rhs)"
  }

  final case class Neq(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs != $rhs)"
  }

  final case class And(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs && $rhs)"
  }

  final case class Or(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs || $rhs)"
  }

  final case class Not(arg: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"(! $arg)"
  }

  final case class IfThenElse(condition: Expression, thenCase: Expression, elseCase: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"(if $condition then $thenCase else $elseCase)"
  }

  final case class IfThen(condition: Expression, thenCase: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"(if $condition then $thenCase)"
  }

  sealed abstract class LiteralValue extends Expression

  final case class IntLiteral(value: BigInt, loc: NestedLoc) extends LiteralValue {
    override def toString = value.toString
  }

  final case class BoolLiteral(value: Boolean, loc: NestedLoc) extends LiteralValue {
    override def toString = value.toString
  }

  final case class Unit(loc: NestedLoc) extends LiteralValue {
    override def toString = "()"
  }

  final case class Specification(streams: Map[String, Expression],
                                 inStreams: Seq[(String, NestedLoc)],
                                 outStreams: Seq[(String, NestedLoc)]) {
    override def toString = {
      inStreams.map { case (name, _) => s"in $name\n" }.mkString +
      streams.map { case (name, expr) => s"define $name = $expr\n" }.mkString +
      outStreams.map { case (name, _) => s"out $name\n" }.mkString
    }
  }
}

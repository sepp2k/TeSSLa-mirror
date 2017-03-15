package de.uni_luebeck.isp.tessla

object TesslaCore {
  sealed abstract class Expression {
    def loc: NestedLoc
  }

  final case class Var(name: String, loc: NestedLoc) extends Expression {
    override def toString = name
  }

  final case class Default(stream: Expression, default: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"default($stream, $default)"
  }

  final case class Last(values: Expression, clock: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"last($values, $clock)"
  }

  final case class Add(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs + $rhs)"
  }

  final case class Sub(lhs: Expression, rhs: Expression, loc: NestedLoc) extends Expression {
    override def toString = s"($lhs - $rhs)"
  }

  final case class IntLiteral(value: BigInt, loc: NestedLoc) extends Expression {
    override def toString = value.toString
  }

  final case class BoolLiteral(value: Boolean, loc: NestedLoc) extends Expression {
    override def toString = value.toString
  }

  final case class Unit(loc: NestedLoc) extends Expression {
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

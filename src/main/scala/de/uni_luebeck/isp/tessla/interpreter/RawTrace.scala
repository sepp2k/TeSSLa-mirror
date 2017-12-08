package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.{TracesOperationError, TracesUnknownIdentifierError}
import de.uni_luebeck.isp.tessla.{Location, TesslaCore}
import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit

object RawTrace{
  sealed abstract class Item

  case class TimeUnitDeclaration(timeUnit: TimeUnit) extends Item {
    override def toString = {
      "$timeunit = \"" + timeUnit + "\""
    }
  }

  trait TraceOp {
    def loc: Location

    def eval(env: Map[String, BigInt]): TesslaCore.Value
  }

  trait TracesUnaryOp extends TraceOp {
    def exp: TraceOp
  }

  trait TracesBinaryOp extends TraceOp {
    def lhs: TraceOp

    def rhs: TraceOp
  }

  case class TimeRange(loc: Location, id: Option[Identifier], from: BigInt, to: Option[BigInt], step: BigInt) {
    override def toString: String = s"$from <= ${id.getOrElse("\"_\"")} ${
      if (to.isDefined) {
        "<= " + to.get
      } else {
        ""
      }
    }${
      if (step != 1) {
        s"; ${id.getOrElse("\"_\"")} += $step"
      } else {
        ""
      }
    }"
  }

  case class EventRange(loc: Location, timeRange: TimeRange, stream: Identifier, value: TraceOp) extends Item{
    override def toString: String = s"$timeRange: $stream = $value"

    def evalValue: TesslaCore.Value =
      value.eval(if (timeRange.id.isDefined) {
        Map(timeRange.id.get.name -> timeRange.from)
      } else {
        Map()
      })
  }

  case class Add(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 + val2, loc)
      case (l, r) => throw TracesOperationError(loc, "+", l, r)
    }
  }

  case class Sub(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 - val2, loc)
      case (l, r) => throw TracesOperationError(loc, "-", l, r)
    }
  }

  case class Mult(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 * val2, loc)
      case (l, r) => throw TracesOperationError(loc, "*", l, r)
    }
  }

  case class Div(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 / val2, loc)
      case (l, r) => throw TracesOperationError(loc, "/", l, r)
    }
  }

  case class Mod(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 % val2, loc)
      case (l, r) => throw TracesOperationError(loc, "%", l, r)
    }
  }

  case class Neg(exp: TraceOp, loc: Location) extends TracesUnaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = exp.eval(env) match {
      case TesslaCore.IntLiteral(v, _) => TesslaCore.IntLiteral(-v, loc)
      case transExp => throw TracesOperationError(loc, "-", transExp)
    }
  }

  case class And(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 && val2, loc)
      case (l, r) => throw TracesOperationError(loc, "&&", l, r)
    }
  }

  case class Implies(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(!val1 || val2, loc)
      case (l, r) => throw TracesOperationError(loc, "->", l, r)
    }
  }

  case class Equiv(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 == val2, loc)
      case (l, r) => throw TracesOperationError(loc, "<->", l, r)
    }
  }

  case class Or(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 || val2, loc)
      case (l, r) => throw TracesOperationError(loc, "||", l, r)
    }
  }

  case class Xor(lhs: TraceOp, rhs: TraceOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral((val1 && !val2) || (!val1 && val2), loc)
      case (l, r) => throw TracesOperationError(loc, "^", l, r)
    }
  }

  case class Not(exp: TraceOp, loc: Location) extends TracesUnaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = exp.eval(env) match {
      case TesslaCore.BoolLiteral(v, _) => TesslaCore.BoolLiteral(!v, loc)
      case transExp => throw TracesOperationError(loc, "!", transExp)
    }
  }

  trait Atomic extends TraceOp

  case class Literal(v: TesslaCore.Value, loc: Location) extends Atomic {
    override def eval(env: Map[String, BigInt]): TesslaCore.Value = v
  }

  case class Identifier(loc: Location, name: String) extends Atomic {
    override def toString: String = "\"" + name + "\""

    override def eval(env: Map[String, BigInt]): TesslaCore.Value =
      if (env.isDefinedAt(name)) {
        TesslaCore.IntLiteral(env(name), loc)
      } else {
        throw TracesUnknownIdentifierError(loc, name)
      }
  }
}

class RawTrace(val timeStampUnit: Option[TimeUnit], val eventRanges: Iterator[RawTrace.EventRange]) extends Iterator[RawTrace.Item] {
  val items: Iterator[RawTrace.Item] = timeStampUnit match {
    case Some(timeUnit) => Iterator(RawTrace.TimeUnitDeclaration(timeUnit)) ++ eventRanges
    case None => eventRanges
  }

  override def hasNext = items.hasNext

  override def next() = items.next()
}

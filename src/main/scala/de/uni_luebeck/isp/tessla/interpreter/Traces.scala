package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.{InputTypeMismatch, TracesOperationError, TracesUnknownIdentifierError, UndeclaredInputStreamError}
import de.uni_luebeck.isp.tessla.{Location, TesslaCore}
import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit

object Traces {

  case class Event(loc: Location, timeStamp: BigInt, stream: Identifier, value: TesslaCore.LiteralValue) {
    override def toString: String = s"$timeStamp: $stream = $value"
  }

  case class EventRange(loc: Location, timeRange: TimeRange, stream: Identifier, value: TracesOp) {
    override def toString: String = s"$timeRange: $stream = $value"

    def evalValue: TesslaCore.LiteralValue =
      value.eval(if (timeRange.id.isDefined) {
        Map(timeRange.id.get.name -> timeRange.from)
      } else {
        Map()
      })

  }

  case class TimeRange(id: Option[Identifier], from: BigInt, to: Option[BigInt], step: BigInt) {
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


  trait TracesOp {
    def loc: Location

    def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue
  }

  trait TracesUnaryOp extends TracesOp {
    def exp: TracesOp
  }

  trait TracesBinaryOp extends TracesOp {
    def lhs: TracesOp

    def rhs: TracesOp
  }

  case class Add(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 + val2, loc)
      case (l, r) => throw TracesOperationError(loc, "+", l, r)
    }
  }

  case class Sub(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 - val2, loc)
      case (l, r) => throw TracesOperationError(loc, "-", l, r)
    }
  }

  case class Mult(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 * val2, loc)
      case (l, r) => throw TracesOperationError(loc, "*", l, r)
    }
  }

  case class Div(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 / val2, loc)
      case (l, r) => throw TracesOperationError(loc, "/", l, r)
    }
  }

  case class Mod(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 % val2, loc)
      case (l, r) => throw TracesOperationError(loc, "%", l, r)
    }
  }

  case class Neg(exp: TracesOp, loc: Location) extends TracesUnaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = exp.eval(env) match {
      case TesslaCore.IntLiteral(v, _) => TesslaCore.IntLiteral(-v, loc)
      case transExp => throw TracesOperationError(loc, "-", transExp)
    }
  }

  case class And(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 && val2, loc)
      case (l, r) => throw TracesOperationError(loc, "&&", l, r)
    }
  }

  case class Implies(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(!val1 || val2, loc)
      case (l, r) => throw TracesOperationError(loc, "->", l, r)
    }
  }

  case class Equiv(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 == val2, loc)
      case (l, r) => throw TracesOperationError(loc, "<->", l, r)
    }
  }

  case class Or(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 || val2, loc)
      case (l, r) => throw TracesOperationError(loc, "||", l, r)
    }
  }

  case class Xor(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = (lhs.eval(env), rhs.eval(env)) match {
      case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral((val1 && !val2) || (!val1 && val2), loc)
      case (l, r) => throw TracesOperationError(loc, "^", l, r)
    }
  }

  case class Not(exp: TracesOp, loc: Location) extends TracesUnaryOp {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = exp.eval(env) match {
      case TesslaCore.BoolLiteral(v, _) => TesslaCore.BoolLiteral(!v, loc)
      case transExp => throw TracesOperationError(loc, "!", transExp)
    }
  }

  trait Atomic extends TracesOp

  case class Literal(v: TesslaCore.LiteralValue, loc: Location) extends Atomic {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue = v
  }

  case class Identifier(name: String, loc: Location) extends Atomic {
    override def eval(env: Map[String, BigInt]): TesslaCore.LiteralValue =
      if (env.isDefinedAt(name)) {
        TesslaCore.IntLiteral(env(name), loc)
      } else {
        throw TracesUnknownIdentifierError(loc, name)
      }
  }

}

class Traces(val timeStampUnit: Option[TimeUnit], values: Iterator[Traces.EventRange]) {

  def flattenInput(threshold: BigInt, abortAt: Option[BigInt]) = feedInput(threshold, abortAt)(println)


  def feedInput(threshold: BigInt, abortAt: Option[BigInt])(process: Traces.Event => Unit): Unit = {
    val queue = new TracesQueue(threshold, abortAt)

    var abort = false

    def handleInput(event: Traces.Event) {
      if (abortAt.isEmpty || event.timeStamp <= abortAt.get) {
        process(event)
      } else {
        abort = true
      }
    }

    values.takeWhile {
      _ => !abort
    }.foreach { value =>
      if (!abort) {
        queue.enqueue(value, handleInput)
      }
    }

    //in the end handle every remaining event from the queue
    queue.processAll(handleInput)
  }

  def interpretInput(tesslaSpec: Interpreter, threshold: BigInt, abortAt: Option[BigInt])(callback: (BigInt, String, TesslaCore.Value) => Unit): Unit = {
    tesslaSpec.addOutStreamListener(callback)
    var previousTS: BigInt = 0

    def provide(event: Traces.Event): Unit = {
      if (event.timeStamp - previousTS != 0) {
        tesslaSpec.step(event.timeStamp - previousTS)
        previousTS = event.timeStamp
      }
      event match {
        case Traces.Event(loc, _, Traces.Identifier(name, streamLoc), value) =>
          tesslaSpec.inStreams.get(name) match {
            case Some((inStream, typ)) =>
              if (value.typ == typ) {
                inStream.provide(value)
              } else {
                throw InputTypeMismatch(value, name, typ, loc)
              }
            case None => throw UndeclaredInputStreamError(name, streamLoc)
          }
      }
    }

    feedInput(threshold, abortAt)(provide)

    tesslaSpec.step()
  }
}

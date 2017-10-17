package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.{InputTypeMismatch, TracesOperationError, UndeclaredInputStreamError}
import de.uni_luebeck.isp.tessla.{Location, SourceLoc, TesslaCore}
import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit

object Traces {

  case class Event(loc: Location, timeRange: TimeRange, stream: Identifier, value: TesslaCore.LiteralValue) {
    override def toString: String = s"$timeRange: $stream = $value"
  }

  case class TimeRange(id: Option[Identifier], from: BigInt, to: Option[BigInt], step: BigInt) {
    override def toString: String = if (to.isDefined && to.get == from){
      s"$from"
    }else{
      s"$from <= ${id.getOrElse("\"_\"")} ${
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

    trait TracesOp{
      def loc: Location
      def eval: TesslaCore.LiteralValue
    }

    trait TracesUnaryOp extends TracesOp{
      def exp: TracesOp
    }

    trait TracesBinaryOp extends TracesOp{
      def lhs: TracesOp
      def rhs: TracesOp
    }

    case class Add(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 + val2, loc)
        case (l, r) => throw TracesOperationError(loc, "+", l, r)
      }
    }

    case class Sub(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 - val2, loc)
        case (l, r) => throw TracesOperationError(loc, "-", l, r)
      }
    }

    case class Mult(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 * val2, loc)
        case (l, r) => throw TracesOperationError(loc, "*", l, r)
      }
    }

    case class Div(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 / val2, loc)
        case (l, r) => throw TracesOperationError(loc, "/", l, r)
      }
    }

    case class Mod(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.IntLiteral(val1, _), TesslaCore.IntLiteral(val2, _)) => TesslaCore.IntLiteral(val1 % val2, loc)
        case (l, r) => throw TracesOperationError(loc, "%", l, r)
      }
    }

    case class Neg(exp: TracesOp,loc: Location) extends TracesUnaryOp{
      override def eval: TesslaCore.LiteralValue = exp.eval match {
        case TesslaCore.IntLiteral(v, _) => TesslaCore.IntLiteral(-v, loc)
        case transExp => throw TracesOperationError(loc, "-", transExp)
      }
    }

    case class And(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 && val2, loc)
        case (l, r) => throw TracesOperationError(loc, "&&", l, r)
      }
    }

    case class Implies(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(!val1 || val2, loc)
        case (l, r) => throw TracesOperationError(loc, "->", l, r)
      }
    }

    case class Equiv(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 == val2, loc)
        case (l, r) => throw TracesOperationError(loc, "<->", l, r)
      }
    }

    case class Or(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral(val1 || val2, loc)
        case (l, r) => throw TracesOperationError(loc, "||", l, r)
      }
    }

    case class Xor(lhs: TracesOp, rhs: TracesOp, loc: Location) extends TracesBinaryOp{
      override def eval: TesslaCore.LiteralValue = (lhs.eval, rhs.eval) match {
        case (TesslaCore.BoolLiteral(val1, _), TesslaCore.BoolLiteral(val2, _)) => TesslaCore.BoolLiteral((val1 && !val2) || (!val1 && val2), loc)
        case (l, r) => throw TracesOperationError(loc, "^", l, r)
      }
    }

    case class Not(exp: TracesOp, loc: Location) extends TracesUnaryOp{
      override def eval: TesslaCore.LiteralValue = exp.eval match {
        case TesslaCore.BoolLiteral(v, _) => TesslaCore.BoolLiteral(!v, loc)
        case transExp => throw TracesOperationError(loc, "!", transExp)
      }
    }

    case class Atomic(v: TesslaCore.LiteralValue, loc: Location) extends TracesOp{
      override def eval: TesslaCore.LiteralValue = v
    }
  }

  case class Identifier(loc: Location, name: String) {
    override def toString: String = name
  }

}

class Traces(val timeStampUnit: Option[TimeUnit], values: Iterator[Traces.Event]) {

  def flattenInput(threshold: BigInt, abortAt: Option[BigInt]) = feedInput(threshold, abortAt)(println)


  def feedInput(threshold: BigInt, abortAt: Option[BigInt])(process: Traces.Event => Unit): Unit = {
    val queue = new TracesQueue(threshold, abortAt)

    var abort = false
    def handleInput(event: Traces.Event) {
      if (abortAt.isEmpty || event.timeRange.from <= abortAt.get) {
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
      if (event.timeRange.from - previousTS != 0) {
        tesslaSpec.step(event.timeRange.from - previousTS)
        previousTS = event.timeRange.from
      }
      event match {
        case Traces.Event(loc, _, Traces.Identifier(streamLoc, name), value) =>
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

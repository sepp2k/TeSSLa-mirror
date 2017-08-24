package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{CompilationError, TesslaCore, Types}
import de.uni_luebeck.isp.tessla.interpreter.Traces._
import de.uni_luebeck.isp.tessla.interpreter.Traces.{DecreasingTimeStampsError, TypeMismatchError, UndeclaredInputStreamError}
import de.uni_luebeck.isp.tessla.Location

object Traces {
  case class TypeMismatchError(value: TesslaCore.Value, streamName: String, streamType: Types.ValueType, loc: Location) extends CompilationError {
    def message: String = s"Tried to provide value of type ${value.typ} ($value) to input stream '$streamName' of type $streamType"
  }

  case class NotAnEventError(line: Traces.Line) extends CompilationError{
    def loc: Location = line.loc
    def message: String = s"Input $line is not an event."
  }

  case class UndeclaredInputStreamError(streamName: String, loc: Location) extends CompilationError {
    def message: String = s"Undeclared input stream: $streamName"
  }

  case class DecreasingTimeStampsError(first: BigInt, second: BigInt, loc: Location) extends CompilationError{
    def message: String = s"Decreasing time stamps: first = $first, second = $second."
  }

  sealed trait Line {
    def loc: Location
  }

  case class TimeUnit(loc: Location, timeUnit: de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit) extends Line {
    override def toString = timeUnit.toString
  }

  case class Event(loc: Location, timeStamp: BigInt, stream: Identifier, value: TesslaCore.LiteralValue) extends Line {
    override def toString = s"$timeStamp: $stream = $value"
  }

  case class Identifier(loc: Location, name: String) {
    override def toString = "\"" + name + "\""
  }
}

class Traces(val timeStampUnit: Option[Traces.TimeUnit], values: Iterator[Traces.Event]) {

  def feedInput(tesslaSpec: Interpreter, threshold: BigInt)(callback: (BigInt, String, TesslaCore.Value) => Unit): Unit = {
    val queue = new TracesQueue(threshold)

    def provide(event: Traces.Event) = {
      event match{
        case Traces.Event(loc, _, Traces.Identifier(streamLoc, name), value) =>
          tesslaSpec.inStreams.get(name) match {
            case Some((inStream, typ)) =>
              if (value.typ == typ) {
                inStream.provide(value)
              } else {
                throw TypeMismatchError(value, name, typ, loc)
              }
            case None => throw UndeclaredInputStreamError(name, streamLoc)
          }
      }

    }

    var previousTS: BigInt = 0

    def handleInput(event : Traces.Event) {
      if (event.timeStamp - previousTS != 0) {
        tesslaSpec.step(event.timeStamp - previousTS)
        previousTS = event.timeStamp
      }
      provide(event)
    }

    tesslaSpec.outStreams.foreach {
      case (name, stream) => stream.addListener {
        case Some(value) => callback(tesslaSpec.getTime, name, value)
        case None =>
      }
    }

    values.foreach(queue.enqueue(_, handleInput))

    //in the end handle every remaining event from the queue
    queue.processAll(handleInput)

    tesslaSpec.step()
  }
}

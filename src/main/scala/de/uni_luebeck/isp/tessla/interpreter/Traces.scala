package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{CompilationError, TesslaCore, Types}
import de.uni_luebeck.isp.tessla.interpreter.Input._
import de.uni_luebeck.isp.tessla.interpreter.Traces.{DecreasingTimeStampsError, TypeMismatchError, UndeclaredInputStreamError}
import de.uni_luebeck.isp.tessla.Location

object Traces {
  case class TypeMismatchError(value: TesslaCore.Value, streamName: String, streamType: Types.ValueType, loc: Location) extends CompilationError {
    def message: String = s"Tried to provide value of type ${value.typ} ($value) to input stream '$streamName' of type $streamType"
  }

  case class NotAnEventError(line: Input.Line) extends CompilationError{
    def loc: Location = line.loc
    def message: String = s"Input $line is not an event."
  }

  case class UndeclaredInputStreamError(streamName: String, loc: Location) extends CompilationError {
    def message: String = s"Undeclared input stream: $streamName"
  }

  case class DecreasingTimeStampsError(first: BigInt, second: BigInt, loc: Location) extends CompilationError{
    def message: String = s"Decreasing time stamps: first = $first, second = $second."
  }

  def read(input: Seq[Input.Line]): Traces = {
    def eventsOnly(lines: Seq[Input.Line]): Seq[Input.Event] = lines.map {
      case e: Input.Event => e
      case l => throw NotAnEventError(l)
    }

    input.headOption match {
      case Some(tu: Input.TimeUnit) => new Traces(Some(tu), eventsOnly(input.tail))
      case Some(_: Input.Event) => new Traces(None, eventsOnly(input))
      case None => new Traces(None, Seq())
    }
  }
}

class Traces(val timeStampUnit: Option[Input.TimeUnit], values: Seq[Input.Event]) {

  def feedInput(tesslaSpec: Interpreter, threshold: BigInt)(callback: (BigInt, String, TesslaCore.Value) => Unit): Unit = {
    val queue = new TracesQueue(threshold)

    def provide(event: Input.Event) = {
      event match{
        case Input.Event(loc, _, Input.Identifier(streamLoc, name), value) =>
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

    def handleInput(event : Input.Event) {
      val ts = event.timeStamp
      if (ts < previousTS) {
        throw DecreasingTimeStampsError(previousTS, ts, event.loc)
      }
      if (ts > previousTS) {
        tesslaSpec.step(ts - previousTS)
        previousTS = ts
      }
      provide(event)
    }

    def dequeue(timeStamp: BigInt, loc:Location): Unit = {
      if (queue.hasDecreasingTimeStamp(timeStamp)){
        throw DecreasingTimeStampsError(queue.smallestTimeStamp, timeStamp, loc)
      }
      while (queue.hasNext(timeStamp)) {
        queue.dequeue(timeStamp).foreach(handleInput)
      }
    }

    tesslaSpec.outStreams.foreach {
      case (name, stream) => stream.addListener {
        case Some(value) => callback(tesslaSpec.getTime, name, value)
        case None =>
      }
    }

    values.foreach {
      case ev@Event(loc, ts, _, _) =>
        dequeue(ts, loc)
        queue.enqueue(ev)
    }

    //in the end handle every remaining event from the queue
    queue.toList.foreach(handleInput)

    tesslaSpec.step()
  }
}

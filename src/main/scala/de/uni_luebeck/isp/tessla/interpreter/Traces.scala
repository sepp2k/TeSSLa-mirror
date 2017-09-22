package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.{InputTypeMismatch, UndeclaredInputStreamError}
import de.uni_luebeck.isp.tessla.TesslaCore
import de.uni_luebeck.isp.tessla.Location

object Traces {

  sealed trait Line {
    def loc: Location
  }

  case class TimeUnit(loc: Location, timeUnit: de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit) extends Line {
    override def toString: String = timeUnit.toString
  }

  case class Event(loc: Location, timeStamp: BigInt, stream: Identifier, value: TesslaCore.LiteralValue) extends Line {
    override def toString: String = s"$timeStamp: $stream = $value"
  }

  case class Identifier(loc: Location, name: String) {
    override def toString: String = "\"" + name + "\""
  }

}

class Traces(val timeStampUnit: Option[Traces.TimeUnit], values: Iterator[Traces.Event]) {

  def feedInput(tesslaSpec: Interpreter, threshold: BigInt)(callback: (BigInt, String, TesslaCore.Value) => Unit): Unit = {
    val queue = new TracesQueue(threshold)

    def provide(event: Traces.Event): Unit = {
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

    var previousTS: BigInt = 0

    def handleInput(event: Traces.Event) {
      if (event.timeStamp - previousTS != 0) {
        tesslaSpec.step(event.timeStamp - previousTS)
        previousTS = event.timeStamp
      }
      provide(event)
    }

    tesslaSpec.addOutStreamListener(callback)

    values.foreach(queue.enqueue(_, handleInput))

    //in the end handle every remaining event from the queue
    queue.processAll(handleInput)

    tesslaSpec.step()
  }
}

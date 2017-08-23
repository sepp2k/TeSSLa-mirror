package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{CompilationError, TesslaCore, UnknownLoc}
import de.uni_luebeck.isp.tessla.interpreter.Input._
import de.uni_luebeck.isp.tessla.interpreter.Traces.InvalidInputError


object Traces {

  case class InvalidInputError(message: String) extends CompilationError {
    def loc = UnknownLoc
  }

  def read(input: Seq[Input.Line]): Traces = {
    def eventsOnly(lines: Seq[Input.Line]): Seq[Input.Event] = lines.map {
      case e: Input.Event => e
      case _ => throw InvalidInputError("Time Unit declarations are only allowed in the beginning of the input.")
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

    def provide(streamName: String, value: TesslaCore.Value) = {
      tesslaSpec.inStreams.get(streamName) match {
        case Some((inStream, typ)) =>
          if (value.typ == typ) {
            inStream.provide(value)
          } else {
            throw InvalidInputError(s"Tried to provide value of type ${value.typ} ($value) to input stream '$streamName' of type $typ")
          }
        case None => throw InvalidInputError(s"Undeclared input stream: $streamName")
      }
    }

    var previousTS: BigInt = 0

    def handleInput(timestamp: BigInt, inStream: String, value: TesslaCore.Value = TesslaCore.Unit(UnknownLoc)) {
      val ts = timestamp
      if (ts < previousTS) sys.error("Decreasing time stamps: first = " + previousTS + " , second = " + ts)
      if (ts > previousTS) {
        tesslaSpec.step(ts - previousTS)
        previousTS = ts
      }
      provide(inStream, value)
    }

    def dequeue(timeStamp: BigInt): Unit = {
      while (queue.hasNext(timeStamp)) {
        queue.dequeue(timeStamp) match {
          case Some((ts, (n, v))) => handleInput(ts, n, v)
          case None =>
        }
      }
    }

    tesslaSpec.outStreams.foreach {
      case (name, stream) => stream.addListener {
        case Some(value) => callback(tesslaSpec.getTime, name, value)
        case None =>
      }
    }

    values.foreach {
      case Event(loc, ts, inStream, value) =>
        queue.enqueue(ts, inStream.name, value)
        dequeue(ts)
    }

    //in the end handle every remaining event from the queue
    queue.toList().foreach {
      case (ts, (n, v)) =>
        handleInput(ts, n, v)
    }

    tesslaSpec.step()
  }
}

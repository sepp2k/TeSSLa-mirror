package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.TimeUnit
import de.uni_luebeck.isp.tessla.{CompilationError, TesslaCore, UnknownLoc}
import de.uni_luebeck.isp.tessla.interpreter.Input._



object Traces {

  def read(input: Spec): Traces = {
    new Traces(input.timeUnit, input.events)
  }
}

class Traces(val timeStampUnit: Option[Input.TimeUnit], values: Seq[Input.Event]) {

  case class InvalidInputError(message: String) extends CompilationError {
    def loc = UnknownLoc
  }

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

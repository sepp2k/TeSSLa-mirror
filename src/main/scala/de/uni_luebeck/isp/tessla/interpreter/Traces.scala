package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.{InputTypeMismatch, UndeclaredInputStreamError}
import de.uni_luebeck.isp.tessla.TesslaCore
import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit

object Traces {
  case class Event(loc: Location, timeRange: TimeRange, stream: Identifier, value: TesslaCore.LiteralValue){
    override def toString: String = s"$timeRange: $stream = $value"
  }

  case class TimeRange(id: Option[Identifier], from: BigInt, to: Option[BigInt], step: BigInt){
    override def toString: String = s"${id.getOrElse("_")} from $from to ${to.getOrElse("infinity")} with step $step."
  }

  case class Identifier(loc: Location, name: String) {
    override def toString: String = "\"" + name + "\""
  }
}

class Traces(val timeStampUnit: Option[TimeUnit], values: Iterator[Traces.Event]) {

  def feedInput(tesslaSpec: Interpreter, threshold: BigInt, abortAt: Option[BigInt])(callback: (BigInt, String, TesslaCore.Value) => Unit): Unit = {
    val queue = new TracesQueue(threshold, abortAt)

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
    var abort = false
    def handleInput(event: Traces.Event) {
      if (abortAt.isEmpty || event.timeRange.from <= abortAt.get) {
        if (event.timeRange.from - previousTS != 0) {
          tesslaSpec.step(event.timeRange.from - previousTS)
          previousTS = event.timeRange.from
        }
        provide(event)
      }else{
        abort = true
      }
    }

    tesslaSpec.addOutStreamListener(callback)

    values.takeWhile{
      value => !abort
    }.foreach{value =>
      if (!abort) {
        queue.enqueue(value, handleInput)
      }
    }
    
    //in the end handle every remaining event from the queue
    queue.processAll(handleInput)

    tesslaSpec.step()
  }
}

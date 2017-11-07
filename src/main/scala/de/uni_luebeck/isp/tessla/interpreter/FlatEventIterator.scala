package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.NegativeStepError
import de.uni_luebeck.isp.tessla.interpreter.RawTrace.{EventRange, TimeRange}

import scala.collection.mutable

class FlatEventIterator(eventRanges: Iterator[EventRange], abortAt: Option[BigInt]) extends Iterator[Trace.Event] {
  val queue: mutable.PriorityQueue[EventRange] =
    new mutable.PriorityQueue[EventRange]()(Ordering.by(ev => ev.timeRange.from)).reverse
  var nextEvents = new mutable.Queue[Trace.Event]
  var eventCounter = 0

  private def generateEvent: Option[Trace.Event] = {
    if (queue.nonEmpty) {
      val generator = queue.dequeue
      val range = generator.timeRange
      if (range.step <= 0) {
        throw NegativeStepError(generator.loc, range.step)
      }
      val diff = range.to.map(_ - range.from)
      if (diff.forall(_ >= 0) && abortAt.forall(eventCounter < _)) {
        if (diff.forall(_ >= range.step)) {
          queue.enqueue(EventRange(generator.loc,
            TimeRange(range.loc, range.id, range.from + range.step, range.to, range.step),
            generator.stream, generator.value))
        }
        eventCounter += 1
        Some(Trace.Event(generator.loc, Trace.TimeStamp(range.loc, range.from),
          generator.stream, generator.evalValue))
      } else {
        None
      }
    } else {
      None
    }
  }

  def gatherValues(): Unit = {
    nextEvents ++= generateEvent.toList
    while (abortAt.forall(eventCounter < _) && nextEvents.isEmpty && eventRanges.hasNext) {
      queue.enqueue(eventRanges.next)
      nextEvents ++= generateEvent.toList
    }
  }

  override def hasNext = {
    gatherValues()
    nextEvents.nonEmpty
  }

  override def next = {
    gatherValues()
    nextEvents.dequeue
  }
}

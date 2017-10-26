package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.interpreter.Trace.{EventRange, TimeRange}

import scala.collection.mutable

class EventQueue(eventRanges: Iterator[EventRange]) extends Iterator[Trace.Event] {
  val queue: mutable.PriorityQueue[Trace.EventRange] =
    new mutable.PriorityQueue[Trace.EventRange]()(Ordering.by(ev => ev.timeRange.from)).reverse
  var nextEvents = new mutable.Queue[Trace.Event]

  private def generateEvent : Option[Trace.Event] = {
    if (queue.nonEmpty) {
      val generator = queue.dequeue
      val range = generator.timeRange
      if (range.to.isEmpty || range.to.get - range.from >= range.step) {
        queue.enqueue(Trace.EventRange(generator.loc,
          TimeRange(range.id, range.from + range.step, range.to, range.step),
          generator.stream, generator.value))
      }
      Some(Trace.Event(generator.loc, Trace.TimeStamp(generator.loc, range.from),
        generator.stream, generator.evalValue))
    }else{
      None
    }
  }

  def gatherValues(): Unit = {
    nextEvents ++= generateEvent.toList
    while (nextEvents.isEmpty && eventRanges.hasNext){
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

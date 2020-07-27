package de.uni_luebeck.isp.tessla.interpreter

import java.util.NoSuchElementException

import de.uni_luebeck.isp.tessla.Location

class EventIterator(events: Iterator[TraceParser.Event], abortAt: Option[BigInt]) extends Iterator[Trace.Event] {
  private var eventCounter = 0

  override def hasNext: Boolean = abortAt.forall(eventCounter <= _) && events.hasNext

  override def next: Trace.Event = {
    if (!hasNext) throw new NoSuchElementException
    eventCounter += 1
    val event = events.next()

    val id = Trace.Identifier(event.streamName.getText, Location.fromToken(event.streamName))
    val value = TraceExpressionEvaluator.eval(event.expression)
    val time = Trace.TimeStamp(Location.fromNode(event.timestamp), BigInt(event.timestamp.getText))
    Trace.Event(event.loc, time, Some(id), value)
  }
}
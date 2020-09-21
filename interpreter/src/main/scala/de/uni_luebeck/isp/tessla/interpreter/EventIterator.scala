/*
 * Copyright (c) 2020 Institute of Software Engineering and Programming Languages,
 * University of Lübeck, Germany
 *
 * Modified MIT license:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this binary (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software and the code which is
 * generated by the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.uni_luebeck.isp.tessla.interpreter

import java.util.NoSuchElementException

import de.uni_luebeck.isp.tessla.core.Location

/** An [[Interpreter.Trace]] generated from [[de.uni_luebeck.isp.tessla.interpreter.TraceParser.Event]]s.
 *
  * This makes use of the [[de.uni_luebeck.isp.tessla.interpreter.TraceExpressionEvaluator]] to evaluate the event data.
 *
  * @param events the parsed input events
 * @param abortAt the amount of events after which the trace should stop
 */

class EventIterator(events: Iterator[TraceParser.Event], abortAt: Option[BigInt]) extends Iterator[Trace.Event] {
  private var eventCounter = 0

  override def hasNext: Boolean = abortAt.forall(eventCounter <= _) && events.hasNext

  override def next(): Trace.Event = {
    if (!hasNext) throw new NoSuchElementException()
    eventCounter += 1
    val event = events.next()

    val id = Trace.Identifier(event.streamName.getText, Location.fromToken(event.streamName))
    val value = TraceExpressionEvaluator.eval(event.expression)
    val time = Trace.TimeStamp(Location.fromNode(event.timestamp), BigInt(event.timestamp.getText))
    Trace.Event(event.loc, time, Some(id), value)
  }
}

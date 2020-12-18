/*
 * Copyright 2020 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

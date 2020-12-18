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

import de.uni_luebeck.isp.tessla.core.Location
import de.uni_luebeck.isp.tessla.interpreter.Trace.TimeStamp
import org.eclipse.tracecompass.ctf.core.CTFException
import org.eclipse.tracecompass.ctf.core.trace.CTFTraceReader

/** An [[Interpreter.Trace]] generated from a CTF trace.
 *
  * @param reader the reader providing the trace data
 * @param abortAt the amount of events after which the trace should stop
 */

class CtfEventIterator(reader: CTFTraceReader, abortAt: Option[BigInt]) extends Interpreter.Trace {
  private var eventCounter = 0

  override def hasNext: Boolean = reader.hasMoreEvents && abortAt.forall(eventCounter < _)

  override def next(): Trace.Event = {
    val event = reader.getCurrentEventDef
    try reader.advance()
    catch {
      case e: CTFException => throw new RuntimeException(e)
    }

    val ts = TimeStamp(Location.unknown, BigInt(event.getTimestamp))
    val stream = Trace.Identifier(event.getDeclaration.getName, Location.unknown)
    eventCounter += 1
    Trace.Event(Location.unknown, ts, Some(stream), event)
  }
}

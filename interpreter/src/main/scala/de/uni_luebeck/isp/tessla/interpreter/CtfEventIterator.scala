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

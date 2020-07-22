package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.interpreter.Trace.TimeStamp
import org.eclipse.tracecompass.ctf.core.CTFException
import org.eclipse.tracecompass.ctf.core.trace.CTFTraceReader

class CtfEventIterator(reader: CTFTraceReader, abortAt: Option[BigInt]) extends Iterator[Trace.Event] {
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

package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Location, TesslaCore, TimeUnit}
import de.uni_luebeck.isp.tessla.Errors.InternalError
import org.eclipse.tracecompass.ctf.core.CTFException
import org.eclipse.tracecompass.ctf.core.trace.{CTFTrace, CTFTraceReader}

object Trace {
  sealed abstract class Item

  type Identifier = RawTrace.Identifier
  val Identifier = RawTrace.Identifier

  case class Event(loc: Location, timeStamp: TimeStamp, streamOpt: Option[Identifier], value: TesslaCore.Value) extends Item {
    override def toString: String = streamOpt match {
      case Some(stream) => s"$timeStamp: ${stream.name} = $value"
      case None => value match {
        case str: TesslaCore.StringValue => str.value
        case _ => value.toString
      }
    }

    def stream = streamOpt match {
      case Some(stream) => stream
      case None => throw InternalError("Requested name of print stream")
    }
  }

  case class TimeUnitDeclaration(timeUnit: TimeUnit) extends Item {
    override def toString = {
      "$timeunit = \"" + timeUnit + "\""
    }
  }

  case class TimeStamp(loc: Location, time: Specification.Time) {
    override def toString = time.toString
  }

  def fromCtfFile(ctfFileName: String, abortAt: Option[BigInt]) = {
    val reader = new CTFTraceReader(new CTFTrace(ctfFileName))

    val iterator = new Iterator[Trace.Event] {
      private var eventCounter = 0

      override def hasNext = reader.hasMoreEvents && abortAt.forall(eventCounter < _)

      override def next() = {
        val event = reader.getCurrentEventDef
        try {
          reader.advance
        } catch {
          case e: CTFException =>
            throw new RuntimeException(e)
        }
        val ts = TimeStamp(Location.unknown, BigInt(event.getTimestamp))
        val stream = Trace.Identifier(Location.unknown, event.getDeclaration.getName)
        val value = TesslaCore.Ctf(event.getFields, Location.unknown)
        eventCounter += 1
        Trace.Event(Location.unknown, ts, Some(stream), value)
      }
    }

    new Trace(None, iterator)
  }

}

class Trace(val timeStampUnit: Option[TimeUnit], val events: Iterator[Trace.Event]) extends Iterator[Trace.Item] {
  val items: Iterator[Trace.Item] = timeStampUnit match {
    case Some(timeUnit) => Iterator(Trace.TimeUnitDeclaration(timeUnit)) ++ events
    case None => events
  }

  override def hasNext = items.hasNext

  override def next() = items.next()
}
package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Location, Tessla, TesslaCore, TimeUnit}
import de.uni_luebeck.isp.tessla.Errors.InternalError
import org.antlr.v4.runtime.{CharStream, CharStreams}
import org.eclipse.tracecompass.ctf.core.CTFException
import org.eclipse.tracecompass.ctf.core.trace.{CTFTrace, CTFTraceReader}

object Trace {
  type Identifier = Tessla.Identifier
  val Identifier = Tessla.Identifier

  case class Event(loc: Location, timeStamp: TimeStamp, streamOpt: Option[Identifier], value: TesslaCore.Value) {
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

  case class TimeStamp(loc: Location, time: Specification.Time) {
    override def toString = time.toString
  }

  def fromCtfFile(ctfFileName: String, abortAt: Option[BigInt]) = {
    val reader = new CTFTraceReader(new CTFTrace(ctfFileName))

    new Iterator[Trace.Event] {
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
        val stream = Trace.Identifier(event.getDeclaration.getName, Location.unknown)
        val value = TesslaCore.Ctf(event.getFields, Location.unknown)
        eventCounter += 1
        Trace.Event(Location.unknown, ts, Some(stream), value)
      }
    }
  }

  def fromSource(traceSource: CharStream, abortAt: Option[Specification.Time] = None) = {
    val rawTrace = new TraceParser().parseTrace(traceSource)
    new FlatEventIterator(rawTrace, abortAt)
  }

  def fromFile(fileName: String, abortAt: Option[Specification.Time] = None) =
    fromSource(CharStreams.fromFileName(fileName), abortAt)
}
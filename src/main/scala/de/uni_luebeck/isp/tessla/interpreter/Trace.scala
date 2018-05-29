package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Location, TesslaCore}
import de.uni_luebeck.isp.tessla.TimeUnit

object Trace {
  sealed abstract class Item

  type Identifier = RawTrace.Identifier
  val Identifier = RawTrace.Identifier

  case class Event(loc: Location, timeStamp: TimeStamp, stream: Identifier, value: TesslaCore.Value) extends Item {
    override def toString: String = s"$timeStamp: ${stream.name} = $value"
  }

  case class TimeUnitDeclaration(timeUnit: TimeUnit) extends Item {
    override def toString = {
      "$timeunit = \"" + timeUnit + "\""
    }
  }

  case class TimeStamp(loc: Location, time: Specification.Time) {
    override def toString = time.toString
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
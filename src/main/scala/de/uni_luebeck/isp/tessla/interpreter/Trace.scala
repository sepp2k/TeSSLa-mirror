package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.TesslaCore
import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit

object Trace {
  sealed abstract class Item

  case class Event(loc: Location, timeStamp: TimeStamp, stream: Identifier, value: TesslaCore.LiteralValue) extends Item {
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

  case class Identifier(loc: Location, name: String) {
    override def toString: String = "\"" + name + "\""
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

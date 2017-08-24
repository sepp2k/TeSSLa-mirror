package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.TesslaCore

object Input {
  sealed trait Line {
    def loc: Location
  }

  case class TimeUnit(loc: Location, timeUnit: de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit) extends Line {
    override def toString = timeUnit.toString
  }

  case class Event(loc: Location, timeStamp: BigInt, stream: Identifier, value: TesslaCore.LiteralValue) extends Line {
    override def toString = s"$timeStamp: $stream = $value"
  }

  case class Identifier(loc: Location, name: String) {
    override def toString = "\"" + name + "\""
  }

}

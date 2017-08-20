package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.compacom.Location
import de.uni_luebeck.isp.tessla.TesslaCore

/**
  * Created by Larissa on 19.08.2017.
  */
object Input {
  case class Spec(lines: Seq[Line]) {
    override def toString = lines.mkString("\n")
  }

  abstract sealed class Line(loc: Location)

  case class TimeUnit(loc: Location, timeUnit: de.uni_luebeck.isp.tessla.TimeUnit.TimeUnit) extends Line(loc){
    override def toString = "$timeunit = " + timeUnit
  }

  case class Event(loc: Location, timeStamp: BigInt, name: Identifier, value: TesslaCore.LiteralValue) extends Line(loc){
    override def toString = s"$timeStamp: $name = $value"
  }

  case class Identifier(loc: Location, name: String){
    override def toString = name
  }
}

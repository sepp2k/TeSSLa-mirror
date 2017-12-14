package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.UnknownTimeUnit

sealed abstract class TimeUnit {
  val factor: BigInt

  def loc: Location

  def <(that: TimeUnit): Boolean = factor < that.factor

  def convertTo(that: TimeUnit): BigInt = factor / that.factor
}

object TimeUnit {
  def fromString(str: String, loc: Location): TimeUnit = str.replaceAll("\"", "") match {
    case "fs" => Femtos(loc)
    case "ps" => Picos(loc)
    case "ns" => Nanos(loc)
    case "µs" => Micros(loc)
    case "us" => Micros(loc)
    case "ms" => Millis(loc)
    case "s" => Seconds(loc)
    case "min" => Minutes(loc)
    case "h" => Hours(loc)
    case "d" => Days(loc)
    case _ => throw UnknownTimeUnit(str, loc)
  }

  def parse(source: TesslaSource) = {
    val unitString = source.src.mkString("")
    fromString(unitString, Location.forWholeFile(unitString, source.path))
  }

  case class Femtos(loc: Location) extends TimeUnit {
    val factor = 1000

    override def toString: String = "fs"
  }

  case class Picos(loc: Location) extends TimeUnit {
    val factor = 1000000

    override def toString: String = "ps"
  }

  case class Nanos(loc: Location) extends TimeUnit {
    val factor = 1000000000

    override def toString: String = "ns"
  }

  case class Micros(loc: Location) extends TimeUnit {
    val factor = 1000000000000L

    override def toString: String = "us"
  }

  case class Millis(loc: Location) extends TimeUnit {
    val factor = 1000000000000000L

    override def toString: String = "ms"
  }

  case class Seconds(loc: Location) extends TimeUnit {
    val factor = 1000000000000000000L

    override def toString: String = "s"
  }

  case class Minutes(loc: Location) extends TimeUnit {
    val factor = BigInt("60000000000000000000")

    override def toString: String = "min"
  }

  case class Hours(loc: Location) extends TimeUnit {
    val factor = BigInt("3600000000000000000000")

    override def toString: String = "h"
  }

  case class Days(loc: Location) extends TimeUnit {
    val factor = BigInt("86400000000000000000000")

    override def toString: String = "d"
  }

}
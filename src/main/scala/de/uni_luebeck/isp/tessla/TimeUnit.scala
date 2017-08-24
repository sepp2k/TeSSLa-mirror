package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.UnknownTimeUnit

/**
  * Created by Thiemo on 08.08.2017.
  */
object TimeUnit {
  def fromString(str: String): TimeUnit = str.replaceAll("\"", "") match {
    case "ns" => Nanos
    case "us" => Micros
    case "ms" => Millis
    case "s" => Seconds
    case "min" => Minutes
    case "h" => Hours
    case "d" => Days
    case _ => throw UnknownTimeUnit(str, UnknownLoc)
  }

  sealed abstract class TimeUnit {
    val factor: BigInt

    def <(that: TimeUnit): Boolean = factor < that.factor

    def convertTo(that: TimeUnit): BigInt = factor / that.factor
  }

  case object Nanos extends TimeUnit {
    val factor = 1

    override def toString: String = "ns"
  }

  case object Micros extends TimeUnit {
    val factor = 1000

    override def toString: String = "us"
  }

  case object Millis extends TimeUnit {
    val factor = 1000000

    override def toString: String = "ms"
  }

  case object Seconds extends TimeUnit {
    val factor = 1000000000

    override def toString: String = "s"
  }

  case object Minutes extends TimeUnit {
    val factor = 60000000000L

    override def toString: String = "min"
  }

  case object Hours extends TimeUnit {
    val factor = 3600000000000L

    override def toString: String = "h"
  }

  case object Days extends TimeUnit {
    val factor = 86400000000000L

    override def toString: String = "d"
  }

}
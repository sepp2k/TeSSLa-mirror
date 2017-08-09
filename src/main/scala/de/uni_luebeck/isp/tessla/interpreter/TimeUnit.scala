package de.uni_luebeck.isp.tessla.interpreter

/**
  * Created by Thiemo on 08.08.2017.
  */
object TimeUnit {
  def fromString(str: String): Unit = str match {
    case "ns" => Nanos
    case "us" => Micros
    case "ms" => Millis
    case "s" => Seconds
    case "m" => Minutes
    case "h" => Hours
    case "d" => Days
  }

  trait Unit {
    val factor: BigInt

    def <(that: Unit): Boolean = factor < that.factor

    def convertTo(that: Unit): BigInt = factor / that.factor
  }

  case object Nanos extends Unit {
    val factor = 1
    override def toString: String = "ns"
  }

  case object Micros extends Unit {
    val factor = 1000
    override def toString: String = "us"
  }

  case object Millis extends Unit {
    val factor = 1000000
    override def toString: String = "ms"
  }

  case object Seconds extends Unit {
    val factor = 1000000000
    override def toString: String = "ms"
  }

  case object Minutes extends Unit {
    val factor = 60000000000L
    override def toString: String = "ms"
  }

  case object Hours extends Unit {
    val factor = 3600000000000L
    override def toString: String = "h"
  }

  case object Days extends Unit {
    val factor = 86400000000000L
    override def toString: String = "d"
  }

}
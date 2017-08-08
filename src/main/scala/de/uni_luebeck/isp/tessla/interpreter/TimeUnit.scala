package de.uni_luebeck.isp.tessla.interpreter

/**
  * Created by Thiemo on 08.08.2017.
  */
trait TimeUnit {
  val factor: BigInt

  def <(that: TimeUnit): Boolean = factor < that.factor

  def convertTo(that: TimeUnit): BigInt = factor / that.factor
}

case object Nanos extends TimeUnit {
  val factor = 1
}

case object Micros extends TimeUnit {
  val factor = 1000
}

case object Millis extends TimeUnit {
  val factor = 1000000
}

case object Seconds extends TimeUnit {
  val factor = 1000000000
}

case object Minutes extends TimeUnit {
  val factor = 60000000000L
}

case object Hours extends TimeUnit {
  val factor = 3600000000000L
}

case object Days extends TimeUnit {
  val factor = 86400000000000L
}
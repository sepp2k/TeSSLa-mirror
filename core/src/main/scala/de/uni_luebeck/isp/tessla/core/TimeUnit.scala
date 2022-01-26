/*
 * Copyright 2022 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Errors.UnknownTimeUnit

/**
 * An abstract time unit. Provides functionality for comparison with and conversion to other time units.
 */
sealed abstract class TimeUnit {

  /**
   * The factor used to provide a common base unit for all time units, used for conversion and comparison.
   */
  val factor: BigInt

  def loc: Location

  /**
   * Compare the time unit to the provided one
   * @param that the time unit to compare to
   * @return true if the provided time unit is larger
   */
  def <(that: TimeUnit): Boolean = factor < that.factor

  /**
   * Calculate a conversion factor to convert numeric values from one time unit to another
   * @param that the time unit to convert to
   * @return the factor to use
   */
  def convertTo(that: TimeUnit): Option[BigInt] = {
    if (factor >= that.factor) Some(factor / that.factor)
    else None
  }
}

object TimeUnit {

  /**
   * Generate a time unit from a provided String. (E.g. "fs", "ms" or "d")
   * @param str the time unit as String.
   * @param loc the location
   * @return the parsed time unit.
   */
  def fromString(str: String, loc: Location): TimeUnit = {
    str.replaceAll("\"", "") match {
      case "fs"  => Femtos(loc)
      case "ps"  => Picos(loc)
      case "ns"  => Nanos(loc)
      case "µs"  => Micros(loc)
      case "us"  => Micros(loc)
      case "ms"  => Millis(loc)
      case "s"   => Seconds(loc)
      case "min" => Minutes(loc)
      case "h"   => Hours(loc)
      case "d"   => Days(loc)
      case _ =>
        throw UnknownTimeUnit(
          str,
          loc,
          List(
            "fs",
            "ps",
            "ns",
            "µs",
            "us",
            "ms",
            "s",
            "min",
            "h",
            "d"
          )
        )
    }
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

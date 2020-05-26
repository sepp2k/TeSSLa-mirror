package de.uni_luebeck.isp.tessla

import language.higherKinds
import scala.collection.mutable.{Map => MMap}

package object util {

  /**
   * `optionIf(cond) {foo}` evaluates `foo` and returns its value wrapped in a `Some` if `cond`
   * is true; otherwise `None` is returned.
   */
  def optionIf[T](condition: Boolean)(thenCase: => T): Option[T] = {
    if (condition) Some(thenCase) else None
  }

  def mapValues[K, V1, V2](map: Map[K, V1])(f: V1 => V2): Map[K, V2] = map.map {
    case (k, v) => k -> f(v)
  }
  def mapValues[K, V1, V2](map: MMap[K, V1])(f: V1 => V2): MMap[K, V2] = map.map {
    case (k, v) => k -> f(v)
  }
}

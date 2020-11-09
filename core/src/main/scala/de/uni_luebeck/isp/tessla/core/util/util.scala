/*

 */

package de.uni_luebeck.isp.tessla.core

import scala.collection.mutable.{Map => MMap}

package object util {

  def mapValues[K, V1, V2](map: Map[K, V1])(f: V1 => V2): Map[K, V2] = map.view.mapValues(f).toMap
  def mapValues[K, V1, V2](map: MMap[K, V1])(f: V1 => V2): MMap[K, V2] = map.view.mapValues(f).to(MMap)

  implicit class RichMap[K, +V](map: Map[K, V]) {
    def mapVals[W](f: V => W): Map[K, W] = map.view.mapValues(f).toMap
  }
}

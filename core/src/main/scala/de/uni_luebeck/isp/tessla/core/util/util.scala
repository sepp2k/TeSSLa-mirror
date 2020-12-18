/*
 * Copyright 2020 The TeSSLa Community
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

import scala.collection.mutable.{Map => MMap}

package object util {

  def mapValues[K, V1, V2](map: Map[K, V1])(f: V1 => V2): Map[K, V2] = map.view.mapValues(f).toMap
  def mapValues[K, V1, V2](map: MMap[K, V1])(f: V1 => V2): MMap[K, V2] = map.view.mapValues(f).to(MMap)

  implicit class RichMap[K, +V](map: Map[K, V]) {
    def mapVals[W](f: V => W): Map[K, W] = map.view.mapValues(f).toMap
  }
}

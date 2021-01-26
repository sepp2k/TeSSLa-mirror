/*
 * Copyright 2021 The TeSSLa Community
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

import scala.collection.mutable

/**
 * Applies a reverse topological sort on an arbitrary graph data structure.
 *
  * This is used in the [[TypeChecker]] to check for recursive definitions.
 */
object ReverseTopologicalSort {

  sealed abstract class Result[Node]

  final case class Sorted[Node](nodes: Seq[Node]) extends Result[Node]

  final case class Cycles[Node](nodesInCycles: Seq[Node]) extends Result[Node]

  def sort[Node](startNodes: Iterable[Node])(getChildren: Node => Seq[Node]): Result[Node] = {
    val visited = mutable.Set[Node]()
    val nodesInCycles = mutable.ArrayBuffer[Node]()

    var nodes = List[Node]()

    def go(current: Node, stack: List[Node]): Unit = {
      if (stack.contains(current)) {
        nodesInCycles ++= stack.slice(0, stack.indexOf(current) + 1)
      } else if (!visited.contains(current)) {
        getChildren(current).foreach(go(_, current :: stack))
        visited += current
        nodes = current :: nodes
      }
    }

    startNodes.foreach { startNode =>
      go(startNode, List())
    }
    if (nodesInCycles.isEmpty) {
      Sorted(nodes.reverse)
    } else {
      Cycles(nodesInCycles.toSeq)
    }
  }
}

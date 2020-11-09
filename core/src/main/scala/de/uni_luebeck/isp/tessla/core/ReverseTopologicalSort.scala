/*

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

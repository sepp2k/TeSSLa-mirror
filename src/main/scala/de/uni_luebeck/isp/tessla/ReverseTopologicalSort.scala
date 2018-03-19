package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object ReverseTopologicalSort {
  sealed abstract class Result[Node]
  final case class Sorted[Node](nodes: Iterable[Node]) extends Result[Node]
  final case class Cycles[Node](nodesInCycles: Seq[Node]) extends Result[Node]

  def sort[Node](startNodes: Iterable[Node])(getChildren: Node => Seq[Node]): Result[Node] = {
    val visited = mutable.Set[Node]()
    val nodesInCycles = mutable.ArrayBuffer[Node]()
    def go(current: Node, stack: Seq[Node]): Seq[Node] = {
      if (stack.contains(current)) {
        nodesInCycles ++= stack.slice(0, stack.indexOf(current) + 1)
        Seq()
      } else if (visited.contains(current)) {
        Seq()
      } else {
        visited += current
        current +: getChildren(current).flatMap(go(_, current +: stack))
      }
    }
    val nodes = startNodes.flatMap { startNode =>
      if (visited(startNode)) Seq()
      else go(startNode, Seq()).reverse
    }
    if (nodesInCycles.isEmpty) {
      Sorted(nodes)
    } else {
      Cycles(nodesInCycles)
    }
  }
}
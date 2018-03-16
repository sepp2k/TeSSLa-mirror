package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object ReverseTopologicalSort {
  sealed abstract class Result[Node]
  final case class Sorted[Node](nodes: Iterable[Node]) extends Result[Node]
  final case class Cycles[Node](cycleStarts: Seq[Node]) extends Result[Node]

  def sort[Node](startNodes: Iterable[Node])(getChildren: Node => Seq[Node]): Result[Node] = {
    val visited = mutable.Set[Node]()
    var cyclesStarts: Seq[Node] = Seq()
    def go(current: Node, stack: Set[Node]): Seq[Node] = {
      if (stack.contains(current)) {
        cyclesStarts = current +: cyclesStarts
        Seq()
      } else if (visited.contains(current)) {
        Seq()
      } else {
        visited += current
        current +: getChildren(current).flatMap(go(_, stack + current))
      }
    }
    val nodes = startNodes.flatMap { startNode =>
      if (visited(startNode)) Seq()
      else go(startNode, Set()).reverse
    }
    if (cyclesStarts.isEmpty) {
      Sorted(nodes)
    } else {
      Cycles(cyclesStarts)
    }
  }
}
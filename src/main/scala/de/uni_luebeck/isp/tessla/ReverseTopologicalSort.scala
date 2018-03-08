package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object ReverseTopologicalSort {
  sealed abstract class Result[Node]
  final case class Sorted[Node](nodes: Iterable[Node]) extends Result[Node]
  final case class Cycles[Node](cycleStarts: Seq[Node]) extends Result[Node]

  def sort[Node](startNodes: Iterable[Node])(getChildren: Node => Iterable[Node]): Result[Node] = {
    val visited = mutable.Set[Node]()
    var cyclesStarts: Seq[Node] = Seq()
    def go(current: Node): Iterable[Node] = {
      if (visited.contains(current)) {
        cyclesStarts = current +: cyclesStarts
        Seq()
      } else {
        visited += current
        getChildren(current).flatMap(go)
      }
    }
    val nodes = startNodes.flatMap(go)
    if (cyclesStarts.isEmpty) {
      Sorted(nodes)
    } else {
      Cycles(cyclesStarts)
    }
  }
}
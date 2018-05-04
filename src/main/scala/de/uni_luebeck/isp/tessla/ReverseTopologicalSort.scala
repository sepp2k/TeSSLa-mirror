package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object ReverseTopologicalSort {
  sealed abstract class Result[Node]
  final case class Sorted[Node](nodes: Iterable[Node]) extends Result[Node]
  final case class Cycles[Node](nodesInCycles: Seq[Node]) extends Result[Node]

  sealed abstract class Mark
  case object Permanent extends Mark
  case object Temporary extends Mark

  def sort[Node](startNodes: Iterable[Node])(getChildren: Node => Seq[Node]): Result[Node] = {
    val marks = mutable.Map[Node, Mark]()
    val nodesInCycles = mutable.ArrayBuffer[Node]()

    var nodes = List[Node]()

    def go(current: Node, stack: List[Node]): Unit = {
      marks.get(current) match {
        case Some(Temporary) =>
          nodesInCycles ++= stack.slice(0, stack.indexOf(current) + 1)
        case Some(Permanent) =>
          // do nothing
        case None =>
          marks(current) = Temporary
          getChildren(current).foreach(go(_, current :: stack))
          marks(current) = Permanent
          nodes = current :: nodes
      }
    }
    startNodes.foreach { startNode =>
      if (!marks.contains(startNode))
        go(startNode, List())
    }
    if (nodesInCycles.isEmpty) {
      Sorted(nodes.reverse)
    } else {
      Cycles(nodesInCycles)
    }
  }
}
package de.uni_luebeck.isp.tessla
import scala.collection.mutable

class FunctionGraph extends WithDebugOutput {
  class NodeId {
    def node: Node = nodes(this)
  }

  case class Node(id: NodeId, function: Function, args: Seq[NodeId])

  var nodes: mutable.Map[NodeId, Node] = mutable.Map()

  def addNode(function: Function, args: Seq[NodeId]): NodeId = {
    val nodeId = new NodeId
    nodes(nodeId) = Node(nodeId, function, args)
    nodeId
  }

  /**
    * Debug output in DOT format
    *
    * @return DOT graph representation as String
    */
  override def debugOutput: String = {
    val builder = new StringBuilder()
    builder ++= "digraph {\n"

    val number = nodes.keys.zipWithIndex.toMap

    for (id <- nodes.keys) {
      // TODO escape string
      val idNr = number(id)
      val label = number(id) + ": " + id.node.toString
      builder ++= s"""  n$idNr [label = "$label" shape = box];\n"""
      for (targetId <- id.node.args) {
        val targetIdNr = number(targetId)
        builder ++= s"""  n$idNr -> n$targetIdNr\n"""
      }
    }

    builder ++= "}"
    builder.toString
  }

}

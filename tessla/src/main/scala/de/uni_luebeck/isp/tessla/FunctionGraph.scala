package de.uni_luebeck.isp.tessla
import scala.collection.mutable

class FunctionGraph {
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
}

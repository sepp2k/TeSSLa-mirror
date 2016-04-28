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

    val number = nodes.keys.zipWithIndex.toMap

    def mkString(n: Node) = {

      n.function match {
        case SimpleFunction(name, FunctionSig(ret, args)) => s"${name}: (${args.mkString("×")}) → $ret"
        case f: TypeAscription[_] => s"of Type ${f.`type`}"
        case MonitorFunction(name, FunctionSig(ret, args)) => s"Monitor ${name}: ${args.mkString("×")} → $ret"
        case f: StateMachineFunction => s"Monitor ${f.name}: ${f.signature.args.mkString("×")} → ${f.signature.ret}"
        case ConstantValue(t, v) => s"$v: $t"
      }
    }

    val builder = new StringBuilder()
    builder ++= "digraph {\n"

    for (id <- nodes.keys) {
      // TODO escape string
      val idNr = number(id)
      val label = "#" + number(id) + " " + mkString(id.node)
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

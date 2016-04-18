package de.uni_luebeck.isp.tessla

import org.json4s._
import org.json4s.JsonDSL._
import scala.util.Try

object ModuleMapper extends CompilerPass[FunctionGraph, ModuleGraph] {
  override def apply(compiler: Compiler, graph: FunctionGraph) = Try {
    import graph.{Node, NodeId}

    val nodes: List[((NodeId, Node), Int)] = graph.nodes.toList.zipWithIndex
    val ref: Map[NodeId, JObject] = nodes.map { case ((nodeId, node), idx) => (nodeId, ("@ref" -> idx):JObject) }.toMap

    def toJObject(elem: ((NodeId, Node), Int)): JObject = {
      val node = elem._1._2

      def simpleFunToJObject(name: String, signature: FunctionSig): (String, JObject) = (name, signature) match {
        case ("add", FunctionSig(SimpleType("Int"), Seq((None, SimpleType("Int")), (None, SimpleType("Int"))))) => {
          //TODO: constant folding!
          ("dataFlowGraph.node.operation.AddNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        }
        case ("add", FunctionSig(
        GenericType("Signal", Seq(SimpleType("Int"))),
        Seq((None, GenericType("Signal", Seq(SimpleType("Int")))),
        (None, GenericType("Signal", Seq(SimpleType("Int"))))))) => {
          ("dataFlowGraph.node.operation.AddNode",
            ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        }
        case ("sub", FunctionSig(SimpleType("Int"), Seq((None, SimpleType("Int")), (None, SimpleType("Int"))))) => {
          //TODO: constant folding!
          ("dataFlowGraph.node.operation.SubNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        }
        case ("constantSignal", FunctionSig(GenericType("Signal", retTypeArgs: Seq[Type]), typeArgs: Seq[(Option[String], Type)])) => {
          ("dataFlowGraph.node.operation.ConstantNode", "value" -> ref(node.args(0)))
        }
        case (n,_) => ("GenericModule", ("predecessors" -> node.args.map(ref(_))) ~ ("name" -> n))
      }

      val (typeString: String, specificMembers: JObject) = node.function match {
        case SimpleFunction(name, signature) => simpleFunToJObject(name, signature)
        case ConstantValue(t: Type, value:Any) => {
          //("dataFlowGraph.node.input.ConstantNode", "value" -> value.toString)
          ("dataFlowGraph.node.input.ValueNode", ("value" -> value.toString):JObject)
        }
        case TypeAscription(t: Type) => ???
      }

      ((("@id" -> elem._2) ~ ("@type" -> typeString))
        ~ ("outputWidth" -> -1)
        ~ specificMembers)
    }


    ModuleGraph(("@type" -> "java.util.Collections$UnmodifiableSet") ~
      ("@items" -> nodes.map(toJObject)))

  }
}

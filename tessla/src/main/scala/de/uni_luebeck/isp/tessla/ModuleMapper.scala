package de.uni_luebeck.isp.tessla

import org.json4s._
import org.json4s.JsonDSL._
import scala.util.Try

object ModuleMapper extends CompilerPass[FunctionGraph, ModuleGraph] {

  case class UnfoldedLiteralError(function: Function) extends Fatal

  case class GenericModuleWarning(function: Function) extends Diagnostic

  override def apply(compiler: Compiler, graph: FunctionGraph) = Try {
    import graph.{Node, NodeId}

    val idx: Map[NodeId, Int] = graph.nodes.keys.zipWithIndex.toMap
    val ref: Map[NodeId, JObject] = idx.mapValues(i => ("@ref" -> i))

    def toJObject(nodeId: NodeId): Option[JObject] = {
      val node = nodeId.node

      val moduleAndMembers: Option[(String, JObject)] = node.function match {
        case ConstantValue(_, _) => None
        /*case TypeAscription(_) => {
          compiler.diagnostic(GenericModuleWarning(node.function))
          None
        }*/
        case SimpleFunction(_, FunctionSig(SimpleType(_), _)) => {
          compiler.diagnostic(UnfoldedLiteralError(node.function))
          None
        }
        case SimpleFunction("add", FunctionSig(GenericType("Signal", Seq(SimpleType("Int"))),
        Seq((None, GenericType("Signal", Seq(SimpleType("Int")))), (None, GenericType("Signal", Seq(SimpleType("Int")))))))
        => {
          Some("dataFlowGraph.node.operation.AddNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        }
        case SimpleFunction("sub", FunctionSig(GenericType("Signal", Seq(SimpleType("Int"))),
        Seq((None, GenericType("Signal", Seq(SimpleType("Int")))), (None, GenericType("Signal", Seq(SimpleType("Int")))))))
        => {
          Some("dataFlowGraph.node.operation.SubNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        }
        case SimpleFunction("constantSignal", _) => try {
          Some("dataFlowGraph.node.operation.ConstantNode", ("value" -> node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString): JObject)
        } catch {
          case e: ClassCastException =>
            compiler.diagnostic(UnfoldedLiteralError(node.function))
            None
        }
        case SimpleFunction("instruction_executions", _) => try {
          Some("dataFlowGraph.node.input.InstructionExecutionsNode",
            JObject("argument" -> JString(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString))
          )
        } catch {
          case e: ClassCastException =>
            compiler.diagnostic(UnfoldedLiteralError(node.function))
            None
        }
        case SimpleFunction("function_calls", _) => try {
          Some("dataFlowGraph.node.input.FunctionCallsNode",
            JObject("argument" -> JString(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString))
          )
        } catch {
          case e: ClassCastException =>
            compiler.diagnostic(UnfoldedLiteralError(node.function))
            None
        }
        case SimpleFunction("eventCount", _) =>
          Some("dataFlowGraph.node.operation.EventCountNode", JObject("input" -> ref(node.args(0))))
        case SimpleFunction("not", _) =>
          Some("dataFlowGraph.node.operation.NotNode", JObject("predecessor" -> ref(node.args(0))))
        case SimpleFunction("gt", _) =>
          Some("dataFlowGraph.node.operation.GTNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))


        case SimpleFunction("filter", _) =>
          Some("dataFlowGraph.node.operation.FilterNode",
            JObject("inputEvents" -> ref(node.args(0))) ~ ("conditionSignal" -> ref(node.args(1)))
          )
        case SimpleFunction(n, _) => {
          compiler.diagnostic(GenericModuleWarning(node.function))
          Some("GenericModule",
            ("inputs" ->
              node.args.map { nodeId => nodeId.node.function match {
                case ConstantValue(_, value) => JString(value.toString)
                case _ => ref(nodeId)
              }
              }) ~ ("name" -> n))
        }
      }

      moduleAndMembers.map {
        case (typeString, members) => ((("@id" -> idx(nodeId)) ~ ("@type" -> typeString)) ~ ("outputWidth" -> -1) ~ members)
      }
    }

    ModuleGraph(("@type" -> "java.util.Collections$UnmodifiableSet") ~ ("@items" -> graph.nodes.keys.flatMap(toJObject)))
  }
}

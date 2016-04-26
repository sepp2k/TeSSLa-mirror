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
        case StateMachineFunction(name, signature, start, stateMap, transitionList) => {
          val refs = node.args.map(x => ref(x))
          val refTransitions = transitionList.map(t => (t._1,t._2.map(x => refs(x-1)),t._3))
          val jsonTransitions = seq2jvalue(refTransitions.map(t => ("current" -> t._1) ~ ("active" -> t._2) ~ ("next" -> t._3)))
          Some("dataFlowGraph.node.operation.MonitorNode", ("predecessors" -> refs) ~ ("start" -> start) ~ ("states" -> stateMap) ~ ("transitions" -> jsonTransitions))
        }
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
        case SimpleFunction("function_returns", _) => try {
          Some("dataFlowGraph.node.input.FunctionReturnsNode",
            JObject("argument" -> JString(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString))
          )
        } catch {
          case e: ClassCastException =>
            compiler.diagnostic(UnfoldedLiteralError(node.function))
            None
        }
        case SimpleFunction("input_vector_timestamps", _) =>
          Some("dataFlowGraph.node.input.InputVectorNode", JObject("argument" -> JString("timestamp")))
        case SimpleFunction("input_vector_ownerships", _) =>
          Some("dataFlowGraph.node.input.InputVectorNode", JObject("argument" -> JString("ownership")))
        case SimpleFunction("anyEvent", _) =>
          Some("dataFlowGraph.node.input.InputVectorNode", JObject("argument" -> JString("valid")))


        case SimpleFunction("timestamps", _) =>
          Some("dataFlowGraph.node.operation.TimestampNode", JObject("predecessor" -> ref(node.args(0))))
        case SimpleFunction("eventCount", _) =>
          Some("dataFlowGraph.node.operation.EventCountNode", JObject("predecessor" -> ref(node.args(0))))
        case SimpleFunction("mrv", _) =>
          Some("dataFlowGraph.node.operation.MostRecentValueNode",
            JObject("predecessor" -> ref(node.args(0))) ~ ("initialValue" -> JString(node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString))
          )
        case SimpleFunction("not", _) =>
          Some("dataFlowGraph.node.operation.NotNode", JObject("predecessor" -> ref(node.args(0))))

        case SimpleFunction("and", _) =>
          Some("dataFlowGraph.node.operation.AndNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("or", _) =>
          Some("dataFlowGraph.node.operation.OrNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))

        case SimpleFunction("neg", _) =>
          Some("dataFlowGraph.node.operation.NegNode", JObject("predecessor" -> ref(node.args(0))))
        case SimpleFunction("gt", _) =>
          Some("dataFlowGraph.node.operation.GTNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("eq", _) =>
          Some("dataFlowGraph.node.operation.EQNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("occursAll", _) =>
          Some("dataFlowGraph.node.operation.OccursAllNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("merge", _) =>
          Some("dataFlowGraph.node.operation.MergeNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("inPast", _) =>
          Some("dataFlowGraph.node.operation.InPastNode", ("delay" -> ref(node.args(0))) ~ ("input" -> ref(node.args(1))))
        case SimpleFunction("ifThen", _) =>
          Some("dataFlowGraph.node.operation.IfThenNode", ("control" -> ref(node.args(0))) ~ ("trueNode" -> ref(node.args(1))))
        case SimpleFunction("delay", _) =>
          Some("dataFlowGraph.node.operation.DelayNode", ("predecessor" -> ref(node.args(0))): JObject)

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

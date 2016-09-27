package de.uni_luebeck.isp.tessla

import org.json4s._
import org.json4s.JsonDSL._
import scala.util.Try

object ModuleMapper extends CompilerPass[FunctionGraph, ModuleGraph] {

  case class UnfoldedLiteralError(function: Function) extends Fatal

  case class GenericModuleWarning(function: Function) extends Diagnostic

  override def apply(compiler: Compiler, graph: FunctionGraph) = Try {
    import graph.{Node, NodeId}

    /*** Coniras platform specific implementation of particular functions ***/

    //timestamps(e) := ifThen(e, input_vector_timestamp)
    // => if timestamps function is used, create a node for timestamps that can be references in the mapping
    val timestampsNode = graph.nodes.values.find{ _ match {
        case Node(_, SimpleFunction("timestamps", _), _) => true
        case _ => false
    }}
    val inputVectorTimestampNodeID = if (!timestampsNode.isEmpty) {

      val existingNode = graph.nodes.values.find{ _ match {
        case Node(_, SimpleFunction("input_vector_timestamps", _), _) => true
        case _ => false
      }}
      existingNode match {
        case None => Some(graph.addNode(
          // function signature will be ignored
          SimpleFunction("input_vector_timestamps", FunctionSig(new TypeVar, Seq())),
          Seq()
        ): NodeId)
        case Some(node) => Some(node.id)
      }
    } else None

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
        case SimpleFunction("out",_) => Some("dataFlowGraph.node.OutputNode",
          ("predecessor" -> ref(node.args(0))) ~
            ("name" -> JString(node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)))
        case StateMachineFunction(name, signature, start, stateMap, transitionList) => {
          val refs = node.args.map(x => ref(x))
          val refTransitions = transitionList.map(t => (t._1,t._2.map(x => refs(x-1)),t._3))
          val jsonTransitions = seq2jvalue(refTransitions.map(t => ("current" -> t._1) ~ ("active" -> t._2) ~ ("next" -> t._3)))
          val stateList = stateMap.toList
          val firstElement = stateList.filter(x => x._1.equals(start.toString())).head
          val sortedStateList = List(firstElement) ++ stateList.filter(x => !x._1.equals(start.toString()))
          val jsonStates = seq2jvalue(sortedStateList.map(s => ("name" -> s._1) ~ ("output" -> s._2)))
          Some("dataFlowGraph.node.operation.MonitorNode", ("predecessors" -> refs) ~ ("states" -> jsonStates) ~ ("transitions" -> jsonTransitions))
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
          Some("dataFlowGraph.node.operation.ConstantNode", ("value" -> JString(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)): JObject)
        } catch {
          case e: ClassCastException =>
            compiler.diagnostic(UnfoldedLiteralError(node.function))
            None
        }
        case SimpleFunction("executions", _) => try {
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
        case SimpleFunction("variable_values", _) => try {
          Some("dataFlowGraph.node.input.VariableValueNode",
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

//        case SimpleFunction("input_vector_ownerships", _) =>
//          Some("dataFlowGraph.node.input.InputVectorNode", JObject("argument" -> JString("ownership")))
//        case SimpleFunction("anyEvent", _) =>
//          Some("dataFlowGraph.node.input.MessageValidNode", JObject("argument" -> JString("valid")))
//        case SimpleFunction("tracePointID", _) =>
//          Some("dataFlowGraph.node.input.instructionReconstructionMessage.InstructionReconstructionID", JObject())


        case SimpleFunction("input_vector_timestamps", _) => Some("dataFlowGraph.node.input.MessageTimeStampNode", JObject())
        case SimpleFunction("timestamps", _) =>
          //Some("dataFlowGraph.node.operation.TimestampNode", JObject("predecessor" -> ref(node.args(0))))
          // timestamps(e) := ifThen(e, input_vector_timestamp)
          Some("dataFlowGraph.node.operation.IfThenNode", ("condition" -> ref(node.args(0))) ~ ("trueCase" -> ref(inputVectorTimestampNodeID.get)))

        case SimpleFunction("input_vector_ir_ids", _) => Some("dataFlowGraph.node.input.instructionReconstructionMessage.InstructionReconstructionID", JObject())
        //case SimpleFunction("executions", _) =>

        case SimpleFunction("input_vector_RegChangeMessageValue", _) => Some("dataFlowGraph.node.input.regChangeMessage.RegChangeMessageValue", JObject())
        case SimpleFunction("input_vector_RegChangeMessageID", _) => Some("dataFlowGraph.node.input.regChangeMessage.RegChangeMessageID", JObject())

        case SimpleFunction("eventCount", FunctionSig(_, Seq(_))) =>
          Some("dataFlowGraph.node.operation.EventCountNode", JObject("predecessor" -> ref(node.args(0))))
        case SimpleFunction("eventCount", FunctionSig(_, Seq(_,_))) =>
        Some("dataFlowGraph.node.operation.EventCountNode", JObject("predecessor" -> ref(node.args(0))) ~ ("reset" -> ref(node.args(1))))

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
          Some("dataFlowGraph.node.operation.NotNode", JObject("predecessor" -> ref(node.args(0))))
        case SimpleFunction("gt", _) =>
          Some("dataFlowGraph.node.operation.GreaterThanNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("eq", _) =>
          Some("dataFlowGraph.node.operation.EqualNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("occursAll", _) =>
          Some("dataFlowGraph.node.operation.OccursAllNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("occursAny", _) =>
          Some("dataFlowGraph.node.operation.OccursAnyNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("merge", _) =>
          Some("dataFlowGraph.node.operation.MergeNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(1))))
        case SimpleFunction("inPast", _) =>
          Some("dataFlowGraph.node.operation.InPastNode", ("delay" -> ref(node.args(0))) ~ ("input" -> ref(node.args(1))))
        case SimpleFunction("ifThen", _) =>
          Some("dataFlowGraph.node.operation.IfThenNode", ("condition" -> ref(node.args(0))) ~ ("trueCase" -> ref(node.args(1))))
        case SimpleFunction("ifThenElse", _) =>
          Some("dataFlowGraph.node.operation.IfThenElseNode", ("condition" -> ref(node.args(0))) ~ ("trueCase" -> ref(node.args(1))) ~ ("falseCase" -> ref(node.args(2))))
        case SimpleFunction("delay", _) =>
          Some("dataFlowGraph.node.operation.DelayNode", ("predecessor" -> ref(node.args(0))): JObject)

        case SimpleFunction("filter", _) =>
          Some("dataFlowGraph.node.operation.FilterNode",
            JObject("inputEvents" -> ref(node.args(0))) ~ ("conditionSignal" -> ref(node.args(1)))
          )
        case SimpleFunction("changeOf", _) =>
          Some("dataFlowGraph.node.operation.ChangeOfNode", ("predecessor" -> ref(node.args(0))): JObject)

        case SimpleFunction("on", _) =>
          Some("dataFlowGraph.node.operation.OccursAllNode", ("operandA" -> ref(node.args(0))) ~ ("operandB" -> ref(node.args(0))))

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

    // replace true/false by 1/0 in output to comply with FPGA configuration tool
    for (k <- graph.nodes.keys){
      k.node.function match {
        case ConstantValue(t, v: Boolean) => graph.nodes.update(k, k.node.copy(function = ConstantValue(t, if (v) 1 else 0)))
        case _ =>
      }
    }

    val items = graph.nodes.keys.flatMap(toJObject)
    ModuleGraph(("@type" -> "java.util.Collections$UnmodifiableSet") ~ ("@items" -> items))
  }
}

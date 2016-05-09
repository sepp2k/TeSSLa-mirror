package de.uni_luebeck.isp.tessla

import org.json4s.JsonDSL._
import org.json4s._

import scala.util.Try

object SoftwareMapper extends CompilerPass[FunctionGraph, SoftwareGraph] {

  case class UnfoldedLiteralError(function: Function) extends Fatal

  case class GenericModuleWarning(function: Function) extends Diagnostic

  override def apply(compiler: Compiler, graph: FunctionGraph) = Try {
    import graph.NodeId

    val idx: Map[NodeId, Int] = graph.nodes.keys.zipWithIndex.toMap
    val ref: Map[NodeId, JObject] = idx.mapValues(i => ("ref" -> i))

    def toJObject(nodeId: NodeId): Option[JObject] = {
      val node = nodeId.node

      val moduleAndMembers: Option[(String, JObject)] = node.function match {
        case ConstantValue(_, _) => None
        /*case TypeAscription(_) => {
          compiler.diagnostic(GenericModuleWarning(node.function))
          None
        }*/
        case StateMachineFunction(name, signature, start, stateMap, transitionList) =>
          val refs = node.args.map(x => ref(x))
          val refTransitions = transitionList.map(t => (t._1,t._2.map(x => refs(x-1)),t._3))
          val jsonTransitions = seq2jvalue(refTransitions.map(t => ("current" -> t._1) ~ ("active" -> t._2) ~ ("next" -> t._3)))
          val stateList = stateMap.toList
          val firstElement = stateList.filter(x => x._1.equals(start.toString)).head
          val sortedStateList = List(firstElement) ++ stateList.filter(x => !x._1.equals(start.toString))
          val jsonStates = seq2jvalue(sortedStateList.map(s => ("name" -> s._1) ~ ("output" -> s._2)))
          Some("node.monitors.Monitor", ("predecessors" -> refs) ~ ("states" -> jsonStates) ~ ("transitions" -> jsonTransitions))
        case SimpleFunction(_, FunctionSig(SimpleType(_), _)) =>
          compiler.diagnostic(UnfoldedLiteralError(node.function))
          None
        /**** Input/Constant functions ****/
        case SimpleFunction("literal", _) => // TODO more types
          Some("TesslaServer.Node.Literal",
            "options" ->
              ("value" ->
                Integer.parseInt(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)
              )
            )
        case SimpleFunction("instruction_executions", _) =>
          Some("TesslaServer.Source.InstructionExecutions",
            "options" ->
              ("instruction" -> node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)
          )
        case SimpleFunction("function_returns", _) =>
          Some("TesslaServer.Source.FunctionReturns",
            "options" ->
              ("function_name" -> node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)
          )
        case SimpleFunction("function_calls", _) =>
          Some("TesslaServer.Source.FunctionCalls",
            "options" ->
              ("function_name" -> node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)
          )
        case SimpleFunction("variable_values", _) =>
          Some("TesslaServer.Source.VariableValues",
            "options" ->
              ("variable_name" -> node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)
          )

        /**** Lifted ****/
        case SimpleFunction("abs", _) => // TODO more types
          Some("TesslaServer.Node.Lifted.Abs",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("add", _) =>
          Some("TesslaServer.Node.Lifted.Add",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("and", _) =>
          Some("TesslaServer.Node.Lifted.And",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("div", _) =>
          Some("TesslaServer.Node.Lifted.Div",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("eq", _) =>
          Some("TesslaServer.Node.Lifted.Eq",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("geq", _) =>
          Some("TesslaServer.Node.Lifted.Geq",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("gt", _) =>
          Some("TesslaServer.Node.Lifted.Gt",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("implies", _) =>
          Some("TesslaServer.Node.Lifted.Implies",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("leq", _) =>
          Some("TesslaServer.Node.Lifted.Leq",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("lt", _) =>
          Some("TesslaServer.Node.Lifted.Lt",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("max", _) =>
          Some("TesslaServer.Node.Lifted.Max",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("min", _) =>
          Some("TesslaServer.Node.Lifted.Min",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("mul", _) =>
          Some("TesslaServer.Node.Lifted.Mul",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("neg", _) =>
          Some("TesslaServer.Node.Lifted.Neg",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("not", _) => // TODO more types
          Some("TesslaServer.Node.Lifted.Not",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("or", _) =>
          Some("TesslaServer.Node.Lifted.Or",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("sub", _) =>
          Some("TesslaServer.Node.Lifted.Sub",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))

        /**** Filter ****/
        case SimpleFunction("merge", _) =>
          Some("TesslaServer.Node.Filter.Merge",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("filter", _) =>
          Some("TesslaServer.Node.Filter.Filter",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("ifThen", _) =>
          Some("TesslaServer.Node.Filter.IfThen",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("ifThenElse", _) =>
          Some("TesslaServer.Node.Filter.IfThenElse",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1)), idx(node.args(2))))
        case SimpleFunction("changeOf", _) =>
          Some("TesslaServer.Node.Filter.ChangeOf",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("sample", _) =>
          Some("TesslaServer.Node.Filter.Sample",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("occurAll", _) =>
          Some("TesslaServer.Node.Filter.OccurAll",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("occurAny", _) =>
          Some("TesslaServer.Node.Filter.OccurAny",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))

        /**** Aggregation ****/
        case SimpleFunction("maximum", _) => //TODO more types
          Some("TesslaServer.Node.Aggregation.Maximum",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" -> ("default" -> Integer.parseInt(
              node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
            ))
          )
        case SimpleFunction("minimum", _) => {//TODO more types
          Some("TesslaServer.Node.Aggregation.Minimum",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" -> ("default" -> Integer.parseInt(
              node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
              ))
          )
        }
        case SimpleFunction("sum", _) =>
          Some("TesslaServer.Node.Aggregation.Sum",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("eventCount", _) =>
          Some("TesslaServer.Node.Aggregation.EventCount",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("mrv", _) => // TODO more Types
          Some("TesslaServer.Node.Aggregation.Mrv",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" -> ("default" -> Integer.parseInt(
              node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
              ))
          )
        case SimpleFunction("sma", _) =>
          Some("TesslaServer.Node.Aggregation.Sma",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" -> ("count" -> Integer.parseInt(
              node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
              ))
          )

        /**** Timing ****/
        case SimpleFunction("timestamps", _) =>
          Some("TesslaServer.Node.Timing.Timestamps",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("delay", _) => //TODO more types
          Some("TesslaServer.Node.Timing.Delay",
            ("operands" -> Seq(idx(node.args(0))))
            ~ ("options" ->
                ("amount" -> node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
              )
          )
        case SimpleFunction("within", _) => // TODO switch int to time
          Some("TesslaServer.Node.Timing.Within",
            ("operands" -> Seq(ref(node.args(2))))
            ~ ("before" -> Integer.parseInt(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString))
            ~ ("after" -> Integer.parseInt(node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString))
          )
        case SimpleFunction("inPast", _) => // TODO switch int to time
          Some("TesslaServer.Node.Timing.InPast",
            ("operands" -> Seq(ref(node.args(1))))
            ~ ("options" ->
                ("amount" -> Integer.parseInt(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)))
          )
        case SimpleFunction("inFuture", _) => // TODO switch int to time
          Some("TesslaServer.Node.Timing.InFuture",
            ("operands" -> Seq(ref(node.args(1))))
              ~ ("options" ->
              ("amount" -> Integer.parseInt(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)))
          )
        /**** Error ****/
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
        case (typeString, members) => (("id" -> idx(nodeId)) ~ ("nodetype" -> typeString)) ~ members
      }
    }

    SoftwareGraph(("type" -> "java.util.Collections$UnmodifiableSet") ~ ("items" -> graph.nodes.keys.flatMap(toJObject)))
  }
}

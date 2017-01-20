package de.uni_luebeck.isp.tessla

import org.json4s.JsonDSL._
import org.json4s._

import scala.collection.mutable
import scala.util.Try

object SoftwareMapper extends CompilerPass[FunctionGraph, SoftwareGraph] {

  case class UnfoldedLiteralError(function: Function) extends Fatal

  case class GenericModuleWarning(function: Function) extends Diagnostic

  override def apply(compiler: Compiler, graph: FunctionGraph) = Try {
    import graph.NodeId

    val idx: Map[NodeId, Int] = graph.nodes.keys.zipWithIndex.toMap
    val ref: Map[NodeId, JObject] = idx.mapValues(i => ("ref" -> i))
    val outs = new mutable.HashSet[Int]

    def toJObject(nodeId: NodeId): Option[JObject] = {
      val node = nodeId.node

      val moduleAndMembers: Option[(String, JObject)] = node.function match {
        case ConstantValue(_, _) => None
        /*case TypeAscription(_) => {
          compiler.diagnostic(GenericModuleWarning(node.function))
          None
        }*/
        case SimpleFunction("out",_) =>
          outs += idx(node.args(0))
          None
        case StateMachineFunction(name, signature, start, stateMap, transitionList) =>
          val refs = node.args.map(x => idx(x))
          val clock = refs.last
          val refTransitions = transitionList.map(t => (t._1,t._2.map(x => refs(x-1)),t._3))
          val jsonTransitions = seq2jvalue(refTransitions.map(t => ("current" -> t._1) ~ ("active" -> t._2) ~ ("next" -> t._3)))
          val stateList = stateMap.toList
          val firstElement = stateList.filter(x => x._1.equals(start.toString)).head
          val sortedStateList = List(firstElement) ++ stateList.filter(x => !x._1.equals(start.toString))
          val jsonStates = seq2jvalue(sortedStateList.map(s => ("name" -> s._1) ~ ("output" -> s._2)))
          Some("TesslaServer.Computation.Monitors.Monitor",
            ("operands" -> refs) ~
              ("options" ->
                ("clock" -> clock) ~
                  ("states" -> jsonStates) ~
                  ("current_state" -> start) ~
                  ("transitions" -> jsonTransitions)
              )
          )
        case SimpleFunction(_, FunctionSig(SimpleType(_), _)) =>
          compiler.diagnostic(UnfoldedLiteralError(node.function))
          None
        /**** Input/Constant functions ****/
        case SimpleFunction("literal", _) => // TODO more types
          Some("TesslaServer.Computation.Literal",
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
              ("function" -> node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)
          )
        case SimpleFunction("function_calls", _) =>
          Some("TesslaServer.Source.FunctionCalls",
            "options" ->
              ("function" -> node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)
          )
        case SimpleFunction("variable_values", _) => // TODO types
          Some("TesslaServer.Source.VariableValues",
            "options" ->
              ("variable" -> node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)
          )

        /**** Lifted ****/
        case SimpleFunction("eventAbs", _) =>
          Some("TesslaServer.Computation.Lifted.EventAbs",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("signalAbs", _) =>
          Some("TesslaServer.Computation.Lifted.SignalAbs",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("add", _) =>
          Some("TesslaServer.Computation.Lifted.Add",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("and", _) =>
          Some("TesslaServer.Computation.Lifted.And",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("div", _) =>
          Some("TesslaServer.Computation.Lifted.Div",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("eq", _) =>
          Some("TesslaServer.Computation.Lifted.Eq",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("geq", _) =>
          Some("TesslaServer.Computation.Lifted.Geq",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("gt", _) =>
          Some("TesslaServer.Computation.Lifted.Gt",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("implies", _) =>
          Some("TesslaServer.Computation.Lifted.Implies",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("leq", _) =>
          Some("TesslaServer.Computation.Lifted.Leq",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("lt", _) =>
          Some("TesslaServer.Computation.Lifted.Lt",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("max", _) =>
          Some("TesslaServer.Computation.Lifted.Max",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("min", _) =>
          Some("TesslaServer.Computation.Lifted.Min",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("mul", _) =>
          Some("TesslaServer.Computation.Lifted.Mul",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("neg", _) =>
          Some("TesslaServer.Computation.Lifted.Neg",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("eventNot", _) =>
          Some("TesslaServer.Computation.Lifted.EventNot",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("signalNot", _) =>
          Some("TesslaServer.Computation.Lifted.SignalNot",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("or", _) =>
          Some("TesslaServer.Computation.Lifted.Or",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("sub", _) =>
          Some("TesslaServer.Computation.Lifted.Sub",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))

        /**** Filter ****/
        case SimpleFunction("merge", _) =>
          Some("TesslaServer.Computation.Filter.Merge",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("filter", _) =>
          Some("TesslaServer.Computation.Filter.Filter",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("ifThen", _) =>
          Some("TesslaServer.Computation.Filter.IfThen",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("ifThenElse", _) =>
          Some("TesslaServer.Computation.Filter.IfThenElse",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1)), idx(node.args(2))))
        case SimpleFunction("changeOf", _) =>
          Some("TesslaServer.Computation.Filter.ChangeOf",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("sample", _) =>
          Some("TesslaServer.Computation.Filter.Sample",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("occurAll", _) =>
          Some("TesslaServer.Computation.Filter.OccurAll",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))
        case SimpleFunction("occurAny", _) =>
          Some("TesslaServer.Computation.Filter.OccurAny",
            "operands" -> Seq(idx(node.args(0)), idx(node.args(1))))

        /**** Aggregation ****/
        case SimpleFunction("signalMaximum", _) =>
          Some("TesslaServer.Computation.Aggregation.SignalMaximum",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("eventMaximum", _) =>
          Some("TesslaServer.Computation.Aggregation.EventMaximum",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" -> ("default" -> Integer.parseInt(
              node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
            ))
          )
        case SimpleFunction("signalMinimum", _) =>
          Some("TesslaServer.Computation.Aggregation.SignalMinimum",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("eventMinimum", _) =>
          Some("TesslaServer.Computation.Aggregation.EventMinimum",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" -> ("default" -> Integer.parseInt(
              node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
              ))
          )
        case SimpleFunction("sum", _) =>
          Some("TesslaServer.Computation.Aggregation.Sum",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("eventCount", _) =>
          Some("TesslaServer.Computation.Aggregation.EventCount",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("mrv", _) => // TODO more Types
          Some("TesslaServer.Computation.Aggregation.Mrv",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" -> ("default" -> Integer.parseInt(
              node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
              ))
          )
        case SimpleFunction("sma", _) =>
          Some("TesslaServer.Computation.Aggregation.Sma",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" -> ("count" -> Integer.parseInt(
              node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString)
              ))
          )

        /**** Timing ****/
        case SimpleFunction("timestamps", _) =>
          Some("TesslaServer.Computation.Timing.Timestamps",
            "operands" -> Seq(idx(node.args(0))))
        case SimpleFunction("delayEventByTime", _) =>
          Some("TesslaServer.Computation.Timing.DelayEventByTime",
            ("operands" -> Seq(idx(node.args(0))))
            ~ ("options" ->
                ("amount" -> Integer.parseInt(node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString))
              )
          )
        case SimpleFunction("delayEventByCount", _) =>
          Some("TesslaServer.Computation.Timing.DelayEventByCount",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" ->
              ("count" -> Integer.parseInt(node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString))
              )
          )
        case SimpleFunction("delaySignalByTime", _) => //TODO more types
          Some("TesslaServer.Computation.Timing.DelaySignalByTime",
            ("operands" -> Seq(idx(node.args(0))))
              ~ ("options" ->
                (
                  ("amount" -> Integer.parseInt(node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString))
                  ~ ("default" -> Integer.parseInt(node.args(2).node.function.asInstanceOf[ConstantValue[_]].value.toString))
                )
              )
          )
        /*case SimpleFunction("within", _) => // TODO switch int to time
          Some("TesslaServer.Computation.Timing.Within",
            ("operands" -> Seq(ref(node.args(2))))
            ~ ("before" -> Integer.parseInt(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString))
            ~ ("after" -> Integer.parseInt(node.args(1).node.function.asInstanceOf[ConstantValue[_]].value.toString))
          )*/
        case SimpleFunction("inPast", _) => // TODO switch int to time
          Some("TesslaServer.Computation.Timing.InPast",
            ("operands" -> Seq(idx(node.args(1))))
            ~ ("options" ->
                ("amount" -> Integer.parseInt(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)))
          )
        /*case SimpleFunction("inFuture", _) => // TODO switch int to time
          Some("TesslaServer.Computation.Timing.InFuture",
            ("operands" -> Seq(ref(node.args(1))))
              ~ ("options" ->
              ("amount" -> Integer.parseInt(node.args(0).node.function.asInstanceOf[ConstantValue[_]].value.toString)))
          )*/
        /**** Error ****/
        case SimpleFunction(n, _) => {
          compiler.diagnostic(GenericModuleWarning(node.function))
          Some("GenericModule",
            ("operands" ->
              node.args.map { nodeId => nodeId.node.function match {
                case ConstantValue(_, value) => JString(value.toString)
                case _ => ref(nodeId)
              }
              }) ~ ("name" -> n))
        }
      }

      moduleAndMembers.map {
        case (typeString, members) =>
          var obj = ((("id" -> idx(nodeId)) ~ ("nodetype" -> typeString)) ~ members)
          node.id.name.foreach(name => obj = obj ~ ("name" -> name))
          obj
      }
    }

    def addOut(obj: JObject) = {
      if(outs(obj.values("id").toString.toInt)) obj ~ ("out" -> true)
      else obj
    }

    SoftwareGraph(("type" -> "java.util.Collections$UnmodifiableSet") ~ ("items" -> graph.nodes.keys.flatMap(toJObject).map(addOut)))
  }
}

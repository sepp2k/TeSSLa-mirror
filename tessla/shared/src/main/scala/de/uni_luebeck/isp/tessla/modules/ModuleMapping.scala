package de.uni_luebeck.isp.tessla.modules

import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.ASTGraph
import de.uni_luebeck.isp.tessla.ASTGraph._
import scala.collection.mutable
import scala.collection.generic
import de.uni_luebeck.isp.tessla.Compiler
import de.uni_luebeck.isp.tessla.Compiler._
import scala.util.{ Failure, Try }

/**
 * @author Normann Decker <decker@isp.uni-luebeck.de>
 */
object ModuleMapping extends Compiler.Pass {

  override def applyPass(compiler: Compiler, state: State): Try[State] = {
    val graph = state match {
      case Graph(graph) => graph
      case _            => return Failure[Compiler.State](UnexpectedCompilerState)
    }

    Try {
      ModuleList(ast2modules(graph))
    }
  }

  def ast2modules(astGraph: ASTGraph): List[Module] = {

    case class ToBeDereferenced(nodeId: NodeId) extends Module {
      override def map(f: Module => Module) = this
      val outputWidth: Int = -1
      val typeString: String = "toBeDereferenced"
    }

    val ASTGraph(nodes, roots, _) = astGraph

    val modules = nodes.clone().flatMap {
      case (nodeID, graphTerm) =>
        graphTerm match {
          case App(IntegralConstant(value), args, nargs, typ) => None
          case App(StringConstant(value), args, nargs, typ)   => None
          case App(UnresolvedFunction("if"), args: List[NodeId], nargs, typ) => {
            // assert args.length = 3 or args.length=2
            val control = ToBeDereferenced(args(0))
            val trueNode = ToBeDereferenced(args(1))
            if (args.length >= 2) {
              val falseNode = ToBeDereferenced(args(2))
              Some(nodeID -> IfThenElseNode(control, trueNode, falseNode))
            } else {
              Some(nodeID -> IfThenNode(control, trueNode))
            }
          }
          case App(UnresolvedFunction("constant"), args, nargs, typ) => {
            nodes(args(0)) match {
              case App(IntegralConstant(value), args, nargs, typ) => Some(nodeID -> IntegerConstantNode(value))
              case App(StringConstant(value), args, nargs, typ) => Some(nodeID -> StringConstantNode(value))
              // The type checker should make sure that this list is exhaustive!
              // In any case, we return the term as string by default which 
              // should make the problem evident in case this assumption fails.
              case term => Some(nodeID -> StringConstantNode(term.toString()))
            }
          }

          case App(UnresolvedFunction("add"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> AddNode(first, second))
          }
          case App(UnresolvedFunction("sub"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> SubNode(first, second))
          }
          case App(UnresolvedFunction("multiply"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> MultiplyNode(first, second))
          }
          case App(UnresolvedFunction("shift"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> ShiftNode(first, second))
          }
          case App(UnresolvedFunction("geq"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> GeqNode(first, second))
          }
          case App(UnresolvedFunction("and"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> AndNode(first, second))
          }
          case App(UnresolvedFunction("or"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> OrNode(first, second))
          }
          case App(UnresolvedFunction("not"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            Some(nodeID -> NotNode(first))
          }
          case App(UnresolvedFunction("implies"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> ImpliesNode(first, second))
          }
          case App(UnresolvedFunction("lessthan"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            Some(nodeID -> LessThanNode(first, second))
          }
          case App(UnresolvedFunction(name), args, nargs, typ) =>
            val inputModuleStringsApplication = Set("ApplicationMessageID",
              "ApplicationMessageTSDef",
              "ApplicationMessageValid",
              "ApplicationMessageValue")

            val inputModuleStringsBranch = Set("BranchToMessageAddress",
              "BranchToMessageValid")

            val inputModuleStringsData = Set("DataMessageAddress",
              "DataMessageSize",
              "DataMessageValid",
              "DataMessageValue")

            if (inputModuleStringsApplication.contains(name)) {
              Some(nodeID -> InputNode("applicationMessage." + name))
            } else if (inputModuleStringsBranch.contains(name)) {
              Some(nodeID -> InputNode("branchToMessage." + name))
            } else if (inputModuleStringsData.contains(name)) {
              Some(nodeID -> InputNode("dataMessage." + name))
            } else {
              Some(nodeID -> GenericModule(name, args.map { nodeId => ToBeDereferenced(nodeId) }))
            }
          case Monitor(salt, args, nargs, typ) =>
            Some(nodeID -> MonitorNode(salt, args.map(nodeId => ToBeDereferenced(nodeId))))
          case _ => Some(nodeID -> GenericModule())
        }
    }

    var mutModuleMap = mutable.Map[NodeId, Module]()
    modules.foreach(p => mutModuleMap += p)

    def deref(module: Module, map: mutable.Map[NodeId, Module]): Module = module match {
      case ToBeDereferenced(id) => {
        val derefModule = deref(map(id), map)
        map.update(id, derefModule)
        derefModule
      }
      case _ => module.map(deref(_, map))
    }

    mutModuleMap.map {
      case (nodeId, module) =>
        deref(module, mutModuleMap)
    }.toList ++ astGraph.outputs.map(name => OutputNode(mutModuleMap(astGraph.roots(DefRoot(name)))))
  }
}
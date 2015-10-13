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

    val ASTGraph(nodes, roots) = astGraph

    val modules = nodes.clone().mapValues {
      graphTerm =>
        graphTerm match {
          case App(IntegralConstant(value), args, nargs, typ) => IntegerConstantNode(value)
          case App(StringConstant(value), args, nargs, typ) => StringConstantNode(value)
          case App(UnresolvedFunction("if"), args: List[NodeId], nargs, typ) => {
            // assert args.length = 3
            val control = ToBeDereferenced(args(0))
            val trueNode = ToBeDereferenced(args(1))
            val falseNode = ToBeDereferenced(args(2))
            IfThenElseNode(control, trueNode, falseNode)
          }
          case App(UnresolvedFunction("constant"), args, nargs, typ) => ToBeDereferenced(args(0))
          case App(UnresolvedFunction("add"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            AddNode(first, second)
          }
          case App(UnresolvedFunction("sub"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            SubNode(first, second)
          }
          case App(UnresolvedFunction("multiply"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            MultiplyNode(first, second)
          }
          case App(UnresolvedFunction("shift"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            ShiftNode(first, second)
          }
          case App(UnresolvedFunction("geq"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            GeqNode(first, second)
          }
          case App(UnresolvedFunction("and"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            AndNode(first, second)
          }
          case App(UnresolvedFunction("or"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            OrNode(first, second)
          }
          case App(UnresolvedFunction("not"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            NotNode(first)
          }
          case App(UnresolvedFunction("implies"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            ImpliesNode(first, second)
          }
          case App(UnresolvedFunction("lessthan"), args: List[NodeId], nargs, typ) => {
            val first = ToBeDereferenced(args(0))
            val second = ToBeDereferenced(args(1))
            LessThanNode(first, second)
          }
          case App(UnresolvedFunction("monitor"), args, nargs, typ) => MonitorNode(ToBeDereferenced(args.head), args.tail.map ( nodeId => ToBeDereferenced(nodeId)))
          case App(UnresolvedFunction(name), args, nargs, typ) => GenericModule(name, args.map { nodeId => ToBeDereferenced(nodeId) })
          case _ => GenericModule()
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
    }.toList

  }

}
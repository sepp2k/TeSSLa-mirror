package de.uni_luebeck.isp.tessla.modules2

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
          case App(IntegralConstant(value), args, nargs, typ) => ConstantNode(value)
          case App(UnresolvedFunction("if"), args: List[NodeId], nargs, typ) => {
            // assert args.length = 3
            val control = ToBeDereferenced(args(0))
            val trueNode = ToBeDereferenced(args(1))
            val falseNode = ToBeDereferenced(args(2))
            IfThenElseNode(control, trueNode, falseNode)
          }
          case App(UnresolvedFunction("constant"), args, nargs, typ) => ToBeDereferenced(args(0))
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
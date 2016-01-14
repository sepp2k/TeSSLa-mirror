package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST.{UnresolvedFunction, App, UnresolvedTerm}
import de.uni_luebeck.isp.tessla.ASTGraph.DefRoot
import de.uni_luebeck.isp.tessla.Compiler.{Graph, UnexpectedCompilerState, State}

import scala.util.{Success, Failure, Try}

object ResolveLocalRefs extends Compiler.Pass {
  override def applyPass(compiler: Compiler, state: State): Try[State] = {
    val graph = state match {
      case Graph(graph) => graph
      case _ => return Failure[Compiler.State](UnexpectedCompilerState)
    }

    for (id <- graph.nodeIds) {
      graph.node(id) match {
        case UnresolvedTerm(name, _) if graph.roots.contains(DefRoot(name)) =>
          graph.replaceNodeRefs(id, graph.roots(DefRoot(name)))
        case UnresolvedTerm(name, typ) =>
          graph.updateNode(id, App(UnresolvedFunction(name), typ=typ))
        case _ =>
      }
    }

    Success(Graph(graph))
  }
}

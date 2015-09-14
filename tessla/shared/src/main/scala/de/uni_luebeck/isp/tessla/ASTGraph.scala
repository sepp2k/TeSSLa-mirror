package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._
import scala.collection.mutable.Map

import ASTGraph._
import scala.collection.mutable.HashMap

object ASTGraph {
  sealed case class NodeId(id: Int)
  type GraphTerm = Term[NodeId]
  sealed abstract class Root
  case class DefRoot(name: String) extends Root
}

class ASTGraph(var nodes: Map[NodeId, GraphTerm] = new HashMap, var roots: Map[Root, NodeId] = new HashMap) {
  private var nextNodeId =
    (nodes.keys ++ Seq(NodeId(0))) maxBy {case NodeId(x) => x}
    
  def freeNodeId(): NodeId = {
    nextNodeId = nextNodeId match {case NodeId(x) => NodeId(x + 1)}
    while (nodes.contains(nextNodeId)) {
      nextNodeId = nextNodeId match {case NodeId(x) => NodeId(x + 1)}
    }
    return nextNodeId
  }
  
  def loadSpec(spec: Spec) {
    for (stmnt <- spec.statements) {
      stmnt match {
        case Def(name, defn) =>
          roots.put(DefRoot(name), loadTerm(defn))
        case MacroDef(_, _, _) =>
          throw new RuntimeException("Macro definitions must be expanded before")
        case Out(name) =>
          // TODO how to handle these?
      }
    }
  }
  
  def loadTerm(term: TreeTerm): NodeId = {
    val termId = freeNodeId()
    val graphTerm: GraphTerm = term match {
      case TreeTerm(t @ UnresolvedTerm(_, _)) => t
      case TreeTerm(t @ Const(_, _)) => t
      case TreeTerm(t @ Ref(_, _)) => t
      case TreeTerm(App(fn, args, nargs, typ)) =>
        val gargs = args map {a => loadTerm(a)}
        val gnargs = nargs map {case NamedArg(name, a) => NamedArg(name, loadTerm(a))}
        App(fn, gargs, gnargs, typ)
      case TreeTerm(TypeAscr(t, typ)) =>
        TypeAscr(loadTerm(t), typ)
    }
    graphTerm.loc = term.term.loc
    nodes.put(termId, graphTerm)
    return termId
  }
  
}
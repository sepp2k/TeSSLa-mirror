package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.Compiler.{UnexpectedCompilerState, Tree, State}
import scala.StringBuilder
import scala.collection.mutable

import ASTGraph._
import scala.util.{Success, Failure, Try}

object ASTGraph extends Compiler.Pass {
  final case class NodeId(id: Int) {
    override def toString = "#" + id
  }
  type GraphTerm = Term[NodeId]
  sealed abstract class Root
  case class DefRoot(name: String) extends Root

  override def applyPass(compiler: Compiler, state: State): Try[State] = {
    state match {
      case Tree(spec) => {
        val g = new ASTGraph
        g.loadSpec(spec)
        Success(Compiler.Graph(g))
      }
      case _ => Failure[Compiler.State](UnexpectedCompilerState)
    }
  }
}

case class ASTGraph(
  private var nodes: mutable.Map[NodeId, GraphTerm] = new mutable.HashMap,
  var roots: mutable.Map[Root, NodeId] = new mutable.HashMap) extends mutable.Cloneable[ASTGraph] {
  private var nextNodeId =
    (nodes.keys ++ Seq(NodeId(0))) maxBy {case NodeId(x) => x}

  var uses: mutable.Map[NodeId, mutable.Set[NodeId]] = null

  recomputeUses()

  def recomputeUses() {
    uses = new mutable.HashMap
    for ((nodeId, term) <- nodes) {
      term foreach (otherId => useSet(otherId) += nodeId)
    }
  }

  def useSet(node: NodeId): mutable.Set[NodeId] = {
    uses.getOrElseUpdate(node, new mutable.HashSet)
  }

  def updateNode(node: NodeId, value: GraphTerm) {
    updateNode(node, Some(value))
  }

  def deleteNode(node: NodeId): Unit = {
    updateNode(node, None)
  }

  def updateNode(node: NodeId, value: Option[GraphTerm]) {
    nodes.get(node).foreach(_.foreach(id => {uses(id) -= node}))
    value match {
      case None => nodes -= node
      case Some(v) =>
        nodes(node) = v
        v.foreach(id => useSet(id) += node)
    }
  }

  def node(node: NodeId) = nodes(node)

  def nodeIds: Seq[NodeId] = nodes.keys.toSeq
    
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
    val graphTerm: GraphTerm = term.term map loadTerm
    graphTerm.loc = term.term.loc
    updateNode(termId, graphTerm)
    return termId
  }

  def replaceNodeRefs(replace: NodeId, using: NodeId): Unit = {
    deleteNode(replace)
    val uses = useSet(replace).clone
    for (use <- uses) {
      updateNode(use, nodes(use) map {x => if (x == replace) using else x})
    }
  }

  override def clone(): ASTGraph = {
    new ASTGraph(nodes = nodes.clone, roots = roots.clone)
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder ++= "digraph {\n"

    for (id <- nodeIds) {
      // TODO escape string
      val idNr = id.id
      val label = id + ": " + node(id).toString
      builder ++= s"""  n$idNr [label = "$label" shape = box];\n"""
      for (targetId <- node(id)) {
        val targetIdNr = targetId.id
        builder ++= s"""  n$idNr -> n$targetIdNr\n"""
      }
    }

    builder ++= "}"
    builder.toString
  }
}
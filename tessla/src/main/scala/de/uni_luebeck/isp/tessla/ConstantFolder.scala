package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import scala.util.Try

object ConstantFolder extends CompilerPass[FunctionGraph, FunctionGraph] {

  case class ConstantFoldingError(function: Semantics, args: Seq[Any]) extends Fatal

  override def apply(compiler: Compiler, input: FunctionGraph): Try[FunctionGraph] = Try {
    import input.{Node, NodeId, nodes}

    val processed: mutable.Map[NodeId, Boolean] = mutable.Map() ++ nodes.keys.map(_ -> false)

    def foldConstants(nodeId: NodeId): Unit = if (!processed(nodeId)) {
      processed.update(nodeId, true)
      val n = nodeId.node
      n.args.foreach(foldConstants)
      n.function match {
        case f: Semantics => {
          if (n.args.forall(_.node.function.isInstanceOf[ConstantValue[_]])) {
            val argValues = n.args.map(_.node.function.asInstanceOf[ConstantValue[_]].value)
            try {
              val returnValue = f(argValues)
              val returnType = n.function.signature.ret
              nodes.update(nodeId, Node(n.id, ConstantValue(returnType, returnValue), Seq()))
            } catch {
              case e: ClassCastException => compiler.diagnostic(ConstantFoldingError(f, argValues))
            }
          }
        }
        case _ =>
      }
    }

    nodes.keys.foreach(foldConstants)
    //remove constant nodes that are not referenced anymore
    val referencedIds: Set[NodeId] = nodes.keys.foldLeft(Set[NodeId]()){case (m,nodeId) => m ++ nodeId.node.args.toSet}
    nodes.retain{case (id,node:Node) => (!node.function.isInstanceOf[ConstantValue[_]]) || referencedIds.contains(id)}

    input
  }
}

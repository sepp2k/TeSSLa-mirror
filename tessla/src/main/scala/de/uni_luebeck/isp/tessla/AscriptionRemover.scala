package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import scala.util.Try

object AscriptionRemover extends CompilerPass[FunctionGraph, FunctionGraph] {
  override def apply(compiler: Compiler, input: FunctionGraph): Try[FunctionGraph] = Try {
    import input.{Node, NodeId, nodes}

    val processed: mutable.Map[NodeId, Boolean] = mutable.Map() ++ nodes.keys.map(_ -> false)

    def bypassTypeAscr(nodeId: NodeId): Unit = if (!processed(nodeId)) {
      processed.update(nodeId, true)
      val newArgs:Seq[NodeId] = nodeId.node.args.map{argId => {
        bypassTypeAscr(argId)
        argId.node.function match {
          case TypeAscription(_) =>
            val arg = argId.node.args.head
            arg.name = argId.name
            arg
          case _ => argId
        }
      }}
      nodes.update(nodeId, nodeId.node.copy(args=newArgs))
    }

    nodes.keys.foreach(bypassTypeAscr)

    //TypeAscriptions are now not referenced anymore and can thus be removed
    nodes.retain{case (id,node) => !id.node.function.isInstanceOf[TypeAscription[_]]}

    input
  }
}

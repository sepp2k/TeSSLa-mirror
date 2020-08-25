package de.uni_luebeck.isp.tessla.analyses

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._

object NodeCounter {
  def nodeCount(spec: Core.Specification): Int = {
    spec.out.map(os => nodeCount(spec.definitions, os._1, Set())).sum
  }

  def nodeCount(
    streams: Map[Core.Identifier, Core.Expression],
    stream: Core.ExpressionArg,
    visited: Set[Core.Identifier]
  ): Int = stream match {
    case ref: Core.ExpressionRef if visited.contains(ref.id) => 0
    case ref: Core.ExpressionRef                             => nodeCount(streams, streams(ref.id), visited + ref.id)
    case exp: Core.Expression =>
      extract(exp).map(nodeCount(streams, _, visited)).sum + 1
  }

  private def extract(e: Core.Expression) = e match {
    case ApplicationExpression(TypeApplicationExpression(e: ExternExpression, _, _), args, _) => args
    case ApplicationExpression(e: ExternExpression, args, _)                                  => args
    case _                                                                                    => Seq()
  }
}

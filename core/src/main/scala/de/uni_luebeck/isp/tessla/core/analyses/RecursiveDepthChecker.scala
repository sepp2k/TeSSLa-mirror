package de.uni_luebeck.isp.tessla.analyses

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._

import scala.collection.mutable

object RecursiveDepthChecker {

  def nestingDepth(spec: Core.Specification): Int = {
    val ins = spec.in.keySet
    val defDepths = spec.definitions.mapValues(extract).collect {
      case (id, (e: ExternExpression, args)) if e.name == "last" || e.name == "delay" =>
        nestingDepth(spec.definitions, ins, args.head, None, id, mutable.Map()).getOrElse(0)
    }

    defDepths.fold(0)(math.max)
  }

  def nestingDepth(
    streams: Map[Core.Identifier, Core.Expression],
    ins: Set[Core.Identifier],
    stream: Core.ExpressionArg,
    id: Option[Core.Identifier],
    origin: Core.Identifier,
    memoized: mutable.Map[Core.Identifier, Option[Int]]
  ): Option[Int] = {
    def visitChild(child: Core.ExpressionArg): Option[Int] = {
      nestingDepth(streams, ins, child, None, origin, memoized)
    }

    stream match {
      case ref: Core.ExpressionRef if ins.contains(ref.id)      => None
      case ref: Core.ExpressionRef if memoized.contains(ref.id) => memoized(ref.id)
      case ref: Core.ExpressionRef                              => nestingDepth(streams, ins, streams(ref.id), Some(ref.id), origin, memoized)
      case exp: Core.Expression =>
        val depth: Option[Int] =
          if (id.contains(origin)) Some(0)
          else {
            extract(exp) match {
              case (e: ExternExpression, args) if e.name == "last" || e.name == "delay" =>
                visitChild(args(1))
              case (e, args) =>
                args.map(visitChild).maxOption.flatten
            }
          }
        val next = depth.map(_ + 1)
        id.foreach(memoized(_) = next)
        next
    }
  }

  private def extract(e: Core.Expression) = e match {
    case ApplicationExpression(TypeApplicationExpression(e: ExternExpression, _, _), args, _) => (e, args)
    case ApplicationExpression(e: ExternExpression, args, _)                                  => (e, args)
    case _                                                                                    => (e, Seq())
  }
}

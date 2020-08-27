package de.uni_luebeck.isp.tessla.core.analyses

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core

import scala.collection.mutable

object DepthChecker {

  def nestingDepth(spec: Core.Specification): Int = {
    val memTable: mutable.Map[Core.Identifier, Int] = mutable.Map()
    val ins = spec.in.keySet
    val defDepths = spec.definitions.values
      .map(extract)
      .collect {
        case (e: ExternExpression, args) if e.name == "last" || e.name == "delay" => args.head
      }
      .map(nestingDepth(spec.definitions, ins, _, None, memTable))

    val outDepths = spec.out.map(_._1).map(nestingDepth(spec.definitions, ins, _, None, memTable))

    (outDepths ++ defDepths).fold(0)(math.max)
  }

  def nestingDepth(
    streams: Map[Core.Identifier, Core.Expression],
    ins: Set[Core.Identifier],
    stream: Core.ExpressionArg,
    id: Option[Core.Identifier],
    memoized: mutable.Map[Core.Identifier, Int]
  ): Int = {
    def visitChild(child: Core.ExpressionArg): Int = {
      nestingDepth(streams, ins, child, None, memoized)
    }

    stream match {
      case ref: Core.ExpressionRef if ins.contains(ref.id)      => 0
      case ref: Core.ExpressionRef if memoized.contains(ref.id) => memoized(ref.id)
      case ref: Core.ExpressionRef                              => nestingDepth(streams, ins, streams(ref.id), Some(ref.id), memoized)
      case exp: Core.Expression =>
        val childDepth = extract(exp) match {
          case (e: ExternExpression, args) if e.name == "last" || e.name == "delay" =>
            visitChild(args(1))
          case (e, args) =>
            args.map(visitChild).maxOption.getOrElse(0)
        }
        id.foreach(memoized(_) = 1 + childDepth)
        1 + childDepth
    }
  }

  private def extract(e: Core.Expression) = e match {
    case ApplicationExpression(TypeApplicationExpression(e: ExternExpression, _, _), args, _) => (e, args)
    case ApplicationExpression(e: ExternExpression, args, _)                                  => (e, args)
    case _                                                                                    => (e, Seq())
  }
}

/*package de.uni_luebeck.isp.tessla.analyses

import de.uni_luebeck.isp.tessla.TesslaCore

import scala.collection.mutable

object DepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    val memTable: mutable.Map[TesslaCore.Identifier, Int] = mutable.Map()
    val streams = spec.streams.map(s => s.id -> s.expression).toMap
    (spec.outStreams.map { os => nestingDepth(streams, os.stream, memTable) } ++ spec.streams.flatMap { s =>
      s.expression match {
        case l: TesslaCore.Last =>
          Some(nestingDepth(streams, l.values, memTable))
        case d: TesslaCore.Delay =>
          Some(nestingDepth(streams, d.delays, memTable))
        case _ =>
          None
      }
    }).fold(0)(math.max)
  }

  def nestingDepth(streams: Map[TesslaCore.Identifier, TesslaCore.Expression], stream: TesslaCore.StreamRef, memoized: mutable.Map[TesslaCore.Identifier, Int]): Int = {
    def visitChild(child: TesslaCore.StreamRef): Int = {
      nestingDepth(streams, child, memoized)
    }

    stream match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => 0
      case s: TesslaCore.Stream =>
        if (memoized.contains(s.id)) return memoized(s.id)

        val childDepth = streams(s.id) match {
          case l: TesslaCore.Last =>
            visitChild(l.clock)
          case d: TesslaCore.Delay =>
            visitChild(d.resets)
          case c: TesslaCore.CustomBuiltInCall =>
            c.streamArgs.map(visitChild).max
        }
        memoized(s.id) = 1 + childDepth
        1 + childDepth
    }
  }
}
*/
package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object DepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    val memTable: mutable.Map[TesslaCore.Identifier, Int] = mutable.Map()
    val streams = spec.streams.toMap
    (spec.outStreams.map {
      case (_, stream, _) =>
        nestingDepth(streams, stream, memTable)
    } ++ spec.streams.collect {
      case (_, l: TesslaCore.Last) =>
        nestingDepth(streams, l.values, memTable)
      case (_, dl: TesslaCore.DelayedLast) =>
        Math.max(
          nestingDepth(streams, dl.values, memTable),
          nestingDepth(streams, dl.delays, memTable))
      case (_, d: TesslaCore.Delay) =>
        nestingDepth(streams, d.delays, memTable)
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
          case l: TesslaCore.SignalLift =>
            l.args.map(visitChild).max
          case l: TesslaCore.Lift =>
            l.args.map(visitChild).max
          case t: TesslaCore.Time =>
            visitChild(t.stream)
          case d: TesslaCore.Default =>
            visitChild(d.stream)
          case d: TesslaCore.DefaultFrom =>
            Math.max(visitChild(d.valueStream), visitChild(d.defaultStream))
          case l: TesslaCore.Last =>
            visitChild(l.clock)
          case m: TesslaCore.Merge =>
            Math.max(visitChild(m.stream1), visitChild(m.stream2))
          case c: TesslaCore.Const =>
            visitChild(c.stream)
          case _: TesslaCore.DelayedLast =>
            0
          case d: TesslaCore.Delay =>
            visitChild(d.resets)
          case c: TesslaCore.StdLibCount =>
            visitChild(c.stream)
        }
        memoized(s.id) = 1 + childDepth
        1 + childDepth
    }
  }
}

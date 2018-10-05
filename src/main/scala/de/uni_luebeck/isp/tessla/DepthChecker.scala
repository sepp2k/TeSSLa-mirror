package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object DepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    val memTable: mutable.Map[TesslaCore.Identifier, Int] = mutable.Map()
    val streams = spec.streams.toMap
    (spec.outStreams.map {
      case (_, stream) =>
        nestingDepth(streams, stream, memTable)
    } ++ spec.streams.collect {
      case (_, TesslaCore.StreamDefinition(l: TesslaCore.Last, _)) =>
        nestingDepth(streams, l.values, memTable)
      case (_, TesslaCore.StreamDefinition(dl: TesslaCore.DelayedLast, _)) =>
        Math.max(
          nestingDepth(streams, dl.values, memTable),
          nestingDepth(streams, dl.delays, memTable))
    }).fold(0)(math.max)
  }

  def nestingDepth(streams: Map[TesslaCore.Identifier, TesslaCore.StreamDefinition], stream: TesslaCore.StreamRef, memoized: mutable.Map[TesslaCore.Identifier, Int]): Int = {
    def visitChild(child: TesslaCore.StreamRef): Int = {
      nestingDepth(streams, child, memoized)
    }

    stream match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => 0
      case s: TesslaCore.Stream =>
        if (memoized.contains(s.id)) return memoized(s.id)

        val childDepth = streams(s.id).expression match {
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
          case dl: TesslaCore.DelayedLast =>
            0
        }
        memoized(s.id) = 1 + childDepth
        1 + childDepth
    }
  }
}

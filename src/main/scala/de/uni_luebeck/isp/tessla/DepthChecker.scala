package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object DepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    val memTable: mutable.Map[String, Int] = mutable.Map()
    (spec.outStreams.map {
      case (_, stream) =>
        nestingDepth(spec.streams, stream, memTable)
    } ++ spec.streams.collect {
      case (name, l: TesslaCore.Last) =>
        nestingDepth(spec.streams, l.values, memTable)
      case (name, dl: TesslaCore.DelayedLast) =>
        Math.max(
          nestingDepth(spec.streams, dl.values, memTable),
          nestingDepth(spec.streams, dl.delays, memTable))
    }).fold(0)(math.max)
  }

  def nestingDepth(streams: Map[String, TesslaCore.Expression], stream: TesslaCore.StreamRef, memoized: mutable.Map[String, Int]): Int = {
    if (memoized.contains(stream.name)) return memoized(stream.name)

    def visitChild(child: TesslaCore.StreamRef): Int = {
      nestingDepth(streams, child, memoized)
    }

    stream match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => 0
      case s: TesslaCore.Stream =>
        val childDepth = streams(s.name) match {
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
          case dl: TesslaCore.DelayedLast =>
            0
        }
        memoized(s.name) = 1 + childDepth
        1 + childDepth
    }
  }
}

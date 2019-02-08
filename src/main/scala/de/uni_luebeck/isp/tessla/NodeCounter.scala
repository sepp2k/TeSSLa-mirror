package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object NodeCounter {
  def nodeCount(spec: TesslaCore.Specification): Int = {
    val visited: mutable.Set[TesslaCore.Identifier] = mutable.Set()
    val streams = spec.streams.toMap
    spec.outStreams.map {
      case (_, stream, _) =>
        nodeCount(streams, stream, visited)
    }.sum
  }

  def nodeCount(streams: Map[TesslaCore.Identifier, TesslaCore.Expression], stream: TesslaCore.StreamRef, visited: mutable.Set[TesslaCore.Identifier]): Int = {
    def visitChild(child: TesslaCore.StreamRef): Int = {
      nodeCount(streams, child, visited)
    }

    stream match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => 0
      case s: TesslaCore.Stream =>
        if (visited.contains(s.id)) return 0

        visited += s.id

        val childDepth = streams(s.id) match {
          case l: TesslaCore.SignalLift =>
            l.args.map(visitChild).sum
          case l: TesslaCore.Lift =>
            l.args.map(visitChild).sum
          case t: TesslaCore.Time =>
            visitChild(t.stream)
          case d: TesslaCore.Default =>
            visitChild(d.stream)
          case d: TesslaCore.DefaultFrom =>
            visitChild(d.valueStream) + visitChild(d.defaultStream)
          case l: TesslaCore.Last =>
            visitChild(l.clock) + visitChild(l.values)
          case m: TesslaCore.Merge =>
            visitChild(m.stream1) + visitChild(m.stream2)
          case c: TesslaCore.Const =>
            visitChild(c.stream)
          case dl: TesslaCore.DelayedLast =>
            visitChild(dl.delays) + visitChild(dl.values)
          case d: TesslaCore.Delay =>
            visitChild(d.resets) + visitChild(d.delays)
          case c: TesslaCore.StdLibCount =>
            visitChild(c.stream)
        }
        1 + childDepth
    }
  }

}

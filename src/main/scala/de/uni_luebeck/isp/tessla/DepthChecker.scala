package de.uni_luebeck.isp.tessla

object DepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    spec.outStreams.map {
      case (_, stream) =>
        nestingDepth(spec.streams, stream, Set())
    }.max
  }

  def nestingDepth(streams: Map[String, TesslaCore.Expression], stream: TesslaCore.StreamRef, visited: Set[String]): Int = {
    if (visited.contains(stream.name)) return 0

    def visitChild(child: TesslaCore.StreamRef): Int = {
      nestingDepth(streams, child, visited + stream.name)
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
            Math.max(visitChild(l.values), visitChild(l.clock))
          case dl: TesslaCore.DelayedLast =>
            Math.max(visitChild(dl.values), visitChild(dl.delays))
        }
        1 + childDepth
    }
  }
}

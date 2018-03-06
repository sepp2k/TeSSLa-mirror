package de.uni_luebeck.isp.tessla

object RecursiveDepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    spec.streams.collect {
      case (name, l: TesslaCore.Last) =>
        nestingDepth(spec.streams, l.values, Set(name))
      case (name, dl: TesslaCore.DelayedLast) =>
        Math.max(
          nestingDepth(spec.streams, dl.values, Set(name)),
          nestingDepth(spec.streams, dl.delays, Set(name)))
    }.fold(0)(math.max)
  }

  def nestingDepth(streams: Map[String, TesslaCore.Expression], stream: TesslaCore.StreamRef, visited: Set[String]): Int = {
    stream match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => 0
      case s: TesslaCore.Stream =>
        if (visited.contains(s.name)) return visited.size

        def visitChild(child: TesslaCore.StreamRef): Int =
          nestingDepth(streams, child, visited + s.name)

        streams(s.name) match {
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
    }
  }
}

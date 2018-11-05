package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object RecursiveDepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    val streams = spec.streams.toMap
    spec.streams.collect {
      case (id, TesslaCore.StreamDefinition(l: TesslaCore.Last, _)) =>
        nestingDepth(streams, l.values, id)
      case (id, TesslaCore.StreamDefinition(dl: TesslaCore.DelayedLast, _)) =>
        Math.max(
          nestingDepth(streams, dl.values, id),
          nestingDepth(streams, dl.delays, id))
    }.fold(0)(math.max)
  }

  def nestingDepth(streams: Map[TesslaCore.Identifier, TesslaCore.StreamDefinition], stream: TesslaCore.StreamRef, origin: TesslaCore.Identifier): Int =
    nestingDepth(streams, stream, origin, mutable.Map()).getOrElse(0)

  def inc(a: Option[Int]): Option[Int] = a.map(_ + 1)

  def nestingDepth(streams: Map[TesslaCore.Identifier, TesslaCore.StreamDefinition], stream: TesslaCore.StreamRef, origin: TesslaCore.Identifier, memoized: mutable.Map[TesslaCore.Identifier, Option[Int]]): Option[Int] = {
    stream match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => None
      case s: TesslaCore.Stream =>
        if (memoized.contains(s.id)) return memoized(s.id)

        def visitChild(child: TesslaCore.StreamRef): Option[Int] =
          nestingDepth(streams, child, origin, memoized)

        val result = if (s.id == origin) {
          Some(0)
        } else {
          streams(s.id).expression match {
            case l: TesslaCore.Lift =>
              val foo: Seq[Option[Int]] = l.args.map(visitChild)
              foo.max
            case t: TesslaCore.Time =>
              visitChild(t.stream)
            case d: TesslaCore.Default =>
              visitChild(d.stream)
            case d: TesslaCore.DefaultFrom =>
              Seq(visitChild(d.valueStream), visitChild(d.defaultStream)).max
            case l: TesslaCore.Last =>
              visitChild(l.clock)
            case dl: TesslaCore.DelayedLast =>
              None
            case m: TesslaCore.Merge =>
              Seq(visitChild(m.stream1), visitChild(m.stream2)).max
            case c: TesslaCore.Const =>
              visitChild(c.stream)
          }
        }

        memoized(s.id) = inc(result)
        inc(result)
    }
  }
}
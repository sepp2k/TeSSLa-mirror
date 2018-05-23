package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object RecursiveDepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    spec.streams.collect {
      case (name, l: TesslaCore.Last) =>
        nestingDepth(spec.streams, l.values, name)
      case (name, dl: TesslaCore.DelayedLast) =>
        Math.max(
          nestingDepth(spec.streams, dl.values, name),
          nestingDepth(spec.streams, dl.delays, name))
    }.fold(0)(math.max)
  }

  def nestingDepth(streams: Map[String, TesslaCore.Expression], stream: TesslaCore.StreamRef, origin: String): Int =
    nestingDepth(streams, stream, origin, mutable.Map()).getOrElse(0)

  def inc(a: Option[Int]): Option[Int] = a.map(_ + 1)

  def nestingDepth(streams: Map[String, TesslaCore.Expression], stream: TesslaCore.StreamRef, origin: String, memoized: mutable.Map[String, Option[Int]]): Option[Int] = {
    if (memoized.contains(stream.name)) return memoized(stream.name)

    stream match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => None
      case s: TesslaCore.Stream =>

        def visitChild(child: TesslaCore.StreamRef): Option[Int] =
          nestingDepth(streams, child, origin, memoized)

        val result = if (s.name == origin) {
          Some(0)
        } else {
          streams(s.name) match {
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
          }
        }

        memoized(s.name) = inc(result)
        inc(result)
    }
  }
}

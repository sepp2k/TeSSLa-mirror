package de.uni_luebeck.isp.tessla.analyses

import de.uni_luebeck.isp.tessla.TesslaCore

import scala.collection.mutable

object RecursiveDepthChecker {
  def nestingDepth(spec: TesslaCore.Specification): Int = {
    val streams = spec.streams.map {s => s.id -> s.expression}.toMap
    spec.streams.flatMap { s =>
      s.expression match {
        case l: TesslaCore.Last =>
          Some(nestingDepth(streams, l.values, s.id))
        case dl: TesslaCore.DelayedLast =>
          Some(Math.max(
            nestingDepth(streams, dl.values, s.id),
            nestingDepth(streams, dl.delays, s.id)))
        case d: TesslaCore.Delay =>
          Some(nestingDepth(streams, d.delays, s.id))
        case _ =>
          None
      }
    }.fold(0)(math.max)
  }

  def nestingDepth(streams: Map[TesslaCore.Identifier, TesslaCore.Expression], stream: TesslaCore.StreamRef, origin: TesslaCore.Identifier): Int =
    nestingDepth(streams, stream, origin, mutable.Map()).getOrElse(0)

  def inc(a: Option[Int]): Option[Int] = a.map(_ + 1)

  def nestingDepth(streams: Map[TesslaCore.Identifier, TesslaCore.Expression], stream: TesslaCore.StreamRef, origin: TesslaCore.Identifier, memoized: mutable.Map[TesslaCore.Identifier, Option[Int]]): Option[Int] = {
    stream match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => None
      case s: TesslaCore.Stream =>
        if (memoized.contains(s.id)) return memoized(s.id)

        def visitChild(child: TesslaCore.StreamRef): Option[Int] =
          nestingDepth(streams, child, origin, memoized)

        val result = if (s.id == origin) {
          Some(0)
        } else {
          streams(s.id) match {
            case l: TesslaCore.SignalLift =>
              l.args.map(visitChild).max
            case l: TesslaCore.Lift =>
              l.args.map(visitChild).max
            case t: TesslaCore.Time =>
              visitChild(t.stream)
            case d: TesslaCore.Default =>
              visitChild(d.stream)
            case d: TesslaCore.DefaultFrom =>
              Seq(visitChild(d.valueStream), visitChild(d.defaultStream)).max
            case l: TesslaCore.Last =>
              visitChild(l.clock)
            case _: TesslaCore.DelayedLast =>
              None
            case d: TesslaCore.Delay =>
                visitChild(d.resets)
            case m: TesslaCore.Merge =>
              Seq(visitChild(m.stream1), visitChild(m.stream2)).max
            case f: TesslaCore.Filter =>
              Seq(visitChild(f.events), visitChild(f.condition)).max
            case c: TesslaCore.Const =>
              visitChild(c.stream)
            case c: TesslaCore.StdLibCount =>
              visitChild(c.stream)
          }
        }

        memoized(s.id) = inc(result)
        inc(result)
    }
  }
}

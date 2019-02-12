package de.uni_luebeck.isp.tessla

/**
  * Sort the definition in reverse topographical order ignoring the edges of a last's first operand and both operands of
  * a delayed last (so each stream will appear before any stream that uses it in the same timestamp) and produce an
  * error for any recursion that does not go through a last.
  *
  * The reverse topological sort will be useful for code generation strategies that put the actions within one time
  * stamp in a linear order. The errors for lastless recursions are useful because any recursion without last would
  * cause an infinite loop in the interpreter (and either an infinite loop or an error during codegen in other backends).
  */
class CycleDetection extends TranslationPhase[TesslaCore.Specification, TesslaCore.Specification] {
  override def translateSpec(spec: TesslaCore.Specification): TesslaCore.Specification = {
    val streams = spec.streams.map {s => s.id -> s}.toMap

    def resolveStream(s: TesslaCore.StreamRef): Seq[TesslaCore.StreamDescription] = s match {
      case _: TesslaCore.Nil | _: TesslaCore.InputStream => Seq()
      case stream: TesslaCore.Stream => Seq(streams(stream.id))
    }

    def requiredStreams(streamDef: TesslaCore.StreamDescription): Seq[TesslaCore.StreamDescription] = {
      streamDef.expression match {
        case c: TesslaCore.Const => resolveStream(c.stream)
        case d: TesslaCore.Default => resolveStream(d.stream)
        case d: TesslaCore.DefaultFrom => resolveStream(d.valueStream) ++ resolveStream(d.defaultStream)
        case l: TesslaCore.Last => resolveStream(l.clock) // ignore the value stream because it goes to another time
        case _: TesslaCore.DelayedLast => Seq() // ignore both operands because both are delayed
        case d: TesslaCore.Delay => resolveStream(d.resets) // ignore the delays stream which is delayed
        case m: TesslaCore.Merge => resolveStream(m.stream1) ++ resolveStream(m.stream2)
        case t: TesslaCore.Time => resolveStream(t.stream)
        case l: TesslaCore.SignalLift => l.args.flatMap(resolveStream)
        case l: TesslaCore.Lift => l.args.flatMap(resolveStream)
        case c: TesslaCore.StdLibCount => resolveStream(c.stream)
      }
    }
    ReverseTopologicalSort.sort(streams.values)(requiredStreams) match {
      case ReverseTopologicalSort.Cycles(nodesInCycles) =>
        val errors = nodesInCycles.flatMap { stream =>
            stream.id.nameOpt.map(_ => Errors.InfiniteRecursion(stream.expression.loc))
        }.toIndexedSeq
        // Add all but the last error to the error list and then throw the last.
        // We need to throw one of them, so the execution does not continue, but we also want to record the others. We
        // exclude the last from iteration, so it isn't reported twice (once in the loop and then again when throwing
        // it afterwards)
        errors.init.foreach { err =>
          error(err)
        }
        throw errors.last

      case ReverseTopologicalSort.Sorted(sortedNodes) =>
        spec.copy(streams = sortedNodes)
    }
  }
}

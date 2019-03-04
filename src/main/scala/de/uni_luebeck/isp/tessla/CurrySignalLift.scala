package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TesslaCore.{Default, Nil, StreamDescription}

class CurrySignalLift extends TranslationPhase[TesslaCore.Specification, TesslaCore.Specification] {
  override def translateSpec(spec: TesslaCore.Specification): TesslaCore.Specification = {
    val streams = spec.streams.map { stream => stream.id -> stream }.toMap

    def getConst(ref: TesslaCore.StreamRef) = ref match {
      case TesslaCore.Stream(id, loc) =>
        val exp = streams(id).expression
        exp match {
          case Default(Nil(_), value, loc) => Some(value)
          case _ => None
        }
      case _ => None
    }

    val updatedStreams = spec.streams.map {
      case TesslaCore.StreamDescription(id, expression, typ) =>
        val newExpression = expression match {
          case TesslaCore.SignalLift(op, argStreams, loc) =>
          val (newArgs, newCurry) =  argStreams.zipWithIndex.foldLeft((Seq.empty[TesslaCore.StreamRef], op.args)) { case ((newArgs, newCurry), (arg, i)) =>
              getConst(arg) match {
                case Some(value) => (newArgs, newCurry + (i -> value))
                case None => (newArgs :+ arg, newCurry)
              }
            }
            if (newArgs.isEmpty) {
              // TODO this should have been caught by the ConstantEvaluator earlier!
              expression
            } else {
              TesslaCore.SignalLift(TesslaCore.CurriedPrimitiveOperator(op.op, newCurry), newArgs, loc)
            }
          case _ => expression
        }
        StreamDescription(id, newExpression, typ)
    }

    // TODO remove default(nil, x) streams if they are now unused

    TesslaCore.Specification(updatedStreams, spec.inStreams, spec.outStreams)
  }
}

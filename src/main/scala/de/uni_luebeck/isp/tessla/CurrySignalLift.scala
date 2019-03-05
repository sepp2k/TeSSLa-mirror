package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TesslaCore.{Default, Nil, SignalLift, StreamDescription, ValueOrError}

class CurrySignalLift extends TranslationPhase[TesslaCore.Specification, TesslaCore.Specification] {
  override def translateSpec(spec: TesslaCore.Specification): TesslaCore.Specification = {
    val streams = spec.streams.map { stream => stream.id -> stream }.toMap

    def getConst(ref: TesslaCore.StreamRef): Option[ValueOrError] = ref match {
      case TesslaCore.Stream(id, loc) =>
        val exp = streams(id).expression
        exp match {
          case Default(Nil(_), value, loc) => Some(value)
          case SignalLift(op, args, loc) =>
            val values = args.map(getConst)
            if (values.exists(_.isEmpty)) {
              None
            } else {
              val vs = values.map(_.get)
              Some(Evaluator.evalPrimitiveOperator(op, vs, loc).get)
            }
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
            val newOp = TesslaCore.CurriedPrimitiveOperator(op.op, newCurry)
            if (newArgs.isEmpty) {
              val value = Evaluator.evalPrimitiveOperator(newOp, Seq(), loc).get
              TesslaCore.Default(TesslaCore.Nil(loc), value, loc)
            } else {
              TesslaCore.SignalLift(newOp, newArgs, loc)
            }
          case _ => expression
        }
        StreamDescription(id, newExpression, typ)
    }

    TesslaCore.Specification(updatedStreams, spec.inStreams, spec.outStreams)
  }
}

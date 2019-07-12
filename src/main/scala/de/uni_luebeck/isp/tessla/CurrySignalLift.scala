package de.uni_luebeck.isp.tessla

class CurrySignalLift(spec: TesslaCore.Specification) extends TranslationPhase.Translator[TesslaCore.Specification] {
  override def translateSpec(): TesslaCore.Specification = {
    val streams = spec.streams.map { stream => stream.id -> stream }.toMap

    def getConst(ref: TesslaCore.StreamRef): Option[TesslaCore.ValueOrError] = ref match {
      case s: TesslaCore.Stream =>
        val exp = streams(s.id).expression
        exp match {
          case TesslaCore.Default(_: TesslaCore.Nil, value, _) => Some(value)
          case TesslaCore.SignalLift(op, args, loc) =>
            val values = args.toStream.map(getConst)
            if (values.exists(_.isEmpty)) {
              None
            } else {
              val vs = values.map(_.get)
              Some(Evaluator.evalPrimitiveOperator(op, vs, streams(s.id).typ.elementType, loc))
            }
          case _ => None
        }
      case _ => None
    }

    val updatedStreams = spec.streams.map { sd =>
      val newExpression = sd.expression match {
        case TesslaCore.SignalLift(op, argStreams, loc) =>
        val (newArgs, newCurry) =  argStreams.zipWithIndex.foldLeft((Seq.empty[TesslaCore.StreamRef], op.args)) { case ((newArgs, newCurry), (arg, i)) =>
            getConst(arg) match {
              case Some(value) => (newArgs, newCurry + (i -> value))
              case None => (newArgs :+ arg, newCurry)
            }
          }
          val newOp = TesslaCore.CurriedPrimitiveOperator(op.name, newCurry)
          if (newArgs.isEmpty) {
            val value = Evaluator.evalPrimitiveOperator(newOp, Seq(), sd.typ.elementType, loc)
            TesslaCore.Default(TesslaCore.Nil(sd.typ, loc), value, loc)
          } else {
            TesslaCore.SignalLift(newOp, newArgs, loc)
          }
        case _ => sd.expression
      }
      sd.copy(expression = newExpression)
    }

    spec.copy(streams = updatedStreams)
  }
}

object CurrySignalLift extends TranslationPhase[TesslaCore.Specification, TesslaCore.Specification] {
  override def translate(spec: TesslaCore.Specification) = {
    new CurrySignalLift(spec).translate()
  }
}
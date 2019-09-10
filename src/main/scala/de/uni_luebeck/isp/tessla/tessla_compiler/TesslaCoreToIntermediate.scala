package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing

import de.uni_luebeck.isp.tessla.{TesslaCore, TranslationPhase}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

/**
  * This class is the de.uni_luebeck.isp.tessla.TranslationPhase for the translation from TeSSLa Core to
  * abstract imperative code
  */
object TesslaCoreToIntermediate extends
        TranslationPhase[TesslaCore.Specification, SourceListing] {

  override def translate(spec: TesslaCore.Specification): Result[SourceListing] = {

    val streams = spec.streams
    val inStreams = spec.inStreams
    val outStreams = spec.outStreams

    var currSource = SourceListing(Seq())
    var warnings = Seq()

    streams.foreach{
      case TesslaCore.StreamDescription(id, expr, typ, annotations) =>
        currSource = expr match {
          case TesslaCore.Default(stream, default, loc) =>
            IntermediateCodeGenerator.produceDefaultStepCode(stream, default, loc, currSource)
          case TesslaCore.DefaultFrom(stream, defStream, loc) =>
            IntermediateCodeGenerator.produceDefaultFromStepCode(stream, defStream, loc, currSource)
          case TesslaCore.Time(stream, loc) =>
            IntermediateCodeGenerator.produceTimeStepCode(stream, loc, currSource)
          case TesslaCore.Last(values, clock, loc) =>
            IntermediateCodeGenerator.produceLastStepCode(values, clock, loc, currSource)
          case TesslaCore.Delay(delay, reset, loc) =>
            IntermediateCodeGenerator.produceDelayStepCode(delay, reset, loc, currSource)
          case TesslaCore.Lift(func, args, loc) =>
            IntermediateCodeGenerator.produceLiftStepCode(func, args, loc, currSource)
          case TesslaCore.SignalLift(op, args, loc) =>
            IntermediateCodeGenerator.produceSignalLiftStepCode(op, args, loc, currSource)
          case TesslaCore.CustomBuiltInCall(name, args, loc) =>
            IntermediateCodeGenerator.produceCustomBuiltInCallStepCode(name, args, loc, currSource)
          case _ => throw new Errors.CommandNotSupportedError(expr.toString)
        }
    }


    Success(currSource, warnings)
  }

}

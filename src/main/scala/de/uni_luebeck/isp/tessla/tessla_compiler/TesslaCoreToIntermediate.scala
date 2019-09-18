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

    streams.foreach { outStream: TesslaCore.StreamDescription => {
      val outStreamRef = TesslaCore.Stream(outStream.id, outStream.typ, outStream.loc)
      currSource =
      outStream.expression match {
        case TesslaCore.Default(stream, default, loc) =>
          IntermediateCodeGenerator.produceDefaultStepCode(outStreamRef, stream, default, loc, currSource)
        case TesslaCore.DefaultFrom(stream, defStream, loc) =>
          IntermediateCodeGenerator.produceDefaultFromStepCode(outStreamRef, stream, defStream, loc, currSource)
        case TesslaCore.Time(stream, loc) =>
          IntermediateCodeGenerator.produceTimeStepCode(outStreamRef, stream, loc, currSource)
        case TesslaCore.Last(values, clock, loc) =>
          IntermediateCodeGenerator.produceLastStepCode(outStreamRef, values, clock, loc, currSource)
        case TesslaCore.Delay(delay, reset, loc) =>
          IntermediateCodeGenerator.produceDelayStepCode(outStreamRef, delay, reset, loc, currSource)
        case TesslaCore.Lift(func, args, loc) =>
          IntermediateCodeGenerator.produceLiftStepCode(outStreamRef, func, args, loc, currSource)
        case TesslaCore.SignalLift(op, args, loc) =>
          IntermediateCodeGenerator.produceSignalLiftStepCode(outStreamRef, op, args, loc, currSource)
        case TesslaCore.CustomBuiltInCall(name, args, loc) =>
          IntermediateCodeGenerator.produceCustomBuiltInCallStepCode(outStreamRef, name, args, loc, currSource)
        case _ => throw new Errors.CommandNotSupportedError(outStream.expression.toString)
      }
    }
    }


    Success(currSource, warnings)
  }

}

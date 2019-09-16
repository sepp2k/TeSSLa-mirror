package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing

import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.TesslaCore.{Arg, CurriedPrimitiveOperator, Function, StreamRef, ValueOrError}

/**
  * This class contains functions for the translation of single TeSSLa expressions to imperative code
  */
object IntermediateCodeGenerator {

    throw new Errors.NotYetImplementedError(loc)
  def produceDefaultStepCode(outStream: Stream, stream: StreamRef, default: ValueOrError, loc: Location, currSrc: SourceListing): SourceListing = {
  }

  def produceDefaultFromStepCode(outStream: Stream, stream: StreamRef, defaultStream: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceTimeStepCode(outStream: Stream, stream: StreamRef, loc: Location, currSrc: SourceListing) : SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceLastStepCode(outStream: Stream, values: StreamRef, clock: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceDelayStepCode(outStream: Stream, delay: StreamRef, reset: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceLiftStepCode(outStream: Stream, f: Function, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceSignalLiftStepCode(outStream: Stream, op: CurriedPrimitiveOperator, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceCustomBuiltInCallStepCode(outStream: Stream, name: String, args: Seq[Arg], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

}

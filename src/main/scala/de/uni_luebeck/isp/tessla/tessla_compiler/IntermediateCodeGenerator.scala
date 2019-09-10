package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing

import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.TesslaCore.{Arg, CurriedPrimitiveOperator, Function, StreamRef, ValueOrError}

/**
  * This class contains functions for the translation of single TeSSLa expressions to imperative code
  */
object IntermediateCodeGenerator {

  def produceDefaultStepCode(stream: StreamRef, default: ValueOrError, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceDefaultFromStepCode(stream: StreamRef, defaultStream: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceTimeStepCode(stream: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceLastStepCode(values: StreamRef, clock: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceDelayStepCode(delay: StreamRef, reset: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceLiftStepCode(f: Function, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceSignalLiftStepCode(op: CurriedPrimitiveOperator, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

  def produceCustomBuiltInCallStepCode(name: String, args: Seq[Arg], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError(loc)
  }

}

package de.uni_luebeck.isp.tessla.tessla_compiler

import scala.language.implicitConversions
import scala.language.postfixOps
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeDSL._
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.TesslaCore.{BoolValue => _, StringValue => _, _}


/**
  * This class contains functions for the translation of single TeSSLa expressions to imperative code
  */
object IntermediateCodeGenerator {

  def streamNameAndType(s : StreamRef) : (String, ImpLanType) = s match {
    case Stream(id, t, _) => (id.uid.toString(), t.elementType)
    case InputStream(n, t, _) => (n, t.elementType)
    case Nil(t,_) => (s"nil_$t", t.elementType)
  }

  def produceDefaultStepCode(outStream: Stream, stream: StreamRef, default: ValueOrError, loc: Location, currSrc: SourceListing): SourceListing = {
    val (s, _) = streamNameAndType(stream)
    val o = outStream.id.uid
    val ot = outStream.typ.elementType

    val newStmt = (currSrc.stepSource

      If(Set(Set(NotEqual("currTs", LongValue(0)))))
        Assignment(s"${o}_changed", BoolValue(false), BoolValue(true), BoolType)
      EndIf()
      If(Set(Set(s"${s}_changed")))
        Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForType(ot), ot)
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
        Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType)
        Assignment(s"${o}_value", s"${s}_value", default, ot)
        Assignment(s"${o}_init", BoolValue(true), BoolValue(true), BoolType)
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
        Assignment(s"${o}_error", s"${s}_error", LongValue(0), LongType)
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      EndIf()

      )
    SourceListing(newStmt)
  }

  def produceDefaultFromStepCode(outStream: Stream, stream: StreamRef, defaultStream: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc) //Why??
  }

  def produceTimeStepCode(outStream: Stream, stream: StreamRef, loc: Location, currSrc: SourceListing) : SourceListing = {

    val (s, _) = streamNameAndType(stream)
    val o = outStream.id.uid
    val ot = outStream.typ.elementType

    val newStmt = (currSrc.stepSource

    Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
    If(Set(Set(s"${s}_changed")))
      Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForType(ot), ot)
      Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType)
      Assignment(s"${o}_value", s"${s}_ts", defaultValueForType(ot), ot)
      Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
      Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      Assignment(s"${o}_error", s"${s}_error", LongValue(0), LongType)
      Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
    EndIf()

      )
    SourceListing(newStmt)
  }

  def produceLastStepCode(outStream: Stream, values: StreamRef, clock: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    val (v, _) = streamNameAndType(values)
    val (c, _) = streamNameAndType(clock)
    val o = outStream.id.uid
    val ot = outStream.typ.elementType

    val newStmt = (currSrc.stepSource

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      If(Set(Set(s"${c}_changed", s"${v}_init")))
        Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForType(ot), ot)
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
        Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType)
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
        If(Set(Set(Equal(s"${v}_ts", "currTs"))))
          Assignment(s"${o}_value", s"${v}_lastValue", defaultValueForType(ot), ot)
          Assignment(s"${o}_error", s"${v}_lastError", LongValue(0), LongType)
          Assignment(s"${o}_init", s"${v}_lastInit", LongValue(0), LongType)
        Else()
          Assignment(s"${o}_value", s"${v}_value", defaultValueForType(ot), ot)
          Assignment(s"${o}_error", s"${v}_error", LongValue(0), LongType)
          Assignment(s"${o}_init", s"${v}_init", LongValue(0), LongType)
        EndIf()
      EndIf()

      )
    SourceListing(newStmt)
  }

  def produceDelayStepCode(outStream: Stream, delay: StreamRef, reset: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    val (d, _) = streamNameAndType(delay)
    val (r, _) = streamNameAndType(reset)
    val o = outStream.id.uid
    val ot = outStream.typ.elementType

    val newStmt = (currSrc.stepSource

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      If(Set(Set(Equal(s"${o}_nextTs", "currTs"))))
        FinalAssignment(s"${o}_lastValue", defaultValueForType(ot), ot)
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
        Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType)
        FinalAssignment(s"${o}_value", defaultValueForType(ot), ot)
        Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      EndIf()

      )

    val newTsGen = (currSrc.tsGenSource

      If(Set(Set(s"${o}_changed", s"${d}_changed"), Set(s"${r}_changed", s"${d}_changed")))
        Assignment(s"${o}_nextTs", Addition("currTs", s"${d}_value"), LongValue(-1), LongType)
      EndIf()

      )

    SourceListing(newStmt, newTsGen, currSrc.functions)
  }

  def produceLiftStepCode(outStream: Stream, f: Function, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc)
  }

  def produceSignalLiftStepCode(outStream: Stream, op: CurriedPrimitiveOperator, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc)
  }

  def produceCustomBuiltInCallStepCode(outStream: Stream, name: String, args: Seq[Arg], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc)
  }

}

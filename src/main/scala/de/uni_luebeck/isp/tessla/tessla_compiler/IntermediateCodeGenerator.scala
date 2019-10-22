package de.uni_luebeck.isp.tessla.tessla_compiler

import scala.language.implicitConversions
import scala.language.postfixOps
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeDSL._
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.TesslaCore.{FunctionType, BoolValue => _, StringValue => _, _}


/**
  * This class contains functions for the translation of single TeSSLa expressions to imperative code
  */
object IntermediateCodeGenerator {

  def streamNameAndType(s : StreamRef) : (String, ImpLanType) = s match {
    case Stream(id, t, _) => ("var_" + id.uid.toString(), t.elementType)
    case InputStream(n, t, _) => ("var_" + n, t.elementType)
    case Nil(t,_) => (s"var_nil_$t", t.elementType)
  }

  def produceDefaultStepCode(outStream: Stream, stream: StreamRef, default: ValueOrError, loc: Location, currSrc: SourceListing): SourceListing = {
    val (s, _) = streamNameAndType(stream)
    val o = s"var_${outStream.id.uid}"
    val ot = outStream.typ.elementType

    val newStmt = (currSrc.stepSource

      If(Seq(Seq(NotEqual("currTs", LongValue(0)))))
        Assignment(s"${o}_changed", BoolValue(false), BoolValue(true), BoolType)
      EndIf()
      If(Seq(Seq(s"${s}_changed")))
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
    SourceListing(newStmt, currSrc.tsGenSource)
  }

  def produceDefaultFromStepCode(outStream: Stream, stream: StreamRef, defaultStream: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc) //Why??
  }

  def produceTimeStepCode(outStream: Stream, stream: StreamRef, loc: Location, currSrc: SourceListing) : SourceListing = {

    val (s, _) = streamNameAndType(stream)
    val o = s"var_${outStream.id.uid}"
    val ot = outStream.typ.elementType

    val newStmt = (currSrc.stepSource

    Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
    If(Seq(Seq(s"${s}_changed")))
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
    SourceListing(newStmt, currSrc.tsGenSource)
  }

  def produceLastStepCode(outStream: Stream, values: StreamRef, clock: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    val (v, _) = streamNameAndType(values)
    val (c, _) = streamNameAndType(clock)
    val o = s"var_${outStream.id.uid}"
    val ot = outStream.typ.elementType

    val newStmt = (currSrc.stepSource

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      If(Seq(Seq(s"${c}_changed", s"${v}_init")))
        Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForType(ot), ot)
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
        Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType)
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
        If(Seq(Seq(Equal(s"${v}_ts", "currTs"))))
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
    SourceListing(newStmt, currSrc.tsGenSource)
  }

  def produceDelayStepCode(outStream: Stream, delay: StreamRef, reset: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    val (d, _) = streamNameAndType(delay)
    val (r, _) = streamNameAndType(reset)
    val o = s"var_${outStream.id.uid}"
    val ot = outStream.typ.elementType

    val newStmt = (currSrc.stepSource

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      If(Seq(Seq(Equal(s"${o}_nextTs", "currTs"))))
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

      If(Seq(Seq(s"${o}_changed", s"${d}_changed"), Seq(s"${r}_changed", s"${d}_changed")))
        Assignment(s"${o}_nextTs", Addition("currTs", s"${d}_value"), LongValue(-1), LongType)
      EndIf()

      )

    SourceListing(newStmt, newTsGen)
  }

  def produceLiftStepCode(outStream: Stream, f: Function, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    val o = s"var_${outStream.id.uid}"
    val ot = outStream.typ.elementType
    val params = args.map{sr => TernaryExpression(Seq(Seq(Equal(s"${streamNameAndType(sr)._1}_ts", "currTs"))),
                                                  FunctionCall("Some", Seq(s"${streamNameAndType(sr)._1}_value")),
                                                  None)
                         }
    val guard : Seq[Seq[ImpLanExpr]] = args.map{sr => Seq(Variable(streamNameAndType(sr)._1))}

    val newStmt = (currSrc.stepSource

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      If(guard)
        Assignment(s"${o}_f", FunctionGenerator.generateLambda(f, s"${o}_f"), defaultValueForType(FunctionType), FunctionType)
        Assignment(s"${o}_fval", FunctionVarApplication(s"${o}_f", params), defaultValueForType(ot), ot)
        If(Seq(Seq(FunctionCall("isSome", Seq(s"${o}_fval")))))
          Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForType(ot), ot)
          Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
          Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType)
          Assignment(s"${o}_value", FunctionCall("getSome", Seq(s"${o}_fval")), defaultValueForType(ot), ot)
          Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
          Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
          // Assignment(s"${o}_error", s"${s}_error", LongValue(0), LongType) //TODO: Error Generation
          Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
        EndIf()
      EndIf()

      )
    SourceListing(newStmt, currSrc.tsGenSource)
  }

  def produceSignalLiftStepCode(outStream: Stream, op: CurriedPrimitiveOperator, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc)
  }

  def produceCustomBuiltInCallStepCode(outStream: Stream, name: String, args: Seq[Arg], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc)
  }

}

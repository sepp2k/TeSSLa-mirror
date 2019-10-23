package de.uni_luebeck.isp.tessla.tessla_compiler

import scala.language.implicitConversions
import scala.language.postfixOps
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeDSL._
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.TesslaCore.{FunctionType, BoolValue => _, StringValue => _, _}


/**
  * Class containing functions for the translation of single TeSSLa expressions to imperative code
  */
object IntermediateCodeGenerator {

  def streamNameAndType(s : StreamRef) : (String, ImpLanType) = s match {
    case Stream(id, t, _) => ("var_" + id.uid.toString(), t.elementType)
    case InputStream(n, t, _) => ("var_" + n, t.elementType)
    case Nil(t,_) => (s"var_nil_$t", t.elementType)
  }

  def outputStreamName(s : StreamRef) : String = s match {
    case Stream(id, t, _) => id.nameOpt.getOrElse(id.uid.toString())
    case InputStream(n, t, _) => n
    case Nil(t,_) => "nil"
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
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing)
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
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing)
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
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing)
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
        Assignment(s"${o}_nextTs", Addition("lastProcessedTs", s"${d}_value"), LongValue(-1), LongType)
      EndIf()
      If(Seq(Seq(Greater(s"${o}_nextTs", "lastProcessedTs"), Greater("currTs", s"${o}_nextTs"), Greater("newInputTs", s"${o}_nextTs"))))
        Assignment("currTs", s"${o}_nextTs", LongValue(0), LongType)
      EndIf()

      )

    SourceListing(newStmt, newTsGen, currSrc.inputProcessing)
  }

  def produceLiftStepCode(outStream: Stream, f: Function, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    val o = s"var_${outStream.id.uid}"
    val ot = outStream.typ.elementType
    val params = args.map{sr => { val (sName, sType) = streamNameAndType(sr)
                                  TernaryExpression(Seq(Seq(Equal(s"${sName}_ts", "currTs"))),
                                                  FunctionCall("Some", Seq(s"${sName}_value"), IntermediateCode.FunctionType(Seq(sType), OptionType(sType))),
                                                  None)
                                }
                         }
    val guard : Seq[Seq[ImpLanExpr]] = args.map{sr => Seq(Variable(s"${streamNameAndType(sr)._1}_changed"))}

    val newStmt = (currSrc.stepSource

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      If(guard)
        Assignment(s"${o}_f", FunctionGenerator.generateLambda(f, s"${o}_f"), defaultValueForType(FunctionType), FunctionType)
        Assignment(s"${o}_fval", FunctionVarApplication(s"${o}_f", params), defaultValueForType(ot), ot)
        If(Seq(Seq(FunctionCall("isSome", Seq(s"${o}_fval"), IntermediateCode.FunctionType(Seq(OptionType(ot)), BoolType)))))
          Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForType(ot), ot)
          Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
          Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType)
          Assignment(s"${o}_value", FunctionCall("getSome", Seq(s"${o}_fval"), IntermediateCode.FunctionType(Seq(OptionType(ot)), ot)), defaultValueForType(ot), ot)
          Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
          Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
          // Assignment(s"${o}_error", s"${s}_error", LongValue(0), LongType) //TODO: Error Generation
          Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
        EndIf()
      EndIf()

      )
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing)
  }

  def produceSignalLiftStepCode(outStream: Stream, op: CurriedPrimitiveOperator, args: Seq[StreamRef], loc: Location, currSrc: SourceListing): SourceListing = {
    val o = s"var_${outStream.id.uid}"
    val ot = outStream.typ.elementType

    val guard1 : Seq[Seq[ImpLanExpr]] = Seq(args.map{sr => Variable(s"${streamNameAndType(sr)._1}_init")})
    val guard2 : Seq[Seq[ImpLanExpr]] = args.map{sr => Seq(Variable(s"${streamNameAndType(sr)._1}_changed"))}

    val fargs : Seq[(ImpLanExpr, ImpLanType)] = {
      if (op.args.isEmpty) {
        args.map(streamNameAndType)
      } else {
        var pos = 0
        (0 until op.args.keys.max + args.size).flatMap{
          case i if op.args.contains(i) => scala.Some(streamNameAndType(args(i)))
          case _ if pos >= args.size => scala.None
          case _ => {
                      pos += 1
                      scala.Some(streamNameAndType(args(pos - 1)))
                    }
        }
      }
    }.map{case (n, t) => (Variable(s"${n}_value"), t)}

    val newStmt = (currSrc.stepSource

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      If(guard1)
        If(guard2)
          Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForType(ot), ot)
          Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
          Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType)
          Assignment(s"${o}_value", ErrorGenerator.generateErrorPreventingCallSLift(op.name, fargs, ot), defaultValueForType(ot), ot)
          Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
          Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
          Assignment(s"${o}_error", ErrorGenerator.generateErrorCodeExpressionSLift(op.name, fargs), LongValue(0), LongType)
          Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
        EndIf()
      EndIf()
      )
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing)
  }

  def produceCustomBuiltInCallStepCode(outStream: Stream, name: String, args: Seq[Arg], loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.CommandNotSupportedError(s"CustomBuiltInCalls are not supported: $name", loc)
  }

  def produceOutputCode(outStream: TesslaCore.OutStreamDescription, currSrc: SourceListing) : SourceListing = {
    val (s, t) = streamNameAndType(outStream.stream)
    val name = outStream.nameOpt.getOrElse(outputStreamName(outStream.stream))

    val newStmt = (currSrc.stepSource
      If(Seq(Seq(s"${s}_changed")))
        FunctionCall("__[TC]output__", Seq(s"${s}_value", StringValue(name), s"${s}_error"),
                      IntermediateCode.FunctionType(Seq(t, StringType, LongType), UnitType))
      EndIf()
    )

    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing)
  }

  def produceInputUnchangeCode(inStream: TesslaCore.InStreamDescription, currSrc: SourceListing) = {
    val s = inStream.name

    val newStmt = (
      currSrc.stepSource
        Assignment(s"${s}_changed", BoolValue(false), BoolValue(false), BoolType)
      )

    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing)
  }

  def produceInputFromConsoleCode(inStream: TesslaCore.InStreamDescription, currSrc: SourceListing) = {
    val s = s"var_${inStream.name}"
    val st = inStream.typ.elementType

    val newInputProcessing = (currSrc.inputProcessing
      If(Seq(Seq(Equal("inputStream", StringValue(inStream.name)))))
        Assignment(s"${s}_lastValue", s"${s}_value", defaultValueForType(st), st)
        Assignment(s"${s}_lastInit", s"${s}_init", BoolValue(false), BoolType)
        FinalAssignment(s"${s}_lastError", LongValue(0), LongType)
        Assignment(s"${s}_value", FunctionCall("__[TC]inputParse__", Seq("value"),
                   IntermediateCode.FunctionType(Seq(StringType), st)
                   ), defaultValueForType(st), st)
        Assignment(s"${s}_init", BoolValue(true), BoolValue(false), BoolType)
        Assignment(s"${s}_ts", "currTs", LongValue(0), LongType)
        FinalAssignment(s"${s}_error", LongValue(0), LongType)
        Assignment(s"${s}_changed", BoolValue(true), BoolValue(false), BoolType)
      EndIf()
    )

    SourceListing(currSrc.stepSource, currSrc.tsGenSource, newInputProcessing)
  }

}

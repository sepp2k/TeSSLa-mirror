package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core._

import scala.language.implicitConversions
import scala.language.postfixOps
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{BoolType, StringType, UnitType, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils._
import de.uni_luebeck.isp.tessla._
/**
  * Class containing functions for the translation of single TeSSLa expressions to imperative code
  */
object StreamCodeGenerator {

  def streamNameAndTypeFromExpressionArg(ea : ExpressionArg) : (String, ImpLanType) = {
    ea match {
      case ExpressionRef(id, tpe, _) =>("var_" + id.fullName, tpe)
      case e: Expression => throw tessla_compiler.Errors.CoreASTError("Required ExpressionRef, but Expression found", e.location)
    }
  }

  def produceNilStepCode(id: Identifier, ot: Type, loc: Location, currSrc: SourceListing): SourceListing = {
    val o = s"var_${id.fullName}"

    val newStmt = (currSrc.stepSource.

      FinalAssignment(s"${o}_lastValue", defaultValueForStreamType(ot), ot).
      FinalAssignment(s"${o}_lastInit", BoolValue(false), BoolType).
      FinalAssignment(s"${o}_lastError", LongValue(0), LongType).
      FinalAssignment(s"${o}_value", defaultValueForStreamType(ot), ot).
      FinalAssignment(s"${o}_init", BoolValue(false), BoolType).
      FinalAssignment(s"${o}_ts", LongValue(0), LongType).
      FinalAssignment(s"${o}_error", LongValue(0), LongType).
      FinalAssignment(s"${o}_changed", BoolValue(false), BoolType)
      )

    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceDefaultStepCode(id: Identifier, ot: Type, stream : ExpressionArg, value : ExpressionArg, loc: Location, currSrc: SourceListing): SourceListing = {
    val (s, _) = streamNameAndTypeFromExpressionArg(stream)
    val o = s"var_${id.fullName}"
    val default = NonStreamCodeGenerator.translateExpressionArg(value)

    val newStmt = (currSrc.stepSource.

      If(Seq(Seq(NotEqual("currTs", LongValue(0))))).
        Assignment(s"${o}_changed", BoolValue(false), BoolValue(true), BoolType).
      EndIf().
      If(Seq(Seq(s"${s}_changed"))).
        Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot).
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
        Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
        Assignment(s"${o}_value", s"${s}_value", default, ot).
        Assignment(s"${o}_init", BoolValue(true), BoolValue(true), BoolType).
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
        Assignment(s"${o}_error", s"${s}_error", LongValue(0), LongType).
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(true), BoolType).
      EndIf()

      )
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceDefaultFromStepCode(id: Identifier, ot: Type, stream : ExpressionArg, default : ExpressionArg, loc: Location, currSrc: SourceListing): SourceListing = {
    val (s, _) = streamNameAndTypeFromExpressionArg(stream)
    val (d, _) = streamNameAndTypeFromExpressionArg(default)
    val o = s"var_${id.fullName}"

    val newStmt = (currSrc.stepSource.

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType).
      If(Seq(Seq(s"${s}_changed"))).
        Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot).
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
        Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
        Assignment(s"${o}_value", s"${s}_value", defaultValueForStreamType(ot), ot).
        Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType).
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
        Assignment(s"${o}_error", s"${s}_error", LongValue(0), LongType).
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType).
      Else().
        If(Seq(Seq(Negation(s"${o}_init"),s"${d}_init"))).
          Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot).
          Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
          Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
          Assignment(s"${o}_value", s"${d}_value", defaultValueForStreamType(ot), ot).
          Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType).
          Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
          Assignment(s"${o}_error", s"${d}_error", LongValue(0), LongType).
          Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType).
        EndIf().
      EndIf()

      )
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceTimeStepCode(id: Identifier, stream: ExpressionArg, loc: Location, currSrc: SourceListing) : SourceListing = {

    val (s, _) = streamNameAndTypeFromExpressionArg(stream)
    val o = s"var_${id.fullName}"

    val newStmt = (currSrc.stepSource.

    Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType).
    If(Seq(Seq(s"${s}_changed"))).
      Assignment(s"${o}_lastValue", s"${o}_value", LongValue(0), LongType).
      Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
      Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
      Assignment(s"${o}_value", s"${s}_ts", LongValue(0), LongType).
      Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType).
      Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
      Assignment(s"${o}_error", s"${s}_error", LongValue(0), LongType).
      Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType).
    EndIf()

      )
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceLastStepCode(id: Identifier, ot: Type, values: ExpressionArg, clock: ExpressionArg, loc: Location, currSrc: SourceListing): SourceListing = {
    val (v, _) = streamNameAndTypeFromExpressionArg(values)
    val (c, _) = streamNameAndTypeFromExpressionArg(clock)
    val o = s"var_${id.fullName}"

    val newStmt = (currSrc.stepSource.

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType).
      If(Seq(Seq(s"${c}_changed", s"${v}_init"))).
        Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot).
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
        Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType).
        If(Seq(Seq(Equal(s"${v}_ts", "currTs")))).
          Assignment(s"${o}_value", s"${v}_lastValue", defaultValueForStreamType(ot), ot).
          Assignment(s"${o}_error", s"${v}_lastError", LongValue(0), LongType).
          Assignment(s"${o}_init", s"${v}_lastInit", BoolValue(false), BoolType).
        Else().
          Assignment(s"${o}_value", s"${v}_value", defaultValueForStreamType(ot), ot).
          Assignment(s"${o}_error", s"${v}_error", LongValue(0), LongType).
          Assignment(s"${o}_init", s"${v}_init", BoolValue(false), BoolType).
        EndIf().
      EndIf()

      )
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceDelayStepCode(id: Identifier, delay: ExpressionArg, reset: ExpressionArg, loc: Location, currSrc: SourceListing): SourceListing = {
    val (d, _) = streamNameAndTypeFromExpressionArg(delay)
    val (r, _) = streamNameAndTypeFromExpressionArg(reset)
    val o = s"var_${id.fullName}"

    val newStmt = (currSrc.stepSource.

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType).
      If(Seq(Seq(Equal(s"${o}_nextTs", "currTs")))).
        FinalAssignment(s"${o}_lastValue", UnitValue, UnitType).
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
        FinalAssignment(s"${o}_lastError", LongValue(0), LongType).
        FinalAssignment(s"${o}_value", UnitValue, UnitType).
        Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType).
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType).
      EndIf()

      )

    val newTsGen = (currSrc.tsGenSource.

      If(Seq(Seq(s"${o}_changed", s"${d}_changed"), Seq(s"${r}_changed", s"${d}_changed"))).
        If(Seq(Seq(NotEqual(s"${o}_error", LongValue(0))), Seq(NotEqual(s"${d}_error", LongValue(0))), Seq(NotEqual(s"${r}_error", LongValue(0))))).
          Assignment(s"${o}_nextTs", LongValue(-1), LongValue(-1), LongType).
          Assignment(s"${o}_error", BitwiseOr(Seq(s"${o}_error", s"${d}_error", s"${r}_error")), LongValue(-1), LongType).
        Else().
          Assignment(s"${o}_nextTs", Addition("lastProcessedTs", s"${d}_value"), LongValue(-1), LongType).
        EndIf().
      EndIf().
      If(Seq(Seq(Greater(s"${o}_nextTs", "lastProcessedTs"), Greater("currTs", s"${o}_nextTs"), Greater("newInputTs", s"${o}_nextTs")))).
        Assignment("currTs", s"${o}_nextTs", LongValue(0), LongType).
      EndIf()

      )

    SourceListing(newStmt, newTsGen, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceLiftStepCode(id: Identifier, ot: Type, args: Seq[ExpressionArg], function: ExpressionArg, loc: Location, currSrc: SourceListing): SourceListing = {
    val o = s"var_${id.fullName}"

    val params = args.map{sr => { val (sName, sType) = streamNameAndTypeFromExpressionArg(sr) //TODO: Sufficient???
                                  TernaryExpression(Seq(Seq(s"${sName}_changed")),
                                                  FunctionCall("__Some__", Seq(s"${sName}_value"), IntermediateCode.FunctionType(Seq(sType), OptionType(sType))),
                                                  None(sType))
                                }
                         }

    //TODO: Special case: if
    val inputError = BitwiseOr(args.map(streamNameAndTypeFromExpressionArg(_)._1 + "_error"))
    val guard : Seq[Seq[ImpLanExpr]] = args.map{sr => Seq(Variable(s"${streamNameAndTypeFromExpressionArg(sr)._1}_changed"))}

    val newStmt = (currSrc.stepSource.

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType).
      If(guard).
        Assignment(s"${o}_errval", inputError, LongValue(0), LongType).
        If(Seq(Seq(Equal(s"${o}_errval", LongValue(0))))).
          Try().
            Assignment(s"${o}_fval", NonStreamCodeGenerator.translateFunctionCall(function, params, Seq()), scala.None, OptionType(ot)).
            If(Seq(Seq(FunctionCall("__isSome__", Seq(s"${o}_fval"), IntermediateCode.FunctionType(Seq(OptionType(ot)), BoolType))))).
              Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot).
              Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
              Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
              Assignment(s"${o}_value", FunctionCall("__getSome__", Seq(s"${o}_fval"), IntermediateCode.FunctionType(Seq(OptionType(ot)), ot)), defaultValueForStreamType(ot), ot).
              Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType).
              Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
              Assignment(s"${o}_error", LongValue(0), LongValue(0), LongType).
              Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType).
            EndIf().
          Catch().
            Assignment(s"${o}_errval", FunctionCall("__[TC]getErrorCode__", Seq(s"var_err"), IntermediateCode.FunctionType(Seq(GeneralType), LongType)), LongValue(0), LongType).
          EndTry().
        EndIf().
        If(Seq(Seq(NotEqual(s"${o}_errval", LongValue(0))))).
          Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot).
          Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
          Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
          Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType).
          Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
          Assignment(s"${o}_error", s"${o}_errval", LongValue(0), LongType).
          Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType).
        EndIf().
      EndIf()

      )
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceSignalLiftStepCode(id: Identifier, ot: Type, args: Seq[ExpressionArg], function: ExpressionArg, loc: Location, currSrc: SourceListing): SourceListing = {
    val o = s"var_${id.fullName}"

    val guard1 : Seq[Seq[ImpLanExpr]] = Seq(args.map{arg => Variable(s"${streamNameAndTypeFromExpressionArg(arg)._1}_init")})
    val guard2 : Seq[Seq[ImpLanExpr]] = args.map{arg => Seq(Variable(s"${streamNameAndTypeFromExpressionArg(arg)._1}_changed"))}
    val fargs = args.map{arg => Variable(s"${streamNameAndTypeFromExpressionArg(arg)._1}_value")}//TODO: Sufficient?

    val fcall = NonStreamCodeGenerator.translateFunctionCall(function, fargs, Seq())

    //TODO: Special case: if
    val inputError = BitwiseOr(args.map(streamNameAndTypeFromExpressionArg(_)._1 + "_error"))

    val newStmt = (currSrc.stepSource.

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType).
      If(guard1).
        If(guard2).
          Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot).
          Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
          Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
          Assignment(s"${o}_error", inputError, LongValue(0), LongType).
          If(Seq(Seq(Equal(s"${o}_error", LongValue(0))))).
            Try().
                Assignment(s"${o}_value", fcall, defaultValueForStreamType(ot), ot).
                Assignment(s"${o}_error", LongValue(0), LongValue(0), LongType).
              Catch().
                Assignment(s"${o}_error", FunctionCall("__[TC]getErrorCode__", Seq(s"var_err"), IntermediateCode.FunctionType(Seq(GeneralType), LongType)), LongValue(0), LongType).
              EndTry().
          EndIf().

          Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType).
          Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
          Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType).
        EndIf().
      EndIf()
      )
    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceMergeStepCode(id: Identifier, ot: Type, args: Seq[ExpressionArg], loc: Location, currSrc: SourceListing): SourceListing = {
    val o = s"var_${id.fullName}"

    val guard : Seq[Seq[ImpLanExpr]] = args.map{arg =>
      val n = streamNameAndTypeFromExpressionArg(arg)._1
      Seq(Variable(s"${n}_init"), Variable(s"${n}_changed"))
    }

    var newStmt = (currSrc.stepSource.

      Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType).
      If(guard).
        Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot).
        Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType).
        Assignment(s"${o}_lastError", s"${o}_error", LongValue(0), LongType).
        Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType).
        Assignment(s"${o}_ts", "currTs", LongValue(0), LongType).
        Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      )

    args.foreach{arg =>
      val (sn, _) = streamNameAndTypeFromExpressionArg(arg)
      newStmt = (newStmt.
        If(Seq(Seq(s"${sn}_init",s"${sn}_changed"))).
          Assignment(s"${o}_value", s"${sn}_value", defaultValueForStreamType(ot), ot).
          Assignment(s"${o}_error", s"${sn}_error", LongValue(0), LongType).
        Else()
      )
    }

    (1 to args.length + 1).foreach{_ => newStmt = newStmt.EndIf()}

    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceOutputCode(id: Identifier, t: Type, nameOpt: Option[String], currSrc: SourceListing) : SourceListing = {
    val s = s"var_${id.fullName}"
    val name = nameOpt.getOrElse(id.idOrName.left.getOrElse(id.fullName))

    val newStmt = (currSrc.stepSource.
      If(Seq(Seq(s"${s}_changed"))).
        FunctionCall("__[TC]output__", Seq(s"${s}_value", StringValue(name), s"${s}_error"),
                      IntermediateCode.FunctionType(Seq(t, StringType, LongType), UnitType)).
      EndIf()
    )

    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceInputUnchangeCode(inStream: Identifier, currSrc: SourceListing) = {
    val s = s"var_${inStream.fullName}"

    val newStmt = (
      currSrc.stepSource.
        Assignment(s"${s}_changed", BoolValue(false), BoolValue(false), BoolType)
      )

    SourceListing(newStmt, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  def produceInputFromConsoleCode(inStream: Identifier, typ: Type, currSrc: SourceListing) = {
    val s = s"var_${inStream.fullName}"

    val newInputProcessing = (currSrc.inputProcessing.
      If(Seq(Seq(Equal("inputStream", StringValue(inStream.idOrName.left.get))))).
        Assignment(s"${s}_lastValue", s"${s}_value", defaultValueForStreamType(typ), typ).
        Assignment(s"${s}_lastInit", s"${s}_init", BoolValue(false), BoolType).
        FinalAssignment(s"${s}_lastError", LongValue(0), LongType).
        Assignment(s"${s}_value", FunctionCall("__[TC]inputParse__", Seq("value"),
                   IntermediateCode.FunctionType(Seq(StringType), typ)
                   ), defaultValueForStreamType(typ), typ).
        Assignment(s"${s}_init", BoolValue(true), BoolValue(false), BoolType).
        Assignment(s"${s}_ts", "currTs", LongValue(0), LongType).
        FinalAssignment(s"${s}_error", LongValue(0), LongType).
        Assignment(s"${s}_changed", BoolValue(true), BoolValue(false), BoolType).
      EndIf()
    )

    SourceListing(currSrc.stepSource, currSrc.tsGenSource, newInputProcessing, currSrc.staticSource)
  }

}

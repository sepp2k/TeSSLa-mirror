package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{BoolType, StringType, UnitType, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils._

import scala.language.{implicitConversions, postfixOps}

/**
 * Class containing functions for the translation of single TeSSLa stream expressions to imperative code
 */

class StreamCodeGenerator(nonStreamCodeGenerator: NonStreamCodeGenerator) {

  /**
   * Translates an assignment to a stream variable to the corresponding ImpLan Code and attaches it in a given
   * source listing. The assigned expression has to be (and is always in TeSSLa Core) an application.
   *
   * @param id The id which is assigned (must be of stream type)
   * @param e The expression which is applicated with args and typeArgs and then assigned
   * @param args The arguments passed to e
   * @param typeArgs The type arguments passed to e
   * @param currSource The source listing where the generated code is attached to. It is attached to the
   *                   stepSource section of the source listing.
   * @return The modified source listing
   */
  def translateExternSignalExpression(
    id: Identifier,
    e: ExternExpression,
    args: Seq[ExpressionArg],
    typeArgs: Seq[Type],
    currSource: SourceListing
  ): SourceListing = {
    val typ = e.tpe.asInstanceOf[Core.FunctionType]
    val typeParamMap = typ.typeParams.zip(typeArgs).toMap
    e.name match {
      case "nil" =>
        produceNilStepCode(id, typ.resultType.resolve(typeParamMap), currSource)
      case "default" =>
        produceDefaultStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "defaultFrom" =>
        produceDefaultFromStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "time" =>
        produceTimeStepCode(id, args(0), currSource)
      case "last" =>
        produceLastStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "delay" =>
        produceDelayStepCode(id, args(0), args(1), currSource)
      case "lift" =>
        produceLiftStepCode(id, typ.resultType.resolve(typeParamMap), args.dropRight(1), args.last, currSource)
      case "slift" =>
        produceSignalLiftStepCode(id, typ.resultType.resolve(typeParamMap), args.dropRight(1), args.last, currSource)
      case "merge" =>
        produceMergeStepCode(id, typ.resultType.resolve(typeParamMap), args, currSource)
      case _ => throw tessla_compiler.Diagnostics.CommandNotSupportedError(e.toString)
    }
  }

  /**
   * Returns name and type if ea is an ExpressionRef otherwise an exception is thrown
   * @param ea The expression to be examined
   * @return Name and type of ea if it is an ExpressionRef
   */
  private def streamNameAndTypeFromExpressionArg(ea: ExpressionArg): (String, ImpLanType) = {
    ea match {
      case ExpressionRef(id, tpe, _) => ("var_" + id.fullName, tpe)
      case e: Expression =>
        throw tessla_compiler.Diagnostics
          .CoreASTError("Required ExpressionRef, but Expression found. Non flat AST.", e.location)
    }
  }

  /**
   * Produces ImpLan code for a x = nil expression
   *
   * @param id The id nil is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param currSrc The source listing the generated block is added to. It is attached to the
   *               stepSource section of the source listing
   * @return The modified source listing
   */
  private def produceNilStepCode(id: Identifier, ot: Type, currSrc: SourceListing): SourceListing = {
    val o = s"var_${id.fullName}"

    val newStmt = currSrc.stepSource
      .FinalAssignment(s"${o}_lastValue", defaultValueForStreamType(ot), ot)
      .FinalAssignment(s"${o}_lastInit", BoolValue(false), BoolType)
      .FinalAssignment(s"${o}_lastError", NoError, ErrorType)
      .FinalAssignment(s"${o}_value", defaultValueForStreamType(ot), ot)
      .FinalAssignment(s"${o}_init", BoolValue(false), BoolType)
      .FinalAssignment(s"${o}_ts", LongValue(0), LongType)
      .FinalAssignment(s"${o}_error", NoError, ErrorType)
      .FinalAssignment(s"${o}_changed", BoolValue(false), BoolType)
      .FinalAssignment(s"${o}_unknown", BoolValue(false), BoolType)

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces ImpLan code for a x = default(...) expression
   *
   * @param id The id default is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param stream The stream parameter of the default
   * @param defVal The default parameter of the default
   * @param currSrc The source listing the generated block is added to. It is attached to the
   *               stepSource section of the source listing
   * @return The modified source listing
   */
  private def produceDefaultStepCode(
    id: Identifier,
    ot: Type,
    stream: ExpressionArg,
    defVal: ExpressionArg,
    currSrc: SourceListing
  ): SourceListing = {
    val (s, _) = streamNameAndTypeFromExpressionArg(stream)
    val o = s"var_${id.fullName}"
    val default = nonStreamCodeGenerator.translateExpressionArg(defVal, nonStreamCodeGenerator.TypeArgManagement.empty)

    val newStmt = currSrc.stepSource
      .If(Seq(Seq(NotEqual("currTs", LongValue(0)))))
      .Assignment(s"${o}_changed", BoolValue(false), BoolValue(true), BoolType)
      .EndIf()
      .If(Seq(Seq(s"${s}_changed")))
      .Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      .Assignment(s"${o}_lastError", s"${o}_error", NoError, ErrorType)
      .Assignment(s"${o}_value", s"${s}_value", default, ot)
      .Assignment(s"${o}_init", BoolValue(true), BoolValue(true), BoolType)
      .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      .Assignment(s"${o}_error", s"${s}_error", NoError, ErrorType)
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(true), BoolType)
      .Assignment(s"${o}_unknown", s"${s}_unknown", BoolValue(false), BoolType)
      .EndIf()

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces ImpLan code for a x = defaultFrom(...) expression
   *
   * @param id The id defaultFrom is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param stream The stream parameter of the defaultFrom
   * @param default The stream parameter of the defaultFrom
   * @param currSrc The source listing the generated block is added to. It is attached to the
   *               stepSource section of the source listing
   * @return The modified source listing
   */
  private def produceDefaultFromStepCode(
    id: Identifier,
    ot: Type,
    stream: ExpressionArg,
    default: ExpressionArg,
    currSrc: SourceListing
  ): SourceListing = {
    val (s, _) = streamNameAndTypeFromExpressionArg(stream)
    val (d, _) = streamNameAndTypeFromExpressionArg(default)
    val o = s"var_${id.fullName}"

    val newStmt = currSrc.stepSource
      .Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      .If(Seq(Seq(s"${s}_changed")))
      .Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      .Assignment(s"${o}_lastError", s"${o}_error", NoError, ErrorType)
      .Assignment(s"${o}_value", s"${s}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_unknown", s"${s}_unknown", BoolValue(false), BoolType)
      .Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      .Assignment(s"${o}_error", s"${s}_error", NoError, ErrorType)
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .Else()
      .If(Seq(Seq(Negation(s"${o}_init"), s"${d}_init")))
      .Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      .Assignment(s"${o}_lastError", s"${o}_error", NoError, ErrorType)
      .Assignment(s"${o}_value", s"${d}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_unknown", s"${d}_unknown", BoolValue(false), BoolType)
      .Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      .Assignment(s"${o}_error", s"${d}_error", NoError, ErrorType)
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .EndIf()
      .EndIf()

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces ImpLan code for a x = time(...) expression
   *
   * @param id The id time is assigned to
   * @param stream The base-stream parameter of the time
   * @param currSrc The source listing the generated block is added to. It is attached to the
   *               stepSource section of the source listing
   * @return The modified source listing
   */
  private def produceTimeStepCode(id: Identifier, stream: ExpressionArg, currSrc: SourceListing): SourceListing = {

    val (s, _) = streamNameAndTypeFromExpressionArg(stream)
    val o = s"var_${id.fullName}"

    val newStmt = currSrc.stepSource
      .Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      .If(Seq(Seq(s"${s}_changed")))
      .Assignment(s"${o}_lastValue", s"${o}_value", LongValue(0), LongType)
      .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      .Assignment(s"${o}_lastError", s"${o}_error", NoError, ErrorType)
      .Assignment(s"${o}_value", s"${s}_ts", LongValue(0), LongType)
      .Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      .Assignment(s"${o}_unknown", s"${s}_unknown", BoolValue(false), BoolType)
      .If(Seq(Seq(s"${o}_unknown")))
      .Assignment(s"${o}_error", s"${s}_error", NoError, ErrorType)
      .Else()
      .Assignment(s"${o}_error", NoError, NoError, ErrorType)
      .EndIf()
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .EndIf()

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces ImpLan code for a x = last(...) expression
   *
   * @param id The id last is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param values The value-stream parameter of the last
   * @param clock The trigger/clock-stream parameter of the last
   * @param currSrc The source listing the generated block is added to. It is attached to the
   *               stepSource section of the source listing
   * @return The modified source listing
   */
  private def produceLastStepCode(
    id: Identifier,
    ot: Type,
    values: ExpressionArg,
    clock: ExpressionArg,
    currSrc: SourceListing
  ): SourceListing = {
    val (v, _) = streamNameAndTypeFromExpressionArg(values)
    val (c, _) = streamNameAndTypeFromExpressionArg(clock)
    val o = s"var_${id.fullName}"

    val newStmt = currSrc.stepSource
      .Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      .If(Seq(Seq(s"${c}_changed")))
      .If(Seq(Seq(Equal(s"${v}_ts", "currTs"), s"${v}_lastInit"), Seq(NotEqual(s"${v}_ts", "currTs"), s"${v}_init")))
      .Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      .Assignment(s"${o}_lastError", s"${o}_error", NoError, ErrorType)
      .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_unknown", s"${c}_unknown", BoolValue(false), BoolType)
      .If(Seq(Seq(s"${o}_unknown")))
      .Assignment(s"${o}_error", s"${c}_error", NoError, ErrorType)
      .Else()
      .If(Seq(Seq(Equal(s"${v}_ts", "currTs"))))
      .Assignment(s"${o}_value", s"${v}_lastValue", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_error", s"${v}_lastError", NoError, ErrorType)
      .Else()
      .Assignment(s"${o}_value", s"${v}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_error", s"${v}_error", NoError, ErrorType)
      .EndIf()
      .EndIf()
      .EndIf()
      .EndIf()

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces ImpLan code for a x = delay(...) expression
   *
   * @param id The id delay is assigned to
   * @param delay The delay-stream parameter of the delay
   * @param reset The reset-stream parameter of the delay
   * @param currSrc The source listing the generated block is added to.
   *                There is code attached to the tsGenSource and stepSource section.
   * @return The modified source listing
   */
  private def produceDelayStepCode(
    id: Identifier,
    delay: ExpressionArg,
    reset: ExpressionArg,
    currSrc: SourceListing
  ): SourceListing = {
    val (d, _) = streamNameAndTypeFromExpressionArg(delay)
    val (r, _) = streamNameAndTypeFromExpressionArg(reset)
    val o = s"var_${id.fullName}"

    val newStmt = currSrc.stepSource
      .Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      .If(Seq(Seq(Equal(s"${o}_nextTs", "currTs"))))
      .FinalAssignment(s"${o}_lastValue", UnitValue, UnitType)
      .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      .FinalAssignment(s"${o}_lastError", NoError, ErrorType)
      .FinalAssignment(s"${o}_value", UnitValue, UnitType)
      .Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .FinalAssignment(s"${o}_error", NoError, ErrorType)
      .Assignment(s"${o}_unknown", BoolValue(false), BoolValue(false), BoolType)
      .EndIf()

    val newTsGen = currSrc.tsGenSource
      .If(Seq(Seq(Greater(s"${o}_nextTs", "lastProcessedTs"), Greater("currTs", s"${o}_nextTs"))))
      .Assignment("currTs", s"${o}_nextTs", LongValue(0), LongType)
      .EndIf()

    val newTail = currSrc.tailSource
      .If(
        Seq(
          Seq(s"${r}_unknown"),
          Seq(NotEqual(s"${d}_error", NoError)),
          Seq(s"${d}_changed", GreaterEqual(LongValue(0), s"${d}_value"))
        )
      )
      .FunctionCall("__[TC]delayPanic__", Seq(), IntermediateCode.FunctionType(Seq(), UnitType))
      .Else()
      .If(Seq(Seq(s"${d}_changed")))
      .If(Seq(Seq(s"${o}_changed"), Seq(s"${r}_changed")))
      .Assignment(s"${o}_nextTs", Addition("currTs", s"${d}_value"), LongValue(-1), LongType)
      .EndIf()
      .Else()
      .If(Seq(Seq(s"${r}_changed")))
      .Assignment(s"${o}_nextTs", LongValue(-1), LongValue(-1), LongType)
      .EndIf()
      .EndIf()
      .EndIf()

    SourceListing(newStmt, newTail, newTsGen, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces ImpLan code for a x = lift(...) expression
   *
   * @param id The id lift is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param args The arguments of the lift except the last one (function)
   * @param function The lifted function
   * @param currSrc The source listing the generated block is added to.
   *                There is code attached to the stepSource section.
   * @return The modified source listing
   */
  private def produceLiftStepCode(
    id: Identifier,
    ot: Type,
    args: Seq[ExpressionArg],
    function: ExpressionArg,
    currSrc: SourceListing
  ): SourceListing = {
    val o = s"var_${id.fullName}"

    val coreExp = args.map { arg =>
      val argName = streamNameAndTypeFromExpressionArg(arg)._1
      TernaryExpression(
        Seq(Seq(Equal(s"${argName}_error", NoError))),
        s"${argName}_value",
        Throw(Variable(s"${argName}_error"), arg.tpe)
      )
    }
    val params = args.zip(coreExp).map {
      case (sr, exp) => {
        val (sName, sType) = streamNameAndTypeFromExpressionArg(sr)
        TernaryExpression(
          Seq(Seq(s"${sName}_changed")),
          FunctionCall("__Some__", Seq(exp), IntermediateCode.FunctionType(Seq(sType), OptionType(sType))),
          NoneValue(sType)
        )
      }
    }

    val guard: Seq[Seq[ImpLanExpr]] = args.map { sr =>
      Seq(Variable(s"${streamNameAndTypeFromExpressionArg(sr)._1}_changed"))
    }

    val newStmt = currSrc.stepSource
      .Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      .If(guard)
      .Assignment(s"${o}_error", NoError, NoError, ErrorType)
      .Try()
      .Assignment(s"${o}_unknown", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(
        s"${o}_fval",
        nonStreamCodeGenerator.translateFunctionCall(function, params, nonStreamCodeGenerator.TypeArgManagement.empty),
        None,
        OptionType(ot)
      )
      .Assignment(s"${o}_unknown", BoolValue(false), BoolValue(false), BoolType)
      .If(
        Seq(
          Seq(
            FunctionCall("__isSome__", Seq(s"${o}_fval"), IntermediateCode.FunctionType(Seq(OptionType(ot)), BoolType))
          )
        )
      )
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(
        s"${o}_newValue",
        FunctionCall("__getSome__", Seq(s"${o}_fval"), IntermediateCode.FunctionType(Seq(OptionType(ot)), ot)),
        defaultValueForStreamType(ot),
        ot
      )
      .EndIf()
      .Catch()
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_error", s"var_err", NoError, ErrorType)
      .EndTry()
      .If(
        Seq(Seq(s"${o}_unknown"), args.map { sr => Variable(s"${streamNameAndTypeFromExpressionArg(sr)._1}_unknown") })
      )
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_unknown", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(
        s"${o}_error",
        FunctionCall(
          "__[TC]UnknownEventError__",
          Seq(s"${o}_error"),
          IntermediateCode.FunctionType(Seq(ErrorType), ErrorType)
        ),
        NoError,
        ErrorType
      )
      .EndIf()
      .If(Seq(Seq(s"${o}_changed")))
      .Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      .Assignment(s"${o}_lastError", s"${o}_error", NoError, ErrorType)
      .Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      .Assignment(s"${o}_value", s"${o}_newValue", defaultValueForStreamType(ot), ot)
      .EndIf()
      .EndIf()
    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces ImpLan code for a x = slift(...) expression
   *
   * @param id The id slift is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param args The arguments of the lift except the last one (function)
   * @param function The lifted function
   * @param currSrc The source listing the generated block is added to.
   *                There is code attached to the stepSource section.
   * @return The modified source listing
   */
  private def produceSignalLiftStepCode(
    id: Identifier,
    ot: Type,
    args: Seq[ExpressionArg],
    function: ExpressionArg,
    currSrc: SourceListing
  ): SourceListing = {
    val o = s"var_${id.fullName}"

    val guard1: Seq[Seq[ImpLanExpr]] = Seq(args.map { arg =>
      Variable(s"${streamNameAndTypeFromExpressionArg(arg)._1}_init")
    })
    val guard2: Seq[Seq[ImpLanExpr]] = args.map { arg =>
      Seq(Variable(s"${streamNameAndTypeFromExpressionArg(arg)._1}_changed"))
    }

    val fargs = args.map { arg =>
      val argName = streamNameAndTypeFromExpressionArg(arg)._1
      TernaryExpression(
        Seq(Seq(Equal(s"${argName}_error", NoError))),
        s"${argName}_value",
        Throw(Variable(s"${argName}_error"), arg.tpe)
      )
    }

    val fcall =
      nonStreamCodeGenerator.translateFunctionCall(function, fargs, nonStreamCodeGenerator.TypeArgManagement.empty)
    val unknown: ImpLanExpr = And(args.map { sr => Variable(s"${streamNameAndTypeFromExpressionArg(sr)._1}_unknown") })

    val newStmt = currSrc.stepSource
      .Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
      .If(guard1)
      .If(guard2)
      .Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot)
      .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
      .Assignment(s"${o}_lastError", s"${o}_error", NoError, ErrorType)
      .Assignment(s"${o}_unknown", unknown, BoolValue(false), BoolType)
      .If(Seq(Seq(s"${o}_unknown")))
      .Assignment(
        s"${o}_error",
        FunctionCall(
          "__[TC]UnknownEventError__",
          Seq(NoError),
          IntermediateCode.FunctionType(Seq(ErrorType), ErrorType)
        ),
        NoError,
        ErrorType
      )
      .Else()
      .Assignment(s"${o}_error", NoError, NoError, ErrorType)
      .Try()
      .Assignment(s"${o}_value", fcall, defaultValueForStreamType(ot), ot)
      .Catch()
      .Assignment(s"${o}_error", s"var_err", NoError, ErrorType)
      .EndTry()
      .EndIf()
      .Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
      .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
      .EndIf()
      .EndIf()
    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces ImpLan code for a x = merge(...) expression
   *
   * @param id The id merge is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param args The stream expressions to be merged
   * @param currSrc The source listing the generated block is added to.
   *                There is code attached to the stepSource section.
   * @return The modified source listing
   */
  private def produceMergeStepCode(
    id: Identifier,
    ot: Type,
    args: Seq[ExpressionArg],
    currSrc: SourceListing
  ): SourceListing = {
    val o = s"var_${id.fullName}"

    val guard: Seq[Seq[ImpLanExpr]] = args.map { sr =>
      Seq(Variable(s"${streamNameAndTypeFromExpressionArg(sr)._1}_changed"))
    }
    val unknown: ImpLanExpr = And(args.map { sr => Variable(s"${streamNameAndTypeFromExpressionArg(sr)._1}_unknown") })

    var newStmt = (
      currSrc.stepSource
        .Assignment(s"${o}_changed", BoolValue(false), BoolValue(false), BoolType)
        .If(guard)
        .Assignment(s"${o}_lastValue", s"${o}_value", defaultValueForStreamType(ot), ot)
        .Assignment(s"${o}_lastInit", s"${o}_init", BoolValue(false), BoolType)
        .Assignment(s"${o}_lastError", s"${o}_error", NoError, ErrorType)
        .Assignment(s"${o}_init", BoolValue(true), BoolValue(false), BoolType)
        .Assignment(s"${o}_ts", "currTs", LongValue(0), LongType)
        .Assignment(s"${o}_changed", BoolValue(true), BoolValue(false), BoolType)
        .Assignment(
          s"${o}_unknown",
          unknown,
          BoolValue(false),
          BoolType
        )
      )

    args.foreach { arg =>
      val (sn, _) = streamNameAndTypeFromExpressionArg(arg)
      newStmt = (newStmt
        .If(Seq(Seq(s"${sn}_changed")))
        .Assignment(s"${o}_value", s"${sn}_value", defaultValueForStreamType(ot), ot)
        .Assignment(s"${o}_error", s"${sn}_error", NoError, ErrorType)
        .Else())
    }

    (1 to args.length + 1).foreach { _ => newStmt = newStmt.EndIf() }

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Add code for output generation (calling of __[TC]output__) to the source listing.
   * __[TC]output__ gets value, error, timestamp passed and if the printing format is raw (i.e. only value, not the
   * current timestamp) and has to be translated accordingly in the final code generation
   *
    * @param id The id of the stream to be printed
   * @param t id's type. Must be Events[...]
   * @param nameOpt The alias name of id for printing. Optional.
   * @param raw If the output should be printed raw (without timestamp). Is passed to __[TC]output__.
   * @param currSrc The source listing the generated block is added to.
   *                There is code attached to the tailSource section.
   * @return The modified source listing
   */
  def produceOutputCode(
    id: Identifier,
    t: Type,
    nameOpt: Option[String],
    currSrc: SourceListing,
    raw: Boolean
  ): SourceListing = {
    val s = s"var_${id.fullName}"
    val name = nameOpt.getOrElse(id.idOrName.left.getOrElse(id.fullName))

    val newTail = currSrc.tailSource
      .If(Seq(Seq(s"${s}_changed")))
      .FunctionCall(
        "__[TC]output__",
        Seq(s"${s}_value", StringValue(name), s"${s}_error", "currTs", BoolValue(raw)),
        IntermediateCode.FunctionType(Seq(t, StringType, ErrorType, LongType, BoolType), UnitType)
      )
      .EndIf()

    SourceListing(currSrc.stepSource, newTail, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces code resetting the changed flags of input variables.
   *
    * @param inStream The inStream to be reseted.
   * @param currSrc  The source listing the generated block is added to.
   *                 There is code attached to the tailSource section.
   * @return The modified source listing
   */
  def produceInputUnchangeCode(inStream: Identifier, currSrc: SourceListing): SourceListing = {
    val s = s"var_${inStream.fullName}"

    val newTail =
      currSrc.tailSource.Assignment(s"${s}_changed", BoolValue(false), BoolValue(false), BoolType)

    SourceListing(currSrc.stepSource, newTail, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces code reading the std input (variable inputStream and value) and passes it to the _value variable
   * of an input stream. For parsing the input string to an exact value a function __[TC]inputParse__ is called
   * which has to be translated in the following phases.
   *
   * @param inStream The input stream to be handled
   * @param typ      The input stream's type. Must be Events[...].
   * @param currSrc  The source listing the generated block is added to.
   *                 There is code attached to the inputProcessing section.
   * @return The modified source listing
   */
  def produceInputFromConsoleCode(inStream: Identifier, typ: Type, currSrc: SourceListing): SourceListing = {
    val s = s"var_${inStream.fullName}"
    val parseExp = typ match {
      case InstantiatedType("Events", Seq(RecordType(m, _)), _) if m.isEmpty => UnitValue
      case _                                                                 => FunctionCall("__[TC]inputParse__", Seq("value"), IntermediateCode.FunctionType(Seq(StringType), typ))
    }

    val newInputProcessing = currSrc.inputProcessing
      .If(Seq(Seq(Equal("inputStream", StringValue(inStream.idOrName.left.get)))))
      .Assignment(s"${s}_lastValue", s"${s}_value", defaultValueForStreamType(typ), typ)
      .Assignment(s"${s}_lastInit", s"${s}_init", BoolValue(false), BoolType)
      .FinalAssignment(s"${s}_lastError", NoError, ErrorType)
      .Assignment(s"${s}_value", parseExp, defaultValueForStreamType(typ), typ)
      .Assignment(s"${s}_init", BoolValue(true), BoolValue(false), BoolType)
      .Assignment(s"${s}_ts", "currTs", LongValue(0), LongType)
      .FinalAssignment(s"${s}_error", NoError, ErrorType)
      .Assignment(s"${s}_changed", BoolValue(true), BoolValue(false), BoolType)
      .FinalAssignment(s"${s}_unknown", BoolValue(false), BoolType)
      .EndIf()

    SourceListing(currSrc.stepSource, currSrc.tailSource, currSrc.tsGenSource, newInputProcessing, currSrc.staticSource)
  }

}
package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{BoolType, StringType, UnitType, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils._

import scala.language.{implicitConversions, postfixOps}

/**
 * Class containing functions for the translation of single TeSSLa stream expressions
 * to a bare minimum code expression
 */

class SimpleStreamCodeGenerator(nonStreamCodeGenerator: NonStreamCodeGenerator) {

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
        produceNilCode(id, typ.resultType.resolve(typeParamMap), currSource)
      case "default" =>
        produceDefaultCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
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
        produceMergeCode(id, typ.resultType.resolve(typeParamMap), args, currSource)
      case "count" =>
        produceCountStepCode(id, args(0), currSource)
      case "const" =>
        produceConstStepCode(id, typ.resultType.resolve(typeParamMap), args, currSource)
      case "filter" =>
        produceFilterStepCode(id, typ.resultType.resolve(typeParamMap), args, currSource)
      case "fold" =>
        produceFoldStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), args(2), currSource)
      case "reduce" =>
        produceReduceStepCode(id, typ.resultType.resolve(typeParamMap), args(0), args(1), currSource)
      case "unitIf" =>
        produceUnitIfStepCode(id, args(0), currSource)
      case "pure" =>
        producePureStepCode(id, typ.resultType.resolve(typeParamMap), args(0), currSource)
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
   * Produces code required for a x = nil expression
   *
   * @param id The id nil is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param currSrc The source listing the generated block is added to. It is attached to the
   *               stepSource section of the source listing
   * @return The modified source listing
   */
  private def produceNilCode(id: Identifier, ot: Type, currSrc: SourceListing): SourceListing = {
    val o = s"var_${id.fullName}"
    val default = defaultValueForStreamType(ot)

    val newStmt = currSrc.stepSource
      .FinalAssignment(o, StreamValue(ot, default), StreamType(ot))

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces code required for a x = default(...) expression
   *
   * @param id The id default is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param stream The stream parameter of the default
   * @param defVal The default parameter of the default
   * @param currSrc The source listing the generated block is added to. It is attached to the
   *               stepSource section of the source listing
   * @return The modified source listing
   */
  private def produceDefaultCode(
    id: Identifier,
    ot: Type,
    stream: ExpressionArg,
    defVal: ExpressionArg,
    currSrc: SourceListing
  ): SourceListing = {
    val (s, st) = streamNameAndTypeFromExpressionArg(stream)
    val o = s"var_${id.fullName}"
    val default = nonStreamCodeGenerator.translateExpressionArg(defVal, nonStreamCodeGenerator.TypeArgManagement.empty)

    val newStmt = currSrc.stepSource
      .FinalAssignment(o, StreamValue(ot, default), StreamType(ot))
      .If(Seq(Seq(FunctionCall("__[Stream]hasChanged__", Seq(s), FunctionType(Seq(StreamType(st)), BoolType)))))
      .FunctionCall("__[Stream]setValue__", Seq(s, o), FunctionType(Seq(StreamType(st), StreamType(ot)), UnitType))
      .EndIf()

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }

  /**
   * Produces code required for a x = merge(...) expression
   *
   * @param id The id merge is assigned to
   * @param ot The type of id. Must be Events[...]
   * @param args The stream expressions to be merged
   * @param currSrc The source listing the generated block is added to.
   *                There is code attached to the stepSource section.
   * @return The modified source listing
   */
  private def produceMergeCode(
    id: Identifier,
    ot: Type,
    args: Seq[ExpressionArg],
    currSrc: SourceListing
  ): SourceListing = {
    val o = s"var_${id.fullName}"
    val default = defaultValueForStreamType(ot)

    val guard: Seq[Seq[ImpLanExpr]] = args.map { sr =>
      val (s, st) = streamNameAndTypeFromExpressionArg(sr)
      Seq(FunctionCall("__[Stream]hasChanged__", Seq(Variable(s)), FunctionType(Seq(StreamType(st)), BoolType)))
    }

    var newStmt = currSrc.stepSource
      .Assignment(o, StreamValue(ot, None), StreamValue(ot, default), StreamType(ot))
      .FunctionCall("__[Stream]updateLast__", Seq(Variable(o)), FunctionType(Seq(StreamType(ot)), UnitType))
      .If(guard) // TODO technically not needed?
      .FunctionCall(
        "__[Stream]merge__",
        Seq(ImmutableList(ot, args), o),
        FunctionType(Seq(ImmutableListType(StreamType(ot)), StreamType(ot)), UnitType)
      )
      .EndIf()

    SourceListing(newStmt, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource)
  }
}

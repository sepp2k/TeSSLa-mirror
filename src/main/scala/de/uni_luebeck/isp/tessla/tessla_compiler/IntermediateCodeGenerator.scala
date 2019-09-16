package de.uni_luebeck.isp.tessla.tessla_compiler

import scala.language.implicitConversions
import scala.language.postfixOps

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeGenerator.IntermediateCodeDSL
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.Tessla.{BuiltInType, ObjectType}
import de.uni_luebeck.isp.tessla.TesslaCore.{BoolValue => _, _}


/**
  * This class contains functions for the translation of single TeSSLa expressions to imperative code
  */
object IntermediateCodeGenerator {

  sealed trait IfState
  final case object InIf extends IfState
  final case object InElse extends IfState
  final case object Out extends IfState

  implicit def typeConversion(t : ValueType) : ImpLanType = {
    t match {

      case TesslaCore.FunctionType => throw new Errors.CommandNotSupportedError("Function type without further type knowledge cannot be translated into imperative code")
      case TesslaCore.ObjectType(memberTypes) => throw new Errors.NotYetImplementedError("Object types cannot be translated into imperative code yet")
    }
  }

  implicit def stringToVariable(str: String) : Variable = {
    Variable(str)
  }

  implicit def intermediateCodeDSLConversion(stmts: Seq[ImpLanStmt]) : IntermediateCodeDSL = {
    new IntermediateCodeDSL(stmts)
  }

  implicit def stmtConversion(dsl : IntermediateCodeDSL) : Seq[ImpLanStmt] = {
    dsl.generateStatements
  }

  class IntermediateCodeDSL(stmts: Seq[ImpLanStmt], ifState : Seq[IfState] = Seq(Out),
                            ifStack: Seq[Seq[ImpLanStmt]] = Seq(), elseStack: Seq[Seq[ImpLanStmt]] = Seq(),
                            condStack: Seq[Set[Set[ImpLanExpr]]] = Seq()) {


    def generateStatements: Seq[ImpLanStmt] = {
      if (ifState(0) != Out) {
        throw new Errors.DSLError("At least one unclosed If")
      }

      if (ifStack.length != 0 || elseStack.length != 0 || condStack.length != 0) {
        throw new Errors.DSLError("Stack sizes are not valid")
      }
      stmts
    }

    def addStmt(stmt: ImpLanStmt) : (Seq[ImpLanStmt], Seq[Seq[ImpLanStmt]], Seq[Seq[ImpLanStmt]]) = ifState(0) match {
      case InIf => (stmts, ifStack.updated(0, ifStack(0) :+ stmt), elseStack)
      case InElse => (stmts, ifStack, elseStack.updated(0, elseStack(0) :+ stmt))
      case Out => (stmts :+ stmt, ifStack, elseStack)
    }

    def Assignment(lhs: Variable, rhs: ImpLanExpr, default: ImpLanVal, typ: ImpLanType) : IntermediateCodeDSL = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.Assignment(lhs, rhs, default, typ))
      new IntermediateCodeDSL(newStmts, ifState, newIfStack, newElseStack, condStack)
    }

    def FinalAssignment(lhs: Variable, default: ImpLanVal, typ: ImpLanType) : IntermediateCodeDSL = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FinalAssignment(lhs, default, typ))
      new IntermediateCodeDSL(newStmts, ifState, newIfStack, newElseStack, condStack)
    }

    def If(cond : Set[Set[ImpLanExpr]]) : IntermediateCodeDSL = {
      new IntermediateCodeDSL(stmts, Seq(InIf) ++ ifState, Seq(Seq()) ++ ifStack, Seq(Seq()) ++ elseStack,
                              Seq(cond) ++ condStack)
    }

    def Else : IntermediateCodeDSL = ifState(0) match {
      case InIf => new IntermediateCodeDSL(stmts, ifState.updated(0, InElse), ifStack, elseStack, condStack)
      case InElse => throw new Errors.DSLError("Two subseqeuent Else blocks")
      case Out => throw new Errors.DSLError("Else without previous If")
    }

    def EndIf : IntermediateCodeDSL = {
      val stmt = ifState(0) match {
        case InIf | InElse =>  IntermediateCode.If(condStack(0), ifStack(0), elseStack(0))
        case Out => throw new Errors.DSLError("EndIf without previous If")
      }
      val tmpRes = new IntermediateCodeDSL(stmts, ifState.drop(1), ifStack.drop(1), elseStack.drop(1), condStack.drop(1))
      val (newStmts, newIfStack, newElseStack) = tmpRes.addStmt(stmt)
      new IntermediateCodeDSL(newStmts, ifState.drop(1), newIfStack, newElseStack, condStack.drop(1))
    }

    def FunctionCall(name: String, params: Seq[ImpLanExpr]) : IntermediateCodeDSL = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FunctionCall(name, params))
      new IntermediateCodeDSL(newStmts, ifState, newIfStack, newElseStack, condStack)
    }

  }

  def produceDefaultStepCode(outStream: Stream, stream: StreamRef, default: ValueOrError, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc) //Why??
  }

  def produceDefaultFromStepCode(outStream: Stream, stream: StreamRef, defaultStream: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc) //Why??
  }

  def produceTimeStepCode(outStream: Stream, stream: StreamRef, loc: Location, currSrc: SourceListing) : SourceListing = {
  }

  def produceLastStepCode(outStream: Stream, values: StreamRef, clock: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc)
  }

  def produceDelayStepCode(outStream: Stream, delay: StreamRef, reset: StreamRef, loc: Location, currSrc: SourceListing): SourceListing = {
    throw new Errors.NotYetImplementedError("", loc)
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

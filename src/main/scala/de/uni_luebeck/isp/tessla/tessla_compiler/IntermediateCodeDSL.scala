package de.uni_luebeck.isp.tessla.tessla_compiler

import scala.language.implicitConversions
import scala.language.postfixOps
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{StringValue, _}
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.TesslaCore.{BoolValue => _, StringValue => _, _}

/**
  * Class containing a DSL for easy creation of ImpLanStmt-Blocks
  */
object IntermediateCodeDSL {

  implicit def valueConversion(v: ValueOrError): ImpLanVal = {
    v match {
      case TesslaCore.BoolValue(b, _) => BoolValue(b)
      case TesslaCore.IntValue(i, _) => LongValue(i.longValue())
      case TesslaCore.FloatValue(f, _) => DoubleValue(f)
      case TesslaCore.StringValue(s, _) => StringValue(s)
      case TesslaCore.TesslaOption(scala.None, t, _) => None(t)
      case TesslaCore.TesslaOption(v, _, _) => Some(v.get)
      case TesslaCore.TesslaMap(m, TesslaCore.BuiltInType("Map", Seq(t1, t2)), loc) =>
        if (m.size == 0)
          EmptyImmutableMap(t1, t2)
        else
          throw new Errors.NotYetImplementedError(s"Translation of non empty maps into imperative code is not implemented yet", loc)
      case TesslaCore.TesslaSet(s, TesslaCore.BuiltInType("Set", Seq(t)), loc) =>
        if (s.size == 0)
          EmptyImmutableSet(t)
        else
          throw new Errors.NotYetImplementedError(s"Translation of non empty sets into imperative code is not implemented yet", loc)
      case TesslaCore.TesslaList(IndexedSeq(), TesslaCore.BuiltInType("List", Seq(t)), _) => EmptyImmutableList(t)
      case Error(error) => throw new Errors.CoreASTError(error, error.loc)
      case other => throw new Errors.NotYetImplementedError(s"Translation of $other into imperative code not implemented yet")
      //FIXME: Unit value ?!
    }
  }

  implicit def typeConversion(t: ValueType): ImpLanType = {
    t match {
      case TesslaCore.BuiltInType("Unit", Seq()) => BoolType
      case TesslaCore.BuiltInType("Bool", Seq()) => BoolType
      case TesslaCore.BuiltInType("Int", Seq()) => LongType
      case TesslaCore.BuiltInType("Float", Seq()) => DoubleType
      case TesslaCore.BuiltInType("String", Seq()) => StringType
      case TesslaCore.BuiltInType("Option", Seq(t)) => OptionType(t)
      case TesslaCore.BuiltInType("Set", Seq(t)) => ImmutableSetType(t)
      case TesslaCore.BuiltInType("Map", Seq(t1, t2)) => ImmutableMapType(t1, t2)
      case TesslaCore.BuiltInType("List", Seq(t)) => ImmutableListType(t)
      case t: TesslaCore.BuiltInType => throw new Errors.NotYetImplementedError(s"BuiltInType $t cannot be translated into imperative code yet")
      case TesslaCore.FunctionType => throw new Errors.CommandNotSupportedError("Function type without further type knowledge cannot be translated into imperative code")
      case TesslaCore.ObjectType(memberTypes) => if (memberTypes.isEmpty) {
        BoolType
      } else {
        throw new Errors.NotYetImplementedError("Object types cannot be translated into imperative code yet")
      }
    }
  }

  def defaultValueForType(t: ValueType): ImpLanVal = {
    t match {
      case TesslaCore.BuiltInType("Unit", Seq()) => BoolValue(true)
      case TesslaCore.BuiltInType("Bool", Seq()) => BoolValue(false)
      case TesslaCore.BuiltInType("Int", Seq()) => LongValue(0)
      case TesslaCore.BuiltInType("Float", Seq()) => DoubleValue(0)
      case TesslaCore.BuiltInType("String", Seq()) => StringValue("")
      case TesslaCore.BuiltInType("Option", Seq(t)) => None(t)
      case TesslaCore.BuiltInType("Set", Seq(t)) => EmptyImmutableSet(t)
      case TesslaCore.BuiltInType("Map", Seq(t1, t2)) => EmptyImmutableMap(t1, t2)
      case TesslaCore.BuiltInType("List", Seq(t)) => EmptyImmutableList(t)
      case t: TesslaCore.BuiltInType => throw new Errors.NotYetImplementedError(s"BuiltInType $t cannot be translated into imperative code yet")
      case TesslaCore.FunctionType => throw new Errors.CommandNotSupportedError("Function type without further type knowledge cannot be translated into imperative code")
      case TesslaCore.ObjectType(memberTypes) => if (memberTypes.isEmpty) {
        BoolValue(true)
      } else {
        throw new Errors.NotYetImplementedError("Object types cannot be translated into imperative code yet")
      }
    }
  }

  implicit def stringToVariable(str: String): Variable = {
    Variable(str)
  }

  implicit def intermediateCodeDSLConversion(stmts: Seq[ImpLanStmt]): IntermediateCodeDSL = {
    new IntermediateCodeDSL(stmts)
  }

  implicit def stmtConversion(dsl: IntermediateCodeDSL): Seq[ImpLanStmt] = {
    dsl.generateStatements
  }
}

sealed trait IfState

final case object InIf extends IfState
final case object InElse extends IfState
final case object Out extends IfState

class IntermediateCodeDSL(stmts: Seq[ImpLanStmt], ifState : Seq[IfState] = Seq(Out),
                          ifStack: Seq[Seq[ImpLanStmt]] = Seq(), elseStack: Seq[Seq[ImpLanStmt]] = Seq(),
                          condStack: Seq[Seq[Seq[ImpLanExpr]]] = Seq()) {

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

    def If(cond : Seq[Seq[ImpLanExpr]]) : IntermediateCodeDSL = {
      new IntermediateCodeDSL(stmts, Seq(InIf) ++ ifState, Seq(Seq()) ++ ifStack, Seq(Seq()) ++ elseStack,
        Seq(cond) ++ condStack)
    }

    def Else() : IntermediateCodeDSL = ifState(0) match {
      case InIf => new IntermediateCodeDSL(stmts, ifState.updated(0, InElse), ifStack, elseStack, condStack)
      case InElse => throw new Errors.DSLError("Two subseqeuent Else blocks")
      case Out => throw new Errors.DSLError("Else without previous If")
    }

    def EndIf() : IntermediateCodeDSL = {
      val stmt = ifState(0) match {
        case InIf | InElse =>  IntermediateCode.If(condStack(0), ifStack(0), elseStack(0))
        case Out => throw new Errors.DSLError("EndIf without previous If")
      }
      val tmpRes = new IntermediateCodeDSL(stmts, ifState.drop(1), ifStack.drop(1), elseStack.drop(1), condStack.drop(1))
      val (newStmts, newIfStack, newElseStack) = tmpRes.addStmt(stmt)
      new IntermediateCodeDSL(newStmts, ifState.drop(1), newIfStack, newElseStack, condStack.drop(1))
    }

    def FunctionCall(name: String, params: Seq[ImpLanExpr], typeHint: FunctionType) : IntermediateCodeDSL = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FunctionCall(name, params, typeHint))
      new IntermediateCodeDSL(newStmts, ifState, newIfStack, newElseStack, condStack)
    }

   def FunctionVarApplication(name: String, params: Seq[ImpLanExpr]) : IntermediateCodeDSL = {
    val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FunctionVarApplication(Variable(name), params))
    new IntermediateCodeDSL(newStmts, ifState, newIfStack, newElseStack, condStack)
   }

  }

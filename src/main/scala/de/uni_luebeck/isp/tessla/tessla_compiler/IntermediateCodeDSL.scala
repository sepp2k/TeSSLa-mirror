package de.uni_luebeck.isp.tessla.tessla_compiler

import scala.language.implicitConversions
import scala.language.postfixOps
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{BoolType, StringType, UnitType, _}
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.TesslaAST.Core._

/**
  * Class containing a DSL for easy creation of ImpLanStmt-Blocks
  */
object IntermediateCodeDSL {

//  implicit def valueConversion(v: ValueOrError): ImpLanVal = {
//    v match {
//      case TesslaCore.BoolValue(b, _) => BoolValue(b)
//      case TesslaCore.IntValue(i, _) => LongValue(i.longValue())
//      case TesslaCore.FloatValue(f, _) => DoubleValue(f)
//      case TesslaCore.StringValue(s, _) => StringValue(s)
//      case TesslaCore.TesslaOption(scala.None, t, _) => None(t)
//      case TesslaCore.TesslaOption(v, _, _) => Some(v.get)
//      case TesslaCore.TesslaMap(m, TesslaCore.BuiltInType("Map", Seq(t1, t2)), loc) =>
//        if (m.size == 0)
//          EmptyImmutableMap(t1, t2)
//        else
//          throw new Errors.NotYetImplementedError(s"Translation of non empty maps into imperative code is not implemented yet", loc)
//      case TesslaCore.TesslaSet(s, TesslaCore.BuiltInType("Set", Seq(t)), loc) =>
//        if (s.size == 0)
//          EmptyImmutableSet(t)
//        else
//          throw new Errors.NotYetImplementedError(s"Translation of non empty sets into imperative code is not implemented yet", loc)
//      case TesslaCore.TesslaList(IndexedSeq(), TesslaCore.BuiltInType("List", Seq(t)), _) => EmptyImmutableList(t)
//      case Error(error) => throw new Errors.CoreASTError(error, error.loc)
//      case other => throw new Errors.NotYetImplementedError(s"Translation of $other into imperative code not implemented yet")
//      //FIXME: Unit value ?!
//    }
//  }

  implicit def typeConversion(t: Type): ImpLanType = {
    t match {
      case InstatiatedType("Unit", Seq(), _) => UnitType //TODO: Do they exist at all under these names
      case InstatiatedType("Bool", Seq(), _) => BoolType
      case InstatiatedType("Int", Seq(), _) => LongType
      case InstatiatedType("Float", Seq(), _) => DoubleType
      case InstatiatedType("String", Seq(), _) => StringType
      case InstatiatedType("Option", Seq(t), _) => OptionType(t)
      case InstatiatedType("Set", Seq(t), _) => ImmutableSetType(t)
      case InstatiatedType("Map", Seq(t1, t2), _) => ImmutableMapType(t1, t2)
      case InstatiatedType("List", Seq(t), _) => ImmutableListType(t)
      case i: InstatiatedType => throw tessla_compiler.Errors.CommandNotSupportedError(s"Type translation for type $i not supported")
      case TypeParam(name, location) => throw tessla_compiler.Errors.TranslationError(s"Unknown type param $name cannot be used to gain default value")
      case RecordType(entries, location) => throw tessla_compiler.Errors.NotYetImplementedError("Record types not supported yet")
      case TesslaAST.Core.FunctionType(typeParams, paramTypes, resultType, location) => throw tessla_compiler.Errors.NotYetImplementedError("Default value for function types not supported yet")
      case _ => throw tessla_compiler.Errors.CommandNotSupportedError(s"Type translation for type $t not supported")
    }
  }

  def defaultValueForType(t: Type): ImpLanVal = {
    t match {
      case InstatiatedType("Unit", Seq(), _) => UnitValue //TODO: Do they exist at all under these names
      case InstatiatedType("Bool", Seq(), _) => BoolValue(false)
      case InstatiatedType("Int", Seq(), _) => LongValue(0)
      case InstatiatedType("Float", Seq(), _) => DoubleValue(0)
      case InstatiatedType("String", Seq(), _) => StringValue("")
      case InstatiatedType("Option", Seq(t), _) => None(t)
      case InstatiatedType("Set", Seq(t), _) => EmptyImmutableSet(t)
      case InstatiatedType("Map", Seq(t1, t2), _) => EmptyImmutableMap(t1, t2)
      case InstatiatedType("List", Seq(t), _) => EmptyImmutableList(t)
      case i: InstatiatedType => throw tessla_compiler.Errors.CommandNotSupportedError(s"Default value for type $i not supported")
      case TypeParam(name, location) => throw tessla_compiler.Errors.TranslationError(s"Unknown type param $name cannot be used to gain default value")
      case RecordType(entries, location) => throw tessla_compiler.Errors.NotYetImplementedError("Record types not supported yet")
      case TesslaAST.Core.FunctionType(typeParams, paramTypes, resultType, location) => throw tessla_compiler.Errors.NotYetImplementedError("Default value for function types not supported yet")
      case _ => throw tessla_compiler.Errors.CommandNotSupportedError(s"Default value for type $t not supported")
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
        throw tessla_compiler.Errors.DSLError("At least one unclosed If")
      }

      if (ifStack.length != 0 || elseStack.length != 0 || condStack.length != 0) {
        throw tessla_compiler.Errors.DSLError("Stack sizes are not valid")
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
      case InElse => throw tessla_compiler.Errors.DSLError("Two subseqeuent Else blocks")
      case Out => throw tessla_compiler.Errors.DSLError("Else without previous If")
    }

    def EndIf() : IntermediateCodeDSL = {
      val stmt = ifState(0) match {
        case InIf | InElse =>  IntermediateCode.If(condStack(0), ifStack(0), elseStack(0))
        case Out => throw tessla_compiler.Errors.DSLError("EndIf without previous If")
      }
      val tmpRes = new IntermediateCodeDSL(stmts, ifState.drop(1), ifStack.drop(1), elseStack.drop(1), condStack.drop(1))
      val (newStmts, newIfStack, newElseStack) = tmpRes.addStmt(stmt)
      new IntermediateCodeDSL(newStmts, ifState.drop(1), newIfStack, newElseStack, condStack.drop(1))
    }

    def FunctionCall(name: String, params: Seq[ImpLanExpr], typeHint: IntermediateCode.FunctionType) : IntermediateCodeDSL = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FunctionCall(name, params, typeHint))
      new IntermediateCodeDSL(newStmts, ifState, newIfStack, newElseStack, condStack)
    }

   def FunctionVarApplication(name: String, params: Seq[ImpLanExpr]) : IntermediateCodeDSL = {
    val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FunctionVarApplication(Variable(name), params))
    new IntermediateCodeDSL(newStmts, ifState, newIfStack, newElseStack, condStack)
   }

  }

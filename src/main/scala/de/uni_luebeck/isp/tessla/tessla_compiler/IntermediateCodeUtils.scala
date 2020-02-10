package de.uni_luebeck.isp.tessla.tessla_compiler

import scala.language.implicitConversions
import scala.language.postfixOps
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{BoolType, StringType, UnitType, _}
import de.uni_luebeck.isp.tessla._
import de.uni_luebeck.isp.tessla.TesslaAST.Core._

/**
  * Class containing a DSL for easy creation of ImpLanStmt-Blocks
  */
object IntermediateCodeUtils {

  def expressionFold[A](exp: ImpLanExpr, n: A, f: (A, ImpLanExpr) => A): A = {
    val subExp: Seq[ImpLanExpr] = exp match {
      case v: ImpLanVal => v match {
        case Some(content) => Seq(content)
        case _ => Seq()
      }
      case CastingExpression(e, _) =>  Seq(e)
      case FunctionCall(_, params, _) => params
      case LambdaApplication(exp, params) => params :+ exp
      case Addition(op1, op2) => Seq(op1, op2)
      case Subtraction(op1, op2) => Seq(op1, op2)
      case BitwiseOr(op1, op2) => Seq(op1, op2)
      case TernaryExpression(guard, e1, e2) => guard.flatten :+ e1 :+ e2
      case Equal(a, b) => Seq(a, b)
      case NotEqual(a, b) => Seq(a, b)
      case Greater(a, b) => Seq(a, b)
      case GreaterEqual(a, b) => Seq(a, b)
      case Negation(a) => Seq(a)
      case LambdaExpression(_, _, _, body) => extractExpressions(body)
      case _ => Seq()
    }

    subExp.foldLeft[A](f(n, exp)) { case (n, p) => expressionFold(p, n, f)}
  }

  def extractExpressions(stmts: Seq[ImpLanStmt]): Seq[ImpLanExpr] = {
    stmts.foldLeft[Seq[ImpLanExpr]](Seq()){case (exps, stmt) => exps ++ extractExpressions(stmt)}
  }

  def extractExpressions(stmt: ImpLanStmt): Seq[ImpLanExpr] = {
    stmt match {
      case expr: ImpLanExpr => Seq(expr)
      case If(guard, stmts, elseStmts) => guard.flatten ++ extractExpressions(stmts) ++ extractExpressions(elseStmts)
      case Assignment(_, rexpr, _, _) => Seq(rexpr)
      case FinalAssignment(_, defVal, _) => Seq(defVal)
      case ReturnStatement(expr) => Seq(expr)
    }
  }

  def getVariableMap(stmts: Seq[ImpLanStmt], baseMap: Map[String, (ImpLanType, Option[ImpLanExpr])] = Map()) : Map[String, (ImpLanType, Option[ImpLanExpr])] = {

    def extractAssignments(stmt: ImpLanStmt) : Seq[(String, ImpLanType, Option[ImpLanExpr])] = stmt match {
      case Assignment(lhs, _, defVal, typ) => Seq((lhs.name, typ, defVal))
      case FinalAssignment(lhs, defVal, typ) => Seq((lhs.name, typ, scala.Some(defVal)))
      case If(_, stmts, elseStmts) => stmts.concat(elseStmts).flatMap(extractAssignments)
      case _ => Seq()
    }

    val varDefs : Seq[(String, ImpLanType, Option[ImpLanExpr])] = baseMap.toSeq.map{case (a, (b,c)) => (a,b,c)} ++ stmts.flatMap(extractAssignments).distinct
    val duplicates = varDefs.groupBy{case (n, _, _) => n}.collect{case (x, List(_,_,_*)) => x}

    if (duplicates.nonEmpty) {
      throw tessla_compiler.Errors.TranslationError(s"Variable(s) with unsound type/default information: ${duplicates.mkString(", ")}")
    }

    varDefs.map{case (name, typ, default) => (name, (typ, default))}.toMap
  }

  implicit def typeConversion(t: Type): ImpLanType = {
    t match {
      case InstatiatedType("Events", Seq(t), _) => typeConversion(t) //TODO: Dirty hack
      case RecordType(entries, _) if entries.isEmpty => UnitType
      case InstatiatedType("Bool", Seq(), _) => BoolType
      case InstatiatedType("Int", Seq(), _) => LongType
      case InstatiatedType("Float", Seq(), _) => DoubleType
      case InstatiatedType("String", Seq(), _) => StringType
      case InstatiatedType("Option", Seq(t), _) => OptionType(t)
      case InstatiatedType("Set", Seq(t), _) => ImmutableSetType(t)
      case InstatiatedType("Map", Seq(t1, t2), _) => ImmutableMapType(t1, t2)
      case InstatiatedType("List", Seq(t), _) => ImmutableListType(t)
      case TesslaAST.Core.FunctionType(_, paramTypes, resultType, _) => IntermediateCode.FunctionType(paramTypes.map{case (_,t) => typeConversion(t)}, typeConversion(resultType)) //TODO: Type params
      case TypeParam(name, location) => GeneralType//TODO: Resolve type params if possible
      case i: InstatiatedType => throw tessla_compiler.Errors.CommandNotSupportedError(s"Type translation for type $i not supported")
      case RecordType(entries, location) => throw tessla_compiler.Errors.NotYetImplementedError("Record types not supported yet")
      case _ => throw tessla_compiler.Errors.CommandNotSupportedError(s"Type translation for type $t not supported")
    }
  }

  def defaultValueForType(t: Type): ImpLanVal = {
    t match {
      case InstatiatedType("Events", Seq(t), _) => defaultValueForType(t) //TODO: Dirty hack
      case RecordType(entries, _) if entries.isEmpty => UnitValue
      case InstatiatedType("Bool", Seq(), _) => BoolValue(false)
      case InstatiatedType("Int", Seq(), _) => LongValue(0)
      case InstatiatedType("Float", Seq(), _) => DoubleValue(0)
      case InstatiatedType("String", Seq(), _) => StringValue("")
      case InstatiatedType("Option", Seq(t), _) => None(t)
      case InstatiatedType("Set", Seq(t), _) => EmptyImmutableSet(t)
      case InstatiatedType("Map", Seq(t1, t2), _) => EmptyImmutableMap(t1, t2)
      case InstatiatedType("List", Seq(t), _) => EmptyImmutableList(t)
      case TesslaAST.Core.FunctionType(_, _, _, _) => EmptyFunction(t)
      case TypeParam(name, location) => throw tessla_compiler.Errors.TranslationError(s"Unknown type param $name cannot be used to gain default value")
      case i: InstatiatedType => throw tessla_compiler.Errors.CommandNotSupportedError(s"Default value for type $i not supported")
      case RecordType(entries, location) => throw tessla_compiler.Errors.NotYetImplementedError("Record types not supported yet")
      case _ => throw tessla_compiler.Errors.CommandNotSupportedError(s"Default value for type $t not supported")
    }
  }

  implicit def stringToVariable(str: String): Variable = {
    Variable(str)
  }

  implicit def intermediateCodeDSLConversion(stmts: Seq[ImpLanStmt]): IntermediateCodeUtils = {
    new IntermediateCodeUtils(stmts)
  }

  implicit def stmtConversion(dsl: IntermediateCodeUtils): Seq[ImpLanStmt] = {
    dsl.generateStatements
  }
}

sealed trait IfState

final case object InIf extends IfState
final case object InElse extends IfState
final case object Out extends IfState

class IntermediateCodeUtils(stmts: Seq[ImpLanStmt], ifState : Seq[IfState] = Seq(Out),
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

    def Assignment(lhs: Variable, rhs: ImpLanExpr, default: ImpLanExpr, typ: ImpLanType) : IntermediateCodeUtils = {
      Assignment(lhs, rhs, scala.Some(default), typ)
    }

    def Assignment(lhs: Variable, rhs: ImpLanExpr, default: Option[ImpLanExpr], typ: ImpLanType) : IntermediateCodeUtils = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.Assignment(lhs, rhs, default, typ))
      new IntermediateCodeUtils(newStmts, ifState, newIfStack, newElseStack, condStack)
    }

    def FinalAssignment(lhs: Variable, default: ImpLanVal, typ: ImpLanType) : IntermediateCodeUtils = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FinalAssignment(lhs, default, typ))
      new IntermediateCodeUtils(newStmts, ifState, newIfStack, newElseStack, condStack)
    }

    def If(cond : Seq[Seq[ImpLanExpr]]) : IntermediateCodeUtils = {
      new IntermediateCodeUtils(stmts, Seq(InIf) ++ ifState, Seq(Seq()) ++ ifStack, Seq(Seq()) ++ elseStack,
        Seq(cond) ++ condStack)
    }

    def Else() : IntermediateCodeUtils = ifState(0) match {
      case InIf => new IntermediateCodeUtils(stmts, ifState.updated(0, InElse), ifStack, elseStack, condStack)
      case InElse => throw tessla_compiler.Errors.DSLError("Two subseqeuent Else blocks")
      case Out => throw tessla_compiler.Errors.DSLError("Else without previous If")
    }

    def EndIf() : IntermediateCodeUtils = {
      val stmt = ifState(0) match {
        case InIf | InElse =>  IntermediateCode.If(condStack(0), ifStack(0), elseStack(0))
        case Out => throw tessla_compiler.Errors.DSLError("EndIf without previous If")
      }
      val tmpRes = new IntermediateCodeUtils(stmts, ifState.drop(1), ifStack.drop(1), elseStack.drop(1), condStack.drop(1))
      val (newStmts, newIfStack, newElseStack) = tmpRes.addStmt(stmt)
      new IntermediateCodeUtils(newStmts, ifState.drop(1), newIfStack, newElseStack, condStack.drop(1))
    }

    def FunctionCall(name: String, params: Seq[ImpLanExpr], typeHint: IntermediateCode.FunctionType) : IntermediateCodeUtils = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FunctionCall(name, params, typeHint))
      new IntermediateCodeUtils(newStmts, ifState, newIfStack, newElseStack, condStack)
    }

   def LambdaApllication(exp: ImpLanExpr, params: Seq[ImpLanExpr]) : IntermediateCodeUtils = {
    val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.LambdaApplication(exp, params))
    new IntermediateCodeUtils(newStmts, ifState, newIfStack, newElseStack, condStack)
   }

  }
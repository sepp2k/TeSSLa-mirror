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

  def foldAST[A](exp: ImpLanExpr, n: A, f: (A, ImpLanExpr) => A, g: (A, ImpLanStmt) => A): A = {
    val subStmts: Seq[ImpLanStmt] = exp match {
      case v: ImpLanVal => v match {
        case Some(content) => Seq(content)
        case _ => Seq()
      }
      case CastingExpression(e, _) =>  Seq(e)
      case FunctionCall(_, params, _) => params
      case LambdaApplication(exp, params) => params :+ exp
      case TernaryExpression(guard, e1, e2) => guard.flatten :+ e1 :+ e2
      case Equal(a, b) => Seq(a, b)
      case LambdaExpression(_, _, _, body) => body
      case _ => Seq()
    }

    foldAST(subStmts, f(n, exp), f, g)
  }

  def foldAST[A](stmts: Seq[ImpLanStmt], n: A, f: (A, ImpLanExpr) => A, g: (A, ImpLanStmt) => A): A = {
    stmts.foldLeft[A](n) {
      case (currN, stmt) => {
        val subStmts = stmt match {
          case expr: ImpLanExpr => Seq()
          case If(guard, stmts, elseStmts) => guard.flatten ++ stmts ++ elseStmts
          case TryCatchBlock(tr, cat) => tr ++ cat
          case Assignment(_, rexpr, defVal, _) => Seq(rexpr) ++ (if (defVal.isDefined) Seq(defVal.get) else Seq())
          case FinalAssignment(_, defVal, _) => Seq(defVal)
          case ReturnStatement(expr) => Seq(expr)
        }

        stmt match {
          case e : ImpLanExpr => foldAST(e, currN , f, g)
          case _ => foldAST(subStmts, g(currN, stmt), f, g)
        }
      }
    }
  }

  def mapAST(stmts: Seq[ImpLanStmt], f: ImpLanExpr => ImpLanExpr, g: ImpLanStmt => Option[ImpLanStmt]): Seq[ImpLanStmt] = {
    stmts.flatMap { stmt =>
      {
        val mappedStmt = stmt match {
          case expr: ImpLanExpr => mapAST(expr, f, g)
          case If(guard, stmts, elseStmts) => If(guard.map(_.map(mapAST(_, f, g))), mapAST(stmts, f, g), mapAST(elseStmts, f, g))
          case TryCatchBlock(tr, cat) => TryCatchBlock(mapAST(tr, f, g), mapAST(cat, f, g))
          case Assignment(lhs, rexpr, defVal, typ) => Assignment(lhs, mapAST(rexpr, f, g), if (defVal.isDefined) scala.Some(mapAST(defVal.get, f, g)) else scala.None, typ)
          case FinalAssignment(lhs, defVal, typ) => FinalAssignment(lhs, mapAST(defVal, f, g), typ)
          case ReturnStatement(expr) => ReturnStatement(mapAST(expr, f, g))
        }
        g(mappedStmt)
      }}
  }

  def mapAST(exp: ImpLanExpr, f: ImpLanExpr => ImpLanExpr, g: ImpLanStmt => Option[ImpLanStmt]): ImpLanExpr = {
    val mappedExp = exp match {
      case lanVal: ImpLanVal => lanVal
      case CastingExpression(e, target) => CastingExpression(mapAST(e, f, g), target)
      case FunctionCall(name, params, typeHint) => FunctionCall(name, params.map(mapAST(_, f, g)), typeHint)
      case LambdaApplication(exp, params) => LambdaApplication(mapAST(exp, f, g), params.map(mapAST(_, f, g)))
      case TernaryExpression(guard, e1, e2) => TernaryExpression(guard.map(_.map(mapAST(_, f, g))), mapAST(e1, f, g), mapAST(e2, f, g))
      case Equal(e1, e2) => Equal(mapAST(e1, f, g), mapAST(e2, f, g))
      case LambdaExpression(argNames, argsTypes, retType, body) => LambdaExpression(argNames, argsTypes, retType, mapAST(body, f, g))
      case _ => exp
    }
    f(mappedExp)
  }

  def getVariableMap(stmts: Seq[ImpLanStmt], baseMap: Map[String, (ImpLanType, Option[ImpLanExpr])] = Map()) : Map[String, (ImpLanType, Option[ImpLanExpr])] = {

    def extractAssignments(stmt: ImpLanStmt) : Seq[(String, ImpLanType, Option[ImpLanExpr])] = stmt match {
      case Assignment(lhs, _, defVal, typ) => Seq((lhs.name, typ, defVal))
      case FinalAssignment(lhs, defVal, typ) => Seq((lhs.name, typ, scala.Some(defVal)))
      case If(_, stmts, elseStmts) => stmts.concat(elseStmts).flatMap(extractAssignments)
      case TryCatchBlock(tr, cat) => tr.concat(cat).flatMap(extractAssignments)
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
      case InstatiatedType("Events", Seq(t), _) => typeConversion(t) //TODO: Unclean solution but not easy to surpass because of implicit
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

  def defaultValueForStreamType(t: Type): ImpLanVal = {
    t match {
      case InstatiatedType("Events", Seq(t), _) => defaultValueForType(t)
      case _ => throw tessla_compiler.Errors.CommandNotSupportedError(s"Stream type required but non-stream type $t passed.")
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

  implicit def Addition(op1: ImpLanExpr, op2: ImpLanExpr) : ImpLanExpr =
    FunctionCall("__add__", Seq(op1, op2), IntermediateCode.FunctionType(Seq(LongType, LongType), LongType))

  implicit def  Subtraction(op1: ImpLanExpr, op2: ImpLanExpr) : ImpLanExpr =
    FunctionCall("__sub__", Seq(op1, op2), IntermediateCode.FunctionType(Seq(LongType, LongType), LongType))

  implicit def  BitwiseOr(ops: Seq[ImpLanExpr]) : ImpLanExpr =
    FunctionCall("__bitor__", ops, IntermediateCode.FunctionType(ops.map{_ => LongType}, LongType))

  implicit def  NotEqual(a: ImpLanExpr, b: ImpLanExpr) : ImpLanExpr =
    Negation(Equal(a, b))

  implicit def  Greater(a: ImpLanExpr, b: ImpLanExpr) : ImpLanExpr =
    FunctionCall("__gt__", Seq(a, b), IntermediateCode.FunctionType(Seq(LongType, LongType), BoolType))

  implicit def  GreaterEqual(a: ImpLanExpr, b: ImpLanExpr) : ImpLanExpr =
    FunctionCall("__geq__", Seq(a, b), IntermediateCode.FunctionType(Seq(LongType, LongType), BoolType))

  implicit def  Negation(a: ImpLanExpr) : ImpLanExpr =
    FunctionCall("__not__", Seq(a), IntermediateCode.FunctionType(Seq(BoolType), BoolType))


}

sealed trait BlockState
case object InIf extends BlockState
case object InElse extends BlockState
case object InTry extends BlockState
case object InCatch extends BlockState
case object Out extends BlockState

class IntermediateCodeUtils(stmts: Seq[ImpLanStmt], blockState : Seq[BlockState] = Seq(Out),
                            ifTryStack: Seq[Seq[ImpLanStmt]] = Seq(), elseCatchStack: Seq[Seq[ImpLanStmt]] = Seq(),
                            condStack: Seq[Seq[Seq[ImpLanExpr]]] = Seq()) {

    def generateStatements: Seq[ImpLanStmt] = {
      if (blockState.head != Out) {
        throw tessla_compiler.Errors.DSLError("At least one unclosed If")
      }

      if (ifTryStack.nonEmpty || elseCatchStack.nonEmpty || condStack.nonEmpty) {
        throw tessla_compiler.Errors.DSLError("Stack sizes are not valid")
      }
      stmts
    }

    def addStmt(stmt: ImpLanStmt) : (Seq[ImpLanStmt], Seq[Seq[ImpLanStmt]], Seq[Seq[ImpLanStmt]]) = blockState.head match {
      case InIf | InTry => (stmts, ifTryStack.updated(0, ifTryStack.head :+ stmt), elseCatchStack)
      case InElse | InCatch => (stmts, ifTryStack, elseCatchStack.updated(0, elseCatchStack.head :+ stmt))
      case Out => (stmts :+ stmt, ifTryStack, elseCatchStack)
    }

    def Assignment(lhs: Variable, rhs: ImpLanExpr, default: ImpLanExpr, typ: ImpLanType) : IntermediateCodeUtils = {
      Assignment(lhs, rhs, scala.Some(default), typ)
    }

    def Assignment(lhs: Variable, rhs: ImpLanExpr, default: Option[ImpLanExpr], typ: ImpLanType) : IntermediateCodeUtils = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.Assignment(lhs, rhs, default, typ))
      new IntermediateCodeUtils(newStmts, blockState, newIfStack, newElseStack, condStack)
    }

    def FinalAssignment(lhs: Variable, default: ImpLanVal, typ: ImpLanType) : IntermediateCodeUtils = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FinalAssignment(lhs, default, typ))
      new IntermediateCodeUtils(newStmts, blockState, newIfStack, newElseStack, condStack)
    }

    def If(cond : Seq[Seq[ImpLanExpr]]) : IntermediateCodeUtils = {
      new IntermediateCodeUtils(stmts, Seq(InIf) ++ blockState, Seq(Seq()) ++ ifTryStack, Seq(Seq()) ++ elseCatchStack,
        Seq(cond) ++ condStack)
    }

    def Else() : IntermediateCodeUtils = blockState.head match {
      case InIf => new IntermediateCodeUtils(stmts, blockState.updated(0, InElse), ifTryStack, elseCatchStack, condStack)
      case InElse => throw tessla_compiler.Errors.DSLError("Two subseqeuent Else blocks")
      case _ => throw tessla_compiler.Errors.DSLError("Else without previous If")
    }

    def EndIf() : IntermediateCodeUtils = {
      val stmt = blockState.head match {
        case InIf | InElse =>  IntermediateCode.If(condStack(0), ifTryStack(0), elseCatchStack(0))
        case _ => throw tessla_compiler.Errors.DSLError("EndIf without previous If")
      }
      val tmpRes = new IntermediateCodeUtils(stmts, blockState.drop(1), ifTryStack.drop(1), elseCatchStack.drop(1), condStack.drop(1))
      val (newStmts, newIfStack, newElseStack) = tmpRes.addStmt(stmt)
      new IntermediateCodeUtils(newStmts, blockState.drop(1), newIfStack, newElseStack, condStack.drop(1))
    }

    def Try() : IntermediateCodeUtils = {
      new IntermediateCodeUtils(stmts, Seq(InTry) ++ blockState, Seq(Seq()) ++ ifTryStack, Seq(Seq()) ++ elseCatchStack,
        condStack)
    }

    def Catch() : IntermediateCodeUtils = blockState.head match {
      case InTry => new IntermediateCodeUtils(stmts, blockState.updated(0, InCatch), ifTryStack, elseCatchStack, condStack)
      case InCatch => throw tessla_compiler.Errors.DSLError("Two subseqeuent Catch blocks")
      case _ => throw tessla_compiler.Errors.DSLError("Catch without previous Try")
    }

    def EndTry() : IntermediateCodeUtils = {
      val stmt = blockState.head match {
        case InTry | InCatch =>  IntermediateCode.TryCatchBlock(ifTryStack.head, elseCatchStack.head)
        case _ => throw tessla_compiler.Errors.DSLError("EndTry without previous Try")
      }
      val tmpRes = new IntermediateCodeUtils(stmts, blockState.drop(1), ifTryStack.drop(1), elseCatchStack.drop(1), condStack)
      val (newStmts, newIfStack, newElseStack) = tmpRes.addStmt(stmt)
      new IntermediateCodeUtils(newStmts, blockState.drop(1), newIfStack, newElseStack, condStack)
    }

    def FunctionCall(name: String, params: Seq[ImpLanExpr], typeHint: IntermediateCode.FunctionType) : IntermediateCodeUtils = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FunctionCall(name, params, typeHint))
      new IntermediateCodeUtils(newStmts, blockState, newIfStack, newElseStack, condStack)
    }

   def LambdaApllication(exp: ImpLanExpr, params: Seq[ImpLanExpr]) : IntermediateCodeUtils = {
    val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.LambdaApplication(exp, params))
    new IntermediateCodeUtils(newStmts, blockState, newIfStack, newElseStack, condStack)
   }

  }

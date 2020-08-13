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

    f(foldAST(subStmts, n, f, g), exp)
  }

  def foldAST[A](stmts: Seq[ImpLanStmt], n: A, f: (A, ImpLanExpr) => A, g: (A, ImpLanStmt) => A): A = {
    stmts.foldLeft[A](n) {
      case (currN, stmt) => {
        val subStmts = stmt match {
          case expr: ImpLanExpr => Seq()
          case If(guard, stmts, elseStmts) => guard.flatten ++ stmts ++ elseStmts
          case TryCatchBlock(tr, cat) => tr ++ cat
          case Assignment(_, rexpr, defVal, _, _) => Seq(rexpr) ++ (if (defVal.isDefined) Seq(defVal.get) else Seq())
          case FinalAssignment(_, defVal, _) => Seq(defVal)
          case ReturnStatement(expr) => Seq(expr)
        }

        stmt match {
          case e : ImpLanExpr => foldAST(e, currN , f, g)
          case _ => g(foldAST(subStmts, currN, f, g), stmt)
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
          case Assignment(lhs, rexpr, defVal, typ, global) => Assignment(lhs, mapAST(rexpr, f, g), if (defVal.isDefined) scala.Some(mapAST(defVal.get, f, g)) else scala.None, typ, global)
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

  def getVariableMap(stmts: Seq[ImpLanStmt], baseMap: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)] = Map(), global: Boolean) : Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)] = {

    def extractAssignments(topLevel: Boolean)(stmt: ImpLanStmt) : Seq[(String, ImpLanType, Option[ImpLanExpr], Boolean)] = stmt match {
      case Assignment(lhs, rhs, defVal, typ, globVar) => (if (global && globVar || topLevel && !globVar) Seq((lhs.name, typ, defVal, global)) else Seq()) ++
                                                         (if (global) extractAssignmentsFromExp(rhs) else Seq())
      case FinalAssignment(lhs, defVal, typ) => Seq((lhs.name, typ, scala.Some(defVal), false))
      case If(_, stmts, elseStmts) => stmts.concat(elseStmts).flatMap(extractAssignments(true))
      case TryCatchBlock(tr, cat) => tr.concat(cat).flatMap(extractAssignments(true))
      case _ => Seq()
    }

    def extractAssignmentsFromExp(exp: ImpLanExpr) : Seq[(String, ImpLanType, Option[ImpLanExpr], Boolean)] = exp match {
      case CastingExpression(e, _) => extractAssignmentsFromExp(e)
      case FunctionCall(_, params, _) => params.flatMap(extractAssignmentsFromExp)
      case LambdaApplication(exp, _) => extractAssignmentsFromExp(exp)
      case TernaryExpression(guard, e1, e2) => guard.flatMap(_.flatMap(extractAssignmentsFromExp)) ++ extractAssignmentsFromExp(e1) ++ extractAssignmentsFromExp(e2)
      case Equal(e1, e2) => extractAssignmentsFromExp(e1) ++ extractAssignmentsFromExp(e2)
      case LambdaExpression(_, _, _, body) => body.flatMap(extractAssignments(false))
      case _ => Seq()
    }

    val varDefs : Seq[(String, ImpLanType, Option[ImpLanExpr], Boolean)] = baseMap.toSeq.map{case (a, (b, c, d)) => (a,b, c, d)} ++ stmts.flatMap(extractAssignments(true)).distinct
    val duplicates = varDefs.groupBy{case (n, _, _, _) => n}.collect{case (x, List(_,_,_*)) => x}

    if (duplicates.nonEmpty) {
      throw tessla_compiler.Errors.DSLError(s"Variable(s) with unsound type/default information: ${duplicates.mkString(", ")}")
    }

    varDefs.map{case (name, typ, default, global) => (name, (typ, default, global))}.toMap
  }

  implicit def typeConversion(t: Type): ImpLanType = {
    t match {
      case InstantiatedType("Events", Seq(t), _) => typeConversion(t) //TODO: Unclean solution but not easy to surpass because of implicit
      case RecordType(entries, _) if entries.isEmpty => UnitType
      case RecordType(entries, _) if entries.size == 1 => typeConversion(entries.toSeq.head._2._1)
      case InstantiatedType("Bool", Seq(), _) => BoolType
      case InstantiatedType("Int", Seq(), _) => LongType
      case InstantiatedType("Float", Seq(), _) => DoubleType
      case InstantiatedType("String", Seq(), _) => StringType
      case InstantiatedType("Option", Seq(t), _) => OptionType(t)
      case InstantiatedType("Set", Seq(t), _) => ImmutableSetType(t)
      case InstantiatedType("MutSet", Seq(t), _) => MutableSetType(t)
      case InstantiatedType("Map", Seq(t1, t2), _) => ImmutableMapType(t1, t2)
      case InstantiatedType("MutMap", Seq(t1, t2), _) => MutableMapType(t1, t2)
      case InstantiatedType("List", Seq(t), _) => ImmutableListType(t)
      case InstantiatedType("MutList", Seq(t), _) => MutableListType(t)
      case TesslaAST.Core.FunctionType(_, paramTypes, resultType, _) => IntermediateCode.FunctionType(paramTypes.map{case (_,t) => typeConversion(t)}, typeConversion(resultType)) //TODO: Type params
      case RecordType(entries, _) => {
        def comp(s1: String, s2: String) : Boolean = {
          val n1 = s1.stripPrefix("_").toIntOption
          val n2 = s2.stripPrefix("_").toIntOption
          if (n1.isDefined && n2.isDefined && s1.startsWith("_") && s2.startsWith("_")) {
            n1.get <= n2.get
          } else {
            s1 < s2
          }
        }
        val sortedEntries = entries.toSeq.sortWith{case ((n1, _), (n2, _)) => comp(n1.name, n2.name)}
        val names = sortedEntries.map(_._1.name)
        val types = sortedEntries.map{case (_,t) => typeConversion(t._1)}
        StructType(types, names)
      }
      case TypeParam(_, _) => GeneralType //TODO: Resolve type params if possible //TODO: Introduce GenericType in Intermediate Code for Rust translation
      case i: InstantiatedType => throw tessla_compiler.Errors.CommandNotSupportedError(s"Type translation for type $i not supported")
      case _ => throw tessla_compiler.Errors.CommandNotSupportedError(s"Type translation for type $t not supported")
    }
  }

  def defaultValueForType(t: Type): ImpLanVal = {
    t match {
      case RecordType(entries, _) if entries.isEmpty => UnitValue
      case RecordType(entries, _) if entries.size == 1 => defaultValueForType(entries.toSeq.head._2._1)
      case InstantiatedType("Bool", Seq(), _) => BoolValue(false)
      case InstantiatedType("Int", Seq(), _) => LongValue(0)
      case InstantiatedType("Float", Seq(), _) => DoubleValue(0)
      case InstantiatedType("String", Seq(), _) => StringValue("")
      case InstantiatedType("Option", Seq(t), _) => None(t)
      case InstantiatedType("Set", Seq(t), _) => EmptyImmutableSet(t)
      case InstantiatedType("MutSet", Seq(t), _) => EmptyMutableSet(t)
      case InstantiatedType("Map", Seq(t1, t2), _) => EmptyImmutableMap(t1, t2)
      case InstantiatedType("MutMap", Seq(t1, t2), _) => EmptyMutableMap(t1, t2)
      case InstantiatedType("List", Seq(t), _) => EmptyImmutableList(t)
      case InstantiatedType("MutList", Seq(t), _) => EmptyMutableList(t)
      case TesslaAST.Core.FunctionType(_, _, _, _) => EmptyFunction(t)
      case TypeParam(_, _) => GeneralValue //TODO: Introduce GenericType in Intermediate Code for Rust translation
      case i: InstantiatedType => throw tessla_compiler.Errors.CommandNotSupportedError(s"Default value for type $i not supported")
      case RecordType(entries, _) => StructValue(entries.map{case (n,t) => (n.name, defaultValueForType(t._1))})
      case _ => throw tessla_compiler.Errors.CommandNotSupportedError(s"Default value for type $t not supported")
    }
  }

  def defaultValueForStreamType(t: Type): ImpLanVal = {
    t match {
      case InstantiatedType("Events", Seq(t), _) => defaultValueForType(t)
      case _ => throw tessla_compiler.Errors.CoreASTError(s"Stream type required but non-stream type $t passed.")
    }
  }

  def structIsTuple(structType: StructType) : Boolean = {
    structType.fieldNames.indices.forall(i => structType.fieldNames.contains(s"_${i+1}"))
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

  implicit def MkStruct(content: Seq[(String, ImpLanExpr)], targetType: ImpLanType) : ImpLanExpr = {
    targetType match {
      case castedTargetType: StructType => {
        val sortedContent = content.sortBy { case (n1, _) => castedTargetType.fieldNames.indexOf(n1) }
        FunctionCall("__mkStruct__", sortedContent.map(_._2), IntermediateCode.FunctionType(castedTargetType.subTypes, targetType))
      }
      case _ => content.head._2
    }
  }

  implicit def GetStruct(struct: ImpLanExpr, fieldName: String, structType: ImpLanType) : ImpLanExpr = {
    structType match {
      case castedStructType: StructType => {
        FunctionCall("__getStruct__", Seq(struct, StringValue(fieldName)), IntermediateCode.FunctionType(Seq(structType, StringType), castedStructType.subTypes(castedStructType.fieldNames.indexOf(fieldName))))
      }
      case _ => struct
    }
  }

}

sealed trait BlockState
case object InIf extends BlockState
case object InElse extends BlockState
case object InTry extends BlockState
case object InCatch extends BlockState
case object Out extends BlockState

class IntermediateCodeUtils(stmts: Seq[ImpLanStmt], blockState : Seq[BlockState] = Seq(Out),
                            ifTryStack: Seq[Seq[ImpLanStmt]] = Seq(), elseCatchStack: Seq[Seq[ImpLanStmt]] = Seq(),
                            condStack: Seq[Seq[Seq[ImpLanExpr]]] = Seq()) { //TODO: Distinguish from Object

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

    def Assignment(lhs: Variable, rhs: ImpLanExpr, default: ImpLanExpr, typ: ImpLanType, global: Boolean = false) : IntermediateCodeUtils = {
      Assignment(lhs, rhs, scala.Some(default), typ, global)
    }

    def Assignment(lhs: Variable, rhs: ImpLanExpr, default: Option[ImpLanExpr], typ: ImpLanType, global: Boolean) : IntermediateCodeUtils = {
      val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.Assignment(lhs, rhs, default, typ, global))
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

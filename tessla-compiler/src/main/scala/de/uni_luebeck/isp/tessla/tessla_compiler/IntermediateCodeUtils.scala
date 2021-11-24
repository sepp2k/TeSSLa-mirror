/*
 * Copyright 2021 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{BoolType, StringType, UnitType, _}

import scala.language.{implicitConversions, postfixOps}

/**
 * Class containing a DSL for easy creation of ImpLanStmt-Blocks and other useful static methods for dealing
 * ImpLan and the translation from TeSSLa
 */
object IntermediateCodeUtils {

  /**
   * Represents the types of declared variables
   */
  sealed trait DeclarationType

  /**
   * Variable can be assigned and evaluated at declaration and is never changed again
   */
  case object FinalDeclaration extends DeclarationType

  /**
   * Variable can be assigned at declaration and is never changed again, the evaluation has to be lazy
   */
  case object FinalLazyDeclaration extends DeclarationType

  /**
   * The variable is reassigned in the generated code
   */
  case object VariableDeclaration extends DeclarationType

  /**
   * Folds an [[IntermediateCode.ImpLanExpr]] including all sub-expressions and sub-statements.
   * Sub-statements/sub-expressions are traversed prior their parent expressions
   * @param exp The sequence of statements to be folded
   * @param n The start value
   * @param f Function executed on every expression
   * @param g Function executed on every statement
   * @tparam A The result type of the folding operation
   * @return The result of the folding
   */
  def foldAST[A](exp: ImpLanExpr, n: A, f: (A, ImpLanExpr) => A, g: (A, ImpLanStmt) => A): A = {
    val subStmts: Seq[ImpLanStmt] = exp match {
      case v: ImpLanVal =>
        v match {
          case SomeValue(content) => Seq(content)
          case _                  => Seq()
        }
      case CastingExpression(e, _, _)       => Seq(e)
      case FunctionCall(_, params, _)       => params
      case LambdaApplication(exp, params)   => params :+ exp
      case TernaryExpression(guard, e1, e2) => guard.flatten :+ e1 :+ e2
      case Equal(a, b)                      => Seq(a, b)
      case LambdaExpression(_, _, _, body)  => body
      case _                                => Seq()
    }

    f(foldAST(subStmts, n, f, g), exp)
  }

  /**
   * Folds a sequence of statements including all sub-expressions and sub-statements.
   * Sub-statements/sub-expressions are traversed prior their parent expressions
   * @param stmts The sequence of statements to be folded
   * @param n The start value
   * @param f Function executed on every expression
   * @param g Function executed on every statement
   * @tparam A The result type of the folding operation
   * @return The result of the folding
   */
  def foldAST[A](stmts: Seq[ImpLanStmt], n: A, f: (A, ImpLanExpr) => A, g: (A, ImpLanStmt) => A): A = {
    stmts.foldLeft[A](n) {
      case (currN, stmt) => {
        val subStmts = stmt match {
          case _: ImpLanExpr                      => Seq()
          case If(guard, stmts, elseStmts)        => guard.flatten ++ stmts ++ elseStmts
          case TryCatchBlock(tr, cat)             => tr ++ cat
          case Assignment(_, rexpr, defVal, _, _) => Seq(rexpr) ++ (if (defVal.isDefined) Seq(defVal.get) else Seq())
          case FinalAssignment(_, value, _, _)    => (if (value.isDefined) Seq(value.get) else Seq())
          case ReturnStatement(expr)              => Seq(expr)
        }

        stmt match {
          case e: ImpLanExpr => foldAST(e, currN, f, g)
          case _             => g(foldAST(subStmts, currN, f, g), stmt)
        }
      }
    }
  }

  /**
   * Maps f on all subexpressions and g on all sub-statements of a sequence of statements
   * @param stmts The sequence of statements to be mapped
   * @param f Function to map expressions
   * @param g Function to map statements
   * @return Mapped sequence of statements
   */
  def mapAST(
    stmts: Seq[ImpLanStmt],
    f: ImpLanExpr => ImpLanExpr,
    g: ImpLanStmt => Option[ImpLanStmt]
  ): Seq[ImpLanStmt] = {
    stmts.flatMap { stmt =>
      {
        val mappedStmt = stmt match {
          case expr: ImpLanExpr => mapAST(expr, f, g)
          case If(guard, stmts, elseStmts) =>
            If(guard.map(_.map(mapAST(_, f, g))), mapAST(stmts, f, g), mapAST(elseStmts, f, g))
          case TryCatchBlock(tr, cat) => TryCatchBlock(mapAST(tr, f, g), mapAST(cat, f, g))
          case Assignment(lhs, rexpr, defVal, typ, glob) =>
            Assignment(
              lhs,
              mapAST(rexpr, f, g),
              if (defVal.isDefined) Some(mapAST(defVal.get, f, g)) else None,
              typ,
              glob
            )
          case FinalAssignment(lhs, value, typ, ld) => FinalAssignment(lhs, value.map(mapAST(_, f, g)), typ, ld)
          case ReturnStatement(expr)                => ReturnStatement(mapAST(expr, f, g))
        }
        g(mappedStmt)
      }
    }
  }

  /**
   * Maps f on all subexpressions and g on all sub-statements of an [[IntermediateCode.ImpLanExpr]]
   * @param exp The expression to be mapped
   * @param f Function to map expressions
   * @param g Function to map statements
   * @return Mapped expression
   */
  def mapAST(exp: ImpLanExpr, f: ImpLanExpr => ImpLanExpr, g: ImpLanStmt => Option[ImpLanStmt]): ImpLanExpr = {
    val mappedExp = exp match {
      case lanVal: ImpLanVal                    => lanVal
      case CastingExpression(e, from, target)   => CastingExpression(mapAST(e, f, g), from, target)
      case FunctionCall(name, params, typeHint) => FunctionCall(name, params.map(mapAST(_, f, g)), typeHint)
      case LambdaApplication(exp, params)       => LambdaApplication(mapAST(exp, f, g), params.map(mapAST(_, f, g)))
      case TernaryExpression(guard, e1, e2) =>
        TernaryExpression(guard.map(_.map(mapAST(_, f, g))), mapAST(e1, f, g), mapAST(e2, f, g))
      case Equal(e1, e2) => Equal(mapAST(e1, f, g), mapAST(e2, f, g))
      case LambdaExpression(argNames, argsTypes, retType, body) =>
        LambdaExpression(argNames, argsTypes, retType, mapAST(body, f, g))
      case _ => exp
    }
    f(mappedExp)
  }

  /**
   * Builds a sequence containing type/default/lazyness/global-definition information for all variables defined in stmts.
   * If unsound information (e.g. multiple declarations) are found an error is thrown.
   * The order is the one of the corresponding assignments.
   * @param stmts The sequence of statements to be examined
   * @param baseSeq The base sequence to be extended
   * @param global Include global definitions in sub-expressions (e.g. functions)
   * @return Map containing variable information.
   */
  def getVariableSeq(
    stmts: Seq[ImpLanStmt],
    baseSeq: Seq[(String, (ImpLanType, Option[ImpLanExpr], DeclarationType))] = Seq(),
    global: Boolean
  ): Seq[(String, (ImpLanType, Option[ImpLanExpr], DeclarationType))] = {

    def extractAssignments(
      topLevel: Boolean
    )(stmt: ImpLanStmt): Seq[(String, ImpLanType, Option[ImpLanExpr], DeclarationType)] = stmt match {
      case Assignment(lhs, rhs, defVal, typ, globVar) =>
        (if (global && globVar || topLevel && !globVar) Seq((lhs.name, typ, defVal, VariableDeclaration)) else Seq()) ++
          (if (global) extractAssignmentsFromExp(rhs) else Seq())
      case FinalAssignment(lhs, value, typ, ld) if topLevel =>
        val decType = if (ld) FinalLazyDeclaration else FinalDeclaration
        Seq((lhs.name, typ, value, decType))
      case If(_, stmts, elseStmts) => stmts.concat(elseStmts).flatMap(extractAssignments(topLevel))
      case TryCatchBlock(tr, cat)  => tr.concat(cat).flatMap(extractAssignments(topLevel))
      case _                       => Seq()
    }

    def extractAssignmentsFromExp(exp: ImpLanExpr): Seq[(String, ImpLanType, Option[ImpLanExpr], DeclarationType)] =
      exp match {
        case CastingExpression(e, _, _) => extractAssignmentsFromExp(e)
        case FunctionCall(_, params, _) => params.flatMap(extractAssignmentsFromExp)
        case LambdaApplication(exp, _)  => extractAssignmentsFromExp(exp)
        case TernaryExpression(guard, e1, e2) =>
          guard.flatMap(_.flatMap(extractAssignmentsFromExp)) ++ extractAssignmentsFromExp(
            e1
          ) ++ extractAssignmentsFromExp(e2)
        case Equal(e1, e2)                   => extractAssignmentsFromExp(e1) ++ extractAssignmentsFromExp(e2)
        case LambdaExpression(_, _, _, body) => body.flatMap(extractAssignments(false))
        case _                               => Seq()
      }

    val varDefs: Seq[(String, ImpLanType, Option[ImpLanExpr], DeclarationType)] = (baseSeq.map {
      case (a, (b, c, d)) => (a, b, c, d)
    } ++ stmts.flatMap(extractAssignments(true))).distinct
    val duplicates = varDefs.groupBy { case (n, _, _, _) => n }.collect { case (x, List(_, _, _*)) => x }

    if (duplicates.nonEmpty) {
      throw Diagnostics.DSLError(s"Variable(s) with unsound type/default information: ${duplicates.mkString(", ")}")
    }

    varDefs.map { case (name, typ, default, lazyDef) => (name, (typ, default, lazyDef)) }
  }

  /**
   * Converts TeSSLa type to corresponding [[IntermediateCode.ImpLanType]]
   * @param t Type to be converted. If type is Events[t] the result is equal to calling the function with t.
   * @return The converted type
   */
  implicit def typeConversion(t: Type): ImpLanType = {
    t match {
      case InstantiatedType("Events", Seq(t), _)      => typeConversion(t)
      case RecordType(entries, _) if entries.isEmpty  => UnitType
      case InstantiatedType("Bool", Seq(), _)         => BoolType
      case InstantiatedType("Int", Seq(), _)          => LongType
      case InstantiatedType("Float", Seq(), _)        => DoubleType
      case InstantiatedType("String", Seq(), _)       => StringType
      case InstantiatedType("Option", Seq(t), _)      => OptionType(t)
      case InstantiatedType("Set", Seq(t), _)         => ImmutableSetType(t)
      case InstantiatedType("MutSet", Seq(t), _)      => MutableSetType(t)
      case InstantiatedType("Map", Seq(t1, t2), _)    => ImmutableMapType(t1, t2)
      case InstantiatedType("MutMap", Seq(t1, t2), _) => MutableMapType(t1, t2)
      case InstantiatedType("List", Seq(t), _)        => ImmutableListType(t)
      case InstantiatedType("MutList", Seq(t), _)     => MutableListType(t)
      case InstantiatedType("Queue", Seq(t), _)       => ImmutableQueueType(t)
      case InstantiatedType("MutQueue", Seq(t), _)    => MutableQueueType(t)
      case InstantiatedType(n, tps, _) if n.startsWith("native:") =>
        NativeType(n.stripPrefix("native:"), tps.map(typeConversion))
      case Core.FunctionType(_, paramTypes, resultType, _) =>
        IntermediateCode.FunctionType(
          paramTypes.map {
            case (LazyEvaluation, t)   => LazyContainer(typeConversion(t))
            case (StrictEvaluation, t) => typeConversion(t)
          },
          typeConversion(resultType)
        )
      case RecordType(entries, _) => {
        val sortedEntries = entries.toSeq.sortWith { case ((n1, _), (n2, _)) => structComparison(n1.name, n2.name) }
        val names = sortedEntries.map(_._1.name)
        val types = sortedEntries.map { case (_, t) => typeConversion(t._1) }
        StructType(types, names)
      }
      case TypeParam(_, _) => GeneralType
      case _ =>
        throw Diagnostics.CommandNotSupportedError(s"Type translation for type $t not supported")
    }
  }

  /**
   * Calculates a static default value for a non-stream type.
   * @param t Type whose default value shall be determined. May not be Events[...]
   * @return Default value for given type
   */
  def defaultValueForType(t: Type): Option[ImpLanVal] = {
    t match {
      case RecordType(entries, _) if entries.isEmpty  => Some(UnitValue)
      case InstantiatedType("Bool", Seq(), _)         => Some(BoolValue(false))
      case InstantiatedType("Int", Seq(), _)          => Some(LongValue(0))
      case InstantiatedType("Float", Seq(), _)        => Some(DoubleValue(0))
      case InstantiatedType("String", Seq(), _)       => Some(StringValue(""))
      case InstantiatedType("Option", Seq(t), _)      => Some(NoneValue(t))
      case InstantiatedType("Set", Seq(t), _)         => Some(EmptyImmutableSet(t))
      case InstantiatedType("MutSet", Seq(t), _)      => Some(EmptyMutableSet(t))
      case InstantiatedType("Map", Seq(t1, t2), _)    => Some(EmptyImmutableMap(t1, t2))
      case InstantiatedType("MutMap", Seq(t1, t2), _) => Some(EmptyMutableMap(t1, t2))
      case InstantiatedType("List", Seq(t), _)        => Some(EmptyImmutableList(t))
      case InstantiatedType("MutList", Seq(t), _)     => Some(EmptyMutableList(t))
      case InstantiatedType("Queue", Seq(t), _)       => Some(EmptyImmutableQueue(t))
      case InstantiatedType("MutQueue", Seq(t), _)    => Some(EmptyMutableQueue(t))
      case Core.FunctionType(_, _, _, _)              => Some(EmptyFunction(t))
      case TypeParam(_, _)                            => Some(GeneralValue)
      case RecordType(entries, _) =>
        val subVals = entries.map { case (n, t) => (n.name, defaultValueForType(t._1)) }
        if (subVals.exists(v => v._2.isEmpty)) None else Some(StructValue(subVals.map { case (n, t) => (n, t.get) }))
      case _ => None
    }
  }

  /**
   * Calculates a static default value for a stream type. For Events[t] it is the default value of t.
   * @param t Type whose default value shall be determined. Must be Events[...]
   * @return Default value for given type
   */
  def defaultValueForStreamType(t: Type): Option[ImpLanVal] = {
    t match {
      case InstantiatedType("Events", Seq(t), _) => defaultValueForType(t)
      case _                                     => throw Diagnostics.CoreASTError(s"Stream type required but non-stream type $t passed.")
    }
  }

  /**
   * Figures out if a struct is a tuple. i.e. has field names _1,_2 ...
   * @param structType The struct to be examined
   * @return Whether given struct is tuple
   */
  def structIsTuple(structType: StructType): Boolean = {
    structType.fieldNames.indices.forall(i => structType.fieldNames.contains(s"_${i + 1}"))
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

  /**
   * Sorting function for structs taking care of the right sorting of tuple fild names
   * @param s1 First param for comparison
   * @param s2 Second param for comparison
   * @return true if s1 <= s2 with respect to the special sorting rules
   */
  def structComparison(s1: String, s2: String): Boolean = {
    val n1 = s1.stripPrefix("_").toIntOption
    val n2 = s2.stripPrefix("_").toIntOption
    if (n1.isDefined && n2.isDefined && s1.startsWith("_") && s2.startsWith("_")) {
      n1.get <= n2.get
    } else {
      s1 <= s2
    }
  }

  /**
   * Generates an addition expression
   * @param op1 First parameter sub-expression, must be of [[IntermediateCode.LongType]]
   * @param op2 Second parameter sub-expression, must be of [[IntermediateCode.LongType]]
   * @return Addition expression
   */
  def Addition(op1: ImpLanExpr, op2: ImpLanExpr): ImpLanExpr =
    FunctionCall("__add__", Seq(op1, op2), IntermediateCode.FunctionType(Seq(LongType, LongType), LongType))

  /**
   * Generates a subtraction expression
   * @param op1 First parameter sub-expression, must be of [[IntermediateCode.LongType]]
   * @param op2 Second parameter sub-expression, must be of [[IntermediateCode.LongType]]
   * @return Subtraction expression
   */
  def Subtraction(op1: ImpLanExpr, op2: ImpLanExpr): ImpLanExpr =
    FunctionCall("__sub__", Seq(op1, op2), IntermediateCode.FunctionType(Seq(LongType, LongType), LongType))

  /**
   * Generates an bitor (|) expression
   * @param ops parameter sub-expressions, must be of type [[IntermediateCode.LongType]]
   * @return Or expression
   */
  def BitwiseOr(ops: Seq[ImpLanExpr]): ImpLanExpr =
    FunctionCall("__bitor__", ops, IntermediateCode.FunctionType(ops.map { _ => LongType }, LongType))

  /**
   * Generates an or (||) expression
   * @param ops parameter sub-expressions, must be of type [[IntermediateCode.BoolType]]
   * @return Or expression
   */
  def Or(ops: Seq[ImpLanExpr]): ImpLanExpr =
    FunctionCall("__or__", ops, IntermediateCode.FunctionType(ops.map { _ => BoolType }, BoolType))

  /**
   * Generates an and (&&) expression
   * @param ops parameter sub-expressions, must be of type [[IntermediateCode.BoolType]]
   * @return And expression
   */
  def And(ops: Seq[ImpLanExpr]): ImpLanExpr =
    FunctionCall("__and__", ops, IntermediateCode.FunctionType(ops.map { _ => BoolType }, BoolType))

  /**
   * Generates a not equal expression
   * @param a First parameter sub-expression
   * @param b Second parameter sub-expression
   * @return Not equal expression
   */
  def NotEqual(a: ImpLanExpr, b: ImpLanExpr): ImpLanExpr =
    Negation(Equal(a, b))

  /**
   * Generates a greater expression
   * @param a First parameter sub-expression, must be of [[IntermediateCode.LongType]]
   * @param b Second parameter sub-expression, must be of [[IntermediateCode.LongType]]
   * @return Greater expression
   */
  def Greater(a: ImpLanExpr, b: ImpLanExpr): ImpLanExpr =
    FunctionCall("__gt__", Seq(a, b), IntermediateCode.FunctionType(Seq(LongType, LongType), BoolType))

  /**
   * Generates a greater equal expression
   * @param a First parameter sub-expression, must be of [[IntermediateCode.LongType]]
   * @param b Second parameter sub-expression, must be of [[IntermediateCode.LongType]]
   * @return Greater equal expression
   */
  def GreaterEqual(a: ImpLanExpr, b: ImpLanExpr): ImpLanExpr =
    FunctionCall("__geq__", Seq(a, b), IntermediateCode.FunctionType(Seq(LongType, LongType), BoolType))

  /**
   * Generates a negate expression
   * @param a Sub-expression to be negated
   * @return Negate expression
   */
  def Negation(a: ImpLanExpr): ImpLanExpr =
    FunctionCall("__not__", Seq(a), IntermediateCode.FunctionType(Seq(BoolType), BoolType))

  /**
   * Generates a throw expression
   * @param e sub expression of the error to be thrown, must be of [[IntermediateCode.ErrorType]]
   * @param forType Type which should be returned inst
   * @return Expression throwing an error
   */
  def Throw(e: ImpLanExpr, forType: ImpLanType): ImpLanExpr =
    FunctionCall("__[TC]throw__", Seq(e), IntermediateCode.FunctionType(Seq(IntermediateCode.ErrorType), forType))

  /**
   * Produces an expression building a struct
   * @param content Sub expressions with field names
   * @param targetType The type of the struct to be generated
   * @return Expression building a struct
   */
  def MkStruct(content: Seq[(String, ImpLanExpr)], targetType: ImpLanType): ImpLanExpr = {
    targetType match {
      case castedTargetType: StructType => {
        val sortedContent = content.sortBy { case (n1, _) => castedTargetType.fieldNames.indexOf(n1) }
        FunctionCall(
          "__mkStruct__",
          sortedContent.map(_._2),
          IntermediateCode.FunctionType(castedTargetType.subTypes, targetType)
        )
      }
      case _ => content.head._2
    }
  }

  /**
   * Produces an expression for accessing a structure field
   * @param struct Expression which evaluates to a struct
   * @param fieldName The accessed field name
   * @param structType The type of the struct
   * @return Expression accessing the struct
   */
  def GetStruct(struct: ImpLanExpr, fieldName: String, structType: ImpLanType): ImpLanExpr = {
    structType match {
      case castedStructType: StructType => {
        FunctionCall(
          "__getStruct__",
          Seq(struct, StringValue(fieldName)),
          IntermediateCode.FunctionType(
            Seq(structType, StringType),
            castedStructType.subTypes(castedStructType.fieldNames.indexOf(fieldName))
          )
        )
      }
      case _ => struct
    }
  }

}

/**
 * Trait representing the internal states of [[IntermediateCodeUtils]]
 */
sealed trait BlockState
case object InIf extends BlockState
case object InElse extends BlockState
case object InTry extends BlockState
case object InCatch extends BlockState
case object Out extends BlockState

/**
 * DSL-Style state class for easy generation of ImpLan code.
 * Functions of this class can be called iteratively to produce ImpLan step by step.
 * @param stmts Sequence of already generated statements
 * @param blockState Stack keeping track of unclosed ifs/try-catchs
 * @param ifTryStack Stack of statements in If/Try blocks which are not closed yet
 * @param elseCatchStack Stack of statements in Else/Catch blocks which are not closed yet
 * @param condStack Stack of conditions of if blocks which are not closed yet
 */
class IntermediateCodeUtils(
  stmts: Seq[ImpLanStmt],
  blockState: Seq[BlockState] = Seq(Out),
  ifTryStack: Seq[Seq[ImpLanStmt]] = Seq(),
  elseCatchStack: Seq[Seq[ImpLanStmt]] = Seq(),
  condStack: Seq[Seq[Seq[ImpLanExpr]]] = Seq()
) {

  /**
   * Returns current statements. Checks if all if/trys are closed.
   * @return  Sequence of statements from the current state
   */
  def generateStatements: Seq[ImpLanStmt] = {
    if (blockState.head != Out) {
      throw Diagnostics.DSLError("At least one unclosed If")
    }

    if (ifTryStack.nonEmpty || elseCatchStack.nonEmpty || condStack.nonEmpty) {
      throw Diagnostics.DSLError("Stack sizes are not valid")
    }
    stmts
  }

  /**
   * Add assignment to the code
   * @param lhs The left hand side of the assignment
   * @param rhs The right hand side of the assignment
   * @param default The default value the variable has until the assignment .
   *                Must match the default value from other assignments to this variable.
   *                Optional.
   * @param typ The type of the assigned identifier.
   *            Must match the type from other assignments to this variable.
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def Assignment(
    lhs: Variable,
    rhs: ImpLanExpr,
    default: Option[ImpLanExpr],
    typ: ImpLanType
  ): IntermediateCodeUtils = {
    Assignment(lhs, rhs, default, typ, false)
  }

  /**
   * Add assignment to the code
   * @param lhs The left hand side of the assignment
   * @param rhs The right hand side of the assignment
   * @param default The default value the variable has until the assignment .
   *                Must match the default value from other assignments to this variable.
   * @param typ The type of the assigned identifier.
   *            Must match the type from other assignments to this variable.
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def Assignment(
    lhs: Variable,
    rhs: ImpLanExpr,
    default: ImpLanExpr,
    typ: ImpLanType
  ): IntermediateCodeUtils = {
    Assignment(lhs, rhs, Some(default), typ, false)
  }

  /**
   * Add assignment to the code
   * @param lhs The left hand side of the assignment
   * @param rhs The right hand side of the assignment
   * @param default The default value the variable has until the assignment .
   *                Must match the default value from other assignments to this variable.
   *                Optional.
   * @param typ The type of the assigned identifier.
   *            Must match the type from other assignments to this variable.
   * @param glob Whether the variable must be declared in the global space.
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def Assignment(
    lhs: Variable,
    rhs: ImpLanExpr,
    default: Option[ImpLanExpr],
    typ: ImpLanType,
    glob: Boolean
  ): IntermediateCodeUtils = {
    val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.Assignment(lhs, rhs, default, typ, glob))
    new IntermediateCodeUtils(newStmts, blockState, newIfStack, newElseStack, condStack)
  }

  /**
   * Add a final assignment to the code. May be called twice for the same lhs but only with exactly the same params.
   * @param lhs The left hand side of the assignment
   * @param value The value the variable has.
   *                Optional.
   * @param typ The type of the assigned identifier.
   * @param lazyDef Flag indicating if the variable is of type lazy
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def FinalAssignment(
    lhs: Variable,
    value: ImpLanVal,
    typ: ImpLanType,
    lazyDef: Boolean = false
  ): IntermediateCodeUtils = {
    FinalAssignmentWithOptionalValue(lhs, Some(value), typ, lazyDef)
  }

  /**
   * Add a final assignment to the code. May be called twice for the same lhs but only with exactly the same params.
   * @param lhs The left hand side of the assignment
   * @param value The value the variable has.
   *                Optional. Note: Only in rare cases it makes sense to have a final assignment without value.
   * @param typ The type of the assigned identifier.
   * @param lazyDef Flag indicating if the variable is of type lazy
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def FinalAssignmentWithOptionalValue(
    lhs: Variable,
    value: Option[ImpLanVal],
    typ: ImpLanType,
    lazyDef: Boolean = false
  ): IntermediateCodeUtils = {
    val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FinalAssignment(lhs, value, typ, lazyDef))
    new IntermediateCodeUtils(newStmts, blockState, newIfStack, newElseStack, condStack)
  }

  /**
   * Adds an If statement to the code. Has to be closed with [[EndIf]].
   * Further generated statements are added inside the if.
   * @param cond The Ifs condition in DNF
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def If(cond: Seq[Seq[ImpLanExpr]]): IntermediateCodeUtils = {
    new IntermediateCodeUtils(
      stmts,
      Seq(InIf) ++ blockState,
      Seq(Seq()) ++ ifTryStack,
      Seq(Seq()) ++ elseCatchStack,
      Seq(cond) ++ condStack
    )
  }

  /**
   * Closes the innermost if and starts its else block.
   * Fails if there was no previous unclosed if block.
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def Else(): IntermediateCodeUtils = blockState.head match {
    case InIf   => new IntermediateCodeUtils(stmts, blockState.updated(0, InElse), ifTryStack, elseCatchStack, condStack)
    case InElse => throw Diagnostics.DSLError("Two subsequent Else blocks")
    case _      => throw Diagnostics.DSLError("Else without previous If")
  }

  /**
   * Closes the innermost if. Fails if there is none.
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def EndIf(): IntermediateCodeUtils = {
    val stmt = blockState.head match {
      case InIf | InElse => IntermediateCode.If(condStack.head, ifTryStack.head, elseCatchStack.head)
      case _             => throw Diagnostics.DSLError("EndIf without previous If")
    }
    val tmpRes = new IntermediateCodeUtils(
      stmts,
      blockState.drop(1),
      ifTryStack.drop(1),
      elseCatchStack.drop(1),
      condStack.drop(1)
    )
    val (newStmts, newIfStack, newElseStack) = tmpRes.addStmt(stmt)
    new IntermediateCodeUtils(newStmts, blockState.drop(1), newIfStack, newElseStack, condStack.drop(1))
  }

  /**
   * Adds a Try statement to the code. Has to be followed by [[Catch]] and closed with [[EndTry]].
   * Further generated statements are added inside the try.
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def Try(): IntermediateCodeUtils = {
    new IntermediateCodeUtils(
      stmts,
      Seq(InTry) ++ blockState,
      Seq(Seq()) ++ ifTryStack,
      Seq(Seq()) ++ elseCatchStack,
      condStack
    )
  }

  /**
   * Closes the innermost try and starts its catch block.
   * In the scope of the catch block a variable var_err exists which holds the thrown error.
   * Fails if there was no previous unclosed try block.
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def Catch(): IntermediateCodeUtils = blockState.head match {
    case InTry =>
      new IntermediateCodeUtils(stmts, blockState.updated(0, InCatch), ifTryStack, elseCatchStack, condStack)
    case InCatch => throw Diagnostics.DSLError("Two subseqeuent Catch blocks")
    case _       => throw Diagnostics.DSLError("Catch without previous Try")
  }

  /**
   * Closes the innermost try. Fails if there is none.
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def EndTry(): IntermediateCodeUtils = {
    val stmt = blockState.head match {
      case InTry | InCatch => IntermediateCode.TryCatchBlock(ifTryStack.head, elseCatchStack.head)
      case _               => throw Diagnostics.DSLError("EndTry without previous Try")
    }
    val tmpRes =
      new IntermediateCodeUtils(stmts, blockState.drop(1), ifTryStack.drop(1), elseCatchStack.drop(1), condStack)
    val (newStmts, newIfStack, newElseStack) = tmpRes.addStmt(stmt)
    new IntermediateCodeUtils(newStmts, blockState.drop(1), newIfStack, newElseStack, condStack)
  }

  /**
   * Add a function call to the generated code.
   * @param name The name of the function to be called
   * @param params The param expressions of the call
   * @param typeHint The type of the called function
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def FunctionCall(
    name: String,
    params: Seq[ImpLanExpr],
    typeHint: IntermediateCode.FunctionType
  ): IntermediateCodeUtils = {
    val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.FunctionCall(name, params, typeHint))
    new IntermediateCodeUtils(newStmts, blockState, newIfStack, newElseStack, condStack)
  }

  /**
   * Add a lambda call to the generated code.
   * @param exp The expression to be called
   * @param params The param expressions of the call
   * @return [[IntermediateCodeUtils]] representing the new state
   */
  def LambdaApplication(exp: ImpLanExpr, params: Seq[ImpLanExpr]): IntermediateCodeUtils = {
    val (newStmts, newIfStack, newElseStack) = addStmt(IntermediateCode.LambdaApplication(exp, params))
    new IntermediateCodeUtils(newStmts, blockState, newIfStack, newElseStack, condStack)
  }

  /**
   * Adds a statement to the right internal stack.
   * @param stmt The statement to be added
   * @return The modified stacks
   */
  private def addStmt(stmt: ImpLanStmt): (Seq[ImpLanStmt], Seq[Seq[ImpLanStmt]], Seq[Seq[ImpLanStmt]]) =
    blockState.head match {
      case InIf | InTry     => (stmts, ifTryStack.updated(0, ifTryStack.head :+ stmt), elseCatchStack)
      case InElse | InCatch => (stmts, ifTryStack, elseCatchStack.updated(0, elseCatchStack.head :+ stmt))
      case Out              => (stmts :+ stmt, ifTryStack, elseCatchStack)
    }

}

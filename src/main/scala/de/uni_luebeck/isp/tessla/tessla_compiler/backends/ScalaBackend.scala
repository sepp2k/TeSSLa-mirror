package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.tessla_compiler.{Errors, IntermediateCode, IntermediateCodeTypeInference}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

/**
  * BackendInterface implementation for the translation to Java code
  */
class ScalaBackend extends BackendInterface("de/uni_luebeck/isp/tessla/tessla_compiler/ScalaSkeleton.scala") {

  def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr])]) : Seq[String] = {
    vars.map{
      case (name, (typ, scala.Some(default))) => s"var $name : ${ScalaConstants.typeTranslation(typ)} = ${translateExpression(default)}"
      case (name, (typ, scala.None)) => s"var $name : ${ScalaConstants.typeTranslation(typ)} = null"
    }.toSeq
  }

  def foldGuard(guard: Seq[Seq[ImpLanExpr]]) : String = {
    guard.map{c => ("(" + c.map(translateExpression).mkString(" && ") + ")")}.mkString(" || ")
  }

  override def generateCode(stmts: Seq[ImpLanStmt]): String = generateCode(stmts, false)

  def generateCode(stmts: Seq[ImpLanStmt], inLambda: Boolean = false) : String = {
    stmts.map{
      case expr: IntermediateCode.ImpLanExpr => translateExpression(expr)
      case IntermediateCode.If(guard, stmts, elseStmts) => {
        val guardFormatted = foldGuard(guard)
        val ifPart = generateCode(stmts)
        val elsePart = generateCode(elseStmts)
        val optElse = if (elsePart == "") "" else s" else {\n$elsePart\n}"

        s"if ($guardFormatted) {\n$ifPart\n}$optElse"
      }
      case Assignment(lhs, rexpr, _, t) if inLambda => s"var $lhs :${ScalaConstants.typeTranslation(t)} = " +
        s"${translateExpression(rexpr)}${generateCastIfNecessary(t, IntermediateCodeTypeInference.typeInference(rexpr, variables.view.mapValues{case (typ, _) => typ}.toMap))}" //TODO include variable scopes
      case Assignment(lhs, rexpr, _, t) => s"$lhs = " +
        s"${translateExpression(rexpr)}${generateCastIfNecessary(t, IntermediateCodeTypeInference.typeInference(rexpr, variables.view.mapValues{case (typ, _) => typ}.toMap))}"

      case FinalAssignment(lhs, rhs, t) if inLambda => s"var $lhs : ${ScalaConstants.typeTranslation(t)} = " +
        s"${translateExpression(rhs)}${generateCastIfNecessary(t, IntermediateCodeTypeInference.typeInference(rhs, variables.view.mapValues{case (typ, _) => typ}.toMap))}"
      case FinalAssignment(_, _, _) => ""

      case IntermediateCode.ReturnStatement(expr) => translateExpression(expr)
    }.filter(_ != "").mkString("\n")
  }

  def generateCastIfNecessary(type1: ImpLanType, type2: ImpLanType) : String = {
    if (IntermediateCodeTypeInference.castingNecessary(type1, type2)) {
        s".asInstanceOf[${ScalaConstants.typeTranslation(type1)}]"
    } else {
      ""
    }
  }

  def translateExpression(e: ImpLanExpr) : String = e match {
      case lanVal: ImpLanVal => ScalaConstants.valueTranslation(lanVal)
      case IntermediateCode.FunctionCall(name, params, typeHint) => {
        if (name.startsWith("__")) {
          ScalaConstants.builtinFunctionCallTranslation(name, params.map(translateExpression), typeHint)
        } else {
          s"$name(${params.map(translateExpression).mkString(", ")})"
        }
      }
      case IntermediateCode.LambdaApplication(exp, params) => s"${translateExpression(exp)}.apply(${params.map(translateExpression).mkString(", ")})"
      case IntermediateCode.Addition(op1, op2) => s"${translateExpression(op1)} + ${translateExpression(op2)}"
      case IntermediateCode.BitwiseOr(op1, op2) => s"${translateExpression(op1)} | ${translateExpression(op2)}"
      case IntermediateCode.Subtraction(op1, op2) => s"${translateExpression(op1)} - ${translateExpression(op2)}"
      case IntermediateCode.TernaryExpression(guard, e1, e2) => s"if(${foldGuard(guard)}) { ${translateExpression(e1)} } else { ${translateExpression(e2)}}"
      case IntermediateCode.Equal(a, b) => {
        if (isObjectType(IntermediateCodeTypeInference.typeInference(a, variables.view.mapValues{case (typ, _) => typ}.toMap))) {
          s"${translateExpression(a)}.equals(${translateExpression(b)})"
        } else {
          s"${translateExpression(a)} == ${translateExpression(b)}"
        }
      }
      case IntermediateCode.NotEqual(a, b) => {
        if (isObjectType(IntermediateCodeTypeInference.typeInference(a, variables.view.mapValues{case (typ, _) => typ}.toMap))) {
          s"!${translateExpression(a)}.equals(${translateExpression(b)})"
        } else {
          s"${translateExpression(a)} != ${translateExpression(b)}"
        }
      }
      case IntermediateCode.Greater(a, b) => s"${translateExpression(a)} > ${translateExpression(b)}"
      case IntermediateCode.GreaterEqual(a, b) => s"${translateExpression(a)} >= ${translateExpression(b)}"
      case IntermediateCode.Negation(a) => s"!${translateExpression(a)}"
      case IntermediateCode.Variable(name) => name
      case IntermediateCode.LambdaExpression(argNames, argsTypes, _, body) => {
        val args = argsTypes.zip(argNames).map{case (t,n) => s"$n : ${ScalaConstants.typeTranslation(t)}"}.mkString(", ")
        s"($args) => {\n${generateCode(body, true)}\n}"
      }
  }

  def isObjectType(t: ImpLanType) : Boolean = {
    t match {
      case IntermediateCode.LongType |
           IntermediateCode.DoubleType |
           IntermediateCode.BoolType |
           IntermediateCode.UnitType => false
      case _ => true
    }
  }

}
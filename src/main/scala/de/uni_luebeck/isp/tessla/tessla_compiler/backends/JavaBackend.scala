package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.tessla_compiler.{Errors, IntermediateCode}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{Assignment, BoolType, FinalAssignment, ImpLanExpr, ImpLanStmt, ImpLanType, ImpLanVal}

class JavaBackend extends BackendInterface("de/uni_luebeck/isp/tessla/tessla_compiler/JavaSkeleton.java") {

  def generateVariableDeclarations() : Seq[String] = {
    variables.map{
      case (name, (typ, default)) => s"${JavaConstants.typeTranslation(typ)} $name = ${JavaConstants.valueTranslation(default)};"
    }.toSeq
  }

  def foldGuard(guard: Seq[Seq[ImpLanExpr]]) : String = {
    guard.map{c => ("(" + c.map(translateExpression(Some(BoolType))).mkString(" && ") + ")")}.mkString(" || ")
  }

  def generateCode(stmts: Seq[ImpLanStmt]) : String = {
    stmts.map{
      case expr: IntermediateCode.ImpLanExpr => s"${translateExpression(None)(expr)};"
      case IntermediateCode.If(guard, stmts, elseStmts) => {
        val guardFormatted = foldGuard(guard)
        val ifPart = generateCode(stmts)
        val elsePart = generateCode(elseStmts)
        val optElse = if (elsePart == "") "" else s" else {\n$elsePart\n}"

        s"if ($guardFormatted) {\n$ifPart\n}$optElse"
      }
      case Assignment(lhs, rexpr, _, typ) => s"$lhs = ${translateExpression(Some(typ))(rexpr)};"
      case FinalAssignment(_, _, _) => ""
      case IntermediateCode.ReturnStatement(expr) => s"return ${translateExpression(None)(expr)};"
    }.filter(_ != "").mkString("\n")
  }

  def translateExpression(resType: Option[ImpLanType])(e: ImpLanExpr) : String = e match {
      case lanVal: ImpLanVal => JavaConstants.valueTranslation(lanVal)
      case IntermediateCode.FunctionCall(name, params) => {
        if (name.startsWith("__")) {
          JavaConstants.builtinFunctionCallTranslation(name, params.map(translateExpression(None)), variables, resType)
        } else {
          s"$name(${params.map(translateExpression(None)).mkString(", ")})"
        }
      }
      case IntermediateCode.FunctionVarApplication(variable, params) => s"$variable.apply(${params.map(translateExpression(None)).mkString(", ")})"
      case IntermediateCode.Addition(op1, op2) => s"${translateExpression(None)(op1)} + ${translateExpression(None)(op2)}"
      case IntermediateCode.Subtraction(op1, op2) => s"${translateExpression(None)(op1)} - ${translateExpression(None)(op2)}"
      case IntermediateCode.TernaryExpression(guard, e1, e2) => s"${foldGuard(guard)} ? ${translateExpression(None)(e1)} : ${translateExpression(None)(e2)}"
      case IntermediateCode.Equal(a, b) => {
        if (isObjectType(IntermediateCode.typeInference(a, variables.mapValues{case (typ, _) => typ}))) {
          s"${translateExpression(None)(a)}.equals(${translateExpression(None)(b)})"
        } else {
          s"${translateExpression(None)(a)} == ${translateExpression(None)(b)}"
        }
      }
      case IntermediateCode.NotEqual(a, b) => s"${translateExpression(None)(a)} != ${translateExpression(None)(b)}"
      case IntermediateCode.Greater(a, b) => s"${translateExpression(None)(a)} > ${translateExpression(None)(b)}"
      case IntermediateCode.GreaterEqual(a, b) => s"${translateExpression(None)(a)} >= ${translateExpression(None)(b)}"
      case IntermediateCode.Negation(a) => s"!${translateExpression(None)(a)}}"
      case IntermediateCode.Variable(name) => name
      case IntermediateCode.LambdaExpression(argNames, argsTypes, _, body) => {
        val args = argsTypes.zip(argNames).map{case (t,n) => s"${JavaConstants.typeTranslation(t)} $n"}.mkString(", ")
        s"($args) -> {${generateCode(body)}}"
      }
  }

  def isObjectType(t: ImpLanType) = {
    t match {
      case IntermediateCode.LongType |
           IntermediateCode.DoubleType |
           IntermediateCode.BoolType |
           IntermediateCode.UnitType => false
      case _ => true
    }
  }

}

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.BackendInterface
import de.uni_luebeck.isp.tessla.tessla_compiler.{IntermediateCodeTypeInference, IntermediateCodeUtils}

/**
  * BackendInterface implementation for the translation to Java code
  */
class ScalaBackend extends BackendInterface("de/uni_luebeck/isp/tessla/tessla_compiler/ScalaSkeleton.scala") {

  override def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)]): Seq[String] = {
    generateVariableDeclarationsWithScopeInfo(vars)
  }

  def generateVariableDeclarationsWithScopeInfo(vars: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)], outScope: Boolean = true): Seq[String] = {
    vars.flatMap {
      case (_, (_, _, true)) if !outScope => scala.None
      case (name, (typ, scala.Some(default), _)) => scala.Some(s"var $name : ${ScalaConstants.typeTranslation(typ)} = ${translateExpression(default)}")
      case (name, (typ, scala.None, _)) => scala.Some(s"var $name : ${ScalaConstants.typeTranslation(typ)} = null")
    }.toSeq
  }

  def foldGuard(guard: Seq[Seq[ImpLanExpr]]): String = {
    if (guard.flatten.isEmpty) {
      "true"
    } else {
      guard.map { c => ("(" + (if (c.isEmpty) "false" else c.map(translateExpression).mkString(" && ")) + ")") }.mkString(" || ")
    }
  }

  override def generateCode(stmts: Seq[ImpLanStmt]): String = {
    stmts.map {
      case expr: ImpLanExpr => translateExpression(expr)
      case If(guard, stmts, elseStmts) => {
        val guardFormatted = foldGuard(guard)
        val ifPart = generateCode(stmts)
        val elsePart = generateCode(elseStmts)
        val optElse = if (elsePart == "") "" else s" else {\n$elsePart\n}"

        s"if ($guardFormatted) {\n$ifPart\n}$optElse"
      }
      case TryCatchBlock(tr, cat) =>
        s"try {\n${generateCode(tr)}\n} catch {\ncase var_err : Throwable => {\n${generateCode(cat)}\n}\n}"

      case Assignment(lhs, rexpr, _, t, _) => s"$lhs = " +
        s"${translateExpression(rexpr)}"
      case FinalAssignment(_, _, _) => ""

      case ReturnStatement(expr) => translateExpression(expr)
    }.filter(_ != "").mkString("\n")
  }

  def translateExpression(e: ImpLanExpr): String = e match {
      case lanVal: ImpLanVal => ScalaConstants.valueTranslation(lanVal)
      case FunctionCall(name, params, typeHint) => {
        if (name.startsWith("__")) {
          ScalaConstants.builtinFunctionCallTranslation(name, params.map(translateExpression), typeHint)
        } else {
          s"$name(${params.map(translateExpression).mkString(", ")})"
        }
      }
      case LambdaApplication(exp, params) => s"${translateExpression(exp)}.apply(${params.map(translateExpression).mkString(", ")})"
      case TernaryExpression(guard, e1, e2) => s"if(${foldGuard(guard)}) {${translateExpression(e1)}} else {${translateExpression(e2)}}"
      case Equal(a, b) => {
        if (isObjectType(IntermediateCodeTypeInference.typeInference(a, variables.view.mapValues { case (typ, _, _) => typ }.toMap))) {
          s"${translateExpression(a)}.equals(${translateExpression(b)})"
        } else {
          s"${translateExpression(a)} == ${translateExpression(b)}"
        }
      }
      case LambdaExpression(argNames, argsTypes, _, body) => {
        val args = argsTypes.zip(argNames).map { case (t, n) => s"$n : ${ScalaConstants.typeTranslation(t)}" }.mkString(", ")
        s"(($args) => {\n${generateVariableDeclarationsWithScopeInfo(IntermediateCodeUtils.getVariableMap(body, Map(), false), false).mkString("\n")}\n${generateCode(body)}\n})"
      }
      case Variable(name) => name
      case CastingExpression(e, t) => s"(${translateExpression(e)}).asInstanceOf[${ScalaConstants.typeTranslation(t)}]"
    }

  def isObjectType(t: ImpLanType) : Boolean = {
    t match {
      case LongType |
           DoubleType |
           BoolType |
           UnitType => false
      case _ => true
    }
  }
}

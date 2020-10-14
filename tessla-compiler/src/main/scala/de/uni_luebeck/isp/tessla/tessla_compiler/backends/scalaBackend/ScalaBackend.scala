/*
 * Copyright 2020 The TeSSLa Community
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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.BackendInterface
import de.uni_luebeck.isp.tessla.tessla_compiler.{
  IntermediateCode,
  IntermediateCodeTypeInference,
  IntermediateCodeUtils
}

/**
 * [[BackendInterface]] implementation for the translation to Scala code
 *
 * @param userIncludes Additional user-specific code which is included on top of the generated scala source.
 *                     Can be used e.g. for additional includes
 */
class ScalaBackend(userIncludes: String = "")
    extends BackendInterface("de/uni_luebeck/isp/tessla/tessla_compiler/ScalaSkeleton.scala", userIncludes) {

  /**
   * Translates a map of variables with base information (type, default value, lazy assignment) to corresponding
   * variable declarations in Scala.
   *
   * @param vars Map of variables to be translated: Name -> (Type x Default value x Lazy assignment)
   * @return Sequence of variable definitions in Scala
   */
  def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr], Boolean)]): Seq[String] = {
    def prefix(lazyDef: Boolean) = if (lazyDef) "lazy val" else "var"

    vars.toSeq.sortWith { case (_, b) => b._2._3 }.map {
      case (name, (typ, Some(default), lazyDef)) =>
        s"${prefix(lazyDef)} $name : ${ScalaConstants.typeTranslation(typ)} = ${translateExpression(default)}"
      case (name, (typ, None, lazyDef)) =>
        s"${prefix(lazyDef)} $name : ${ScalaConstants.typeTranslation(typ)} = null"
    }
  }

  /**
   * Translate guard given as DNF of expressions to equivalent Scala expression
   * @param guard The guard in DNF
   * @return Translation of the guard in Scala
   */
  private def foldGuard(guard: Seq[Seq[ImpLanExpr]]): String = {
    guard.map("(" + _.map(translateExpression).mkString(" && ") + ")").mkString(" || ")
  }

  /**
   * Translates a sequence of [[IntermediateCode.ImpLanStmt]] to the corresponding code in Scala.
   *
   * @param stmts The sequence of statements to be translated.
   * @return The generated code in Scala
   */
  override def generateCode(stmts: Seq[ImpLanStmt]): String = {
    stmts
      .map {
        case expr: ImpLanExpr => translateExpression(expr)
        case If(guard, stmts, elseStmts) =>
          val guardFormatted = foldGuard(guard)
          val ifPart = generateCode(stmts)
          val elsePart = generateCode(elseStmts)
          val optElse = if (elsePart == "") "" else s" else {\n$elsePart\n}"

          s"if ($guardFormatted) {\n$ifPart\n}$optElse"
        case TryCatchBlock(tr, cat) =>
          s"try {\n${generateCode(tr)}\n} catch {\ncase var_err : Throwable => {\n${generateCode(cat)}\n}\n}"

        case Assignment(lhs, rexpr, _, _) =>
          s"$lhs = " + s"${translateExpression(rexpr)}"
        case FinalAssignment(_, _, _, _) => ""

        case ReturnStatement(expr) => translateExpression(expr)
      }
      .filter(_ != "")
      .mkString("\n")
  }

  /**
   * Translates an [[ImpLanExpr]] to the corresponding code in Scala.
   *
    * @param e The expression of statements to be translated.
   * @return The generated code in Scala
   */
  private def translateExpression(e: ImpLanExpr): String = e match {
    case lanVal: ImpLanVal => ScalaConstants.valueTranslation(lanVal)
    case FunctionCall(name, params, typeHint) =>
      if (name.startsWith("__")) {
        ScalaConstants.builtinFunctionCallTranslation(name, params, translateExpression, typeHint)
      } else {
        s"$name(${params.map(translateExpression).mkString(", ")})"
      }
    case LambdaApplication(exp, params) =>
      s"${translateExpression(exp)}.apply(${params.map(translateExpression).mkString(", ")})"
    case TernaryExpression(guard, e1, e2) =>
      s"(if(${foldGuard(guard)}) {${translateExpression(e1)}} else {${translateExpression(e2)}})"
    case Equal(a, b) =>
      if (
        needsObjectCompare(
          IntermediateCodeTypeInference.typeInference(a, variables.view.mapValues { case (typ, _, _) => typ }.toMap)
        )
      ) {
        s"${translateExpression(a)}.equals(${translateExpression(b)})"
      } else {
        s"${translateExpression(a)} == ${translateExpression(b)}"
      }
    case LambdaExpression(argNames, argsTypes, _, body) =>
      val args =
        argsTypes.zip(argNames).map { case (t, n) => s"$n : ${ScalaConstants.typeTranslation(t)}" }.mkString(", ")
      s"(($args) => {\n${generateVariableDeclarations(IntermediateCodeUtils.getVariableMap(body)).mkString("\n")}\n${generateCode(body)}\n})"
    case Variable(name) => name

    case CastingExpression(e, f, t) if f == t => translateExpression(e)
    case CastingExpression(e, LazyContainer(f), t) =>
      s"(${translateExpression(e)})()" +
        (if (f != t) s".asInstanceOf[${ScalaConstants.typeTranslation(t)}]" else "")
    case CastingExpression(e, f, LazyContainer(t)) => s"() => {${translateExpression(CastingExpression(e, f, t))}}"
    case CastingExpression(e, _, t)                => s"(${translateExpression(e)}).asInstanceOf[${ScalaConstants.typeTranslation(t)}]"
  }

  /**
   * Decides whether .equals or == is used for comparison
   * @param t Type of objects which are compared
   * @return true if .equals is needed for comparison
   */
  private def needsObjectCompare(t: ImpLanType): Boolean = {
    t match {
      case LongType | DoubleType | BoolType | UnitType | ErrorType => false
      case _                                                       => true
    }
  }
}

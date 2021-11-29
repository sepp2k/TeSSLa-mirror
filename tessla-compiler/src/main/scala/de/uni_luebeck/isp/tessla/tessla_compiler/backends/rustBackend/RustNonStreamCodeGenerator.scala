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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.{
  DefinitionOrdering,
  Diagnostics,
  ExtendedSpecification,
  NonStreamCodeGeneratorInterface
}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils.{
  FinalDeclaration,
  FinalLazyDeclaration,
  VariableDeclaration
}

class RustNonStreamCodeGenerator(extSpec: ExtendedSpecification)
    extends NonStreamCodeGeneratorInterface[String, String](extSpec) {

  /**
   * Translates a function application to Rust
   * @param e The function expression which is applied
   * @param args The argument expressions of the application
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function application
   */
  override def translateFunctionCall(
    e: ExpressionArg,
    args: Seq[String],
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): String = {
    e match {
      case TypeApplicationExpression(app, types, _) =>
        translateFunctionCall(app, args, tm.typeApp(types), defContext)
      case ExternExpression(name, typ: Core.FunctionType, _) =>
        val newTm = tm.parsKnown(typ.typeParams)
        val resultType = RustUtils.convertType(typ.resultType.resolve(newTm.resMap))
        val paramTypes = typ.paramTypes.map { case (_, t) => RustUtils.convertType(t.resolve(newTm.resMap)) }
        // TODO find out if we possibly need to type-cast params or the expression to match the expected signature
        RustUtils.translateBuiltinFunctionCall(s"__${name}__", args, typ)
      case e: ExternExpression => translateExtern(e, tm, defContext)
      case _: FunctionExpression | _: ExpressionRef | _: ApplicationExpression | _: RecordAccessorExpression =>
        s"${translateExpressionArg(e, tm, defContext)}(${args.mkString(", ")})"
      case e =>
        throw Diagnostics.CoreASTError("Function call to expression of wrong type cannot be translated", e.location)
    }
  }

  /**
   * Translates an assignment from TeSSLa Core into Rust
   * @param id The id which is assigned
   * @param e The expression assigned to id
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated assignment
   */
  override def translateAssignment(
    id: Identifier,
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): String = {
    val resTpe = e.tpe.resolve(tm.resMap) // TODO do we ever need to specify this explicitly?
    val lazyVar = extSpec.lazyVars.get.contains(id)
    if (lazyVar) {
      definedIdentifiers += (id -> FinalLazyDeclaration)
      val inlinedExp = inlineVars(e, defContext)
      // TODO how exactly do we handle this
      s"let lazy_var_$id = ${translateExpressionArg(inlinedExp, tm, defContext)};"
    } else if (finalAssignmentPossible(e)) {
      definedIdentifiers += (id -> FinalDeclaration)
      s"let var_$id = ${translateExpressionArg(e, tm, defContext)};"
    } else {
      definedIdentifiers += (id -> VariableDeclaration)
      s"let mut var_$id = ${translateExpressionArg(e, tm, defContext)};"
    }
  }

  /**
   * Translates a TeSSLa Core FunctionExpression to a Rust expression
   * @param e The function to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function expression
   */
  override def translateFunction(
    e: FunctionExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): String = {
    definedIdentifiers ++= e.params.map(_._1 -> FinalDeclaration)
    val newTm = tm.parsKnown(e.typeParams)
    val arguments = e.params
      .map {
        case (id, StrictEvaluation, tpe) => if (id.fullName == "_") "_" else s"var_$id"
        case (id, LazyEvaluation, tpe)   => if (id.fullName == "_") "_" else s"lazy_var_$id" // TODO how to handle lazy...
      }
      .mkString(", ")
    s"|$arguments| {\n" +
      s"${translateBody(e.body, e.result, newTm, defContext).mkString("\n")}" +
      s"\n}"
  }

  /**
   * Translates a block of statements with return expression (i.e. the body of a lambda) to Rust statements
   * @param body The sequence of statements to be translated
   * @param ret The return expression of the block
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated block.
   */
  override def translateBody(
    body: Map[Identifier, DefinitionExpression],
    ret: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): Seq[String] = {
    val newDefContext = defContext ++ body

    val translatedBody = DefinitionOrdering.order(body) map {
      case (id, exp) => translateAssignment(id, exp, tm, newDefContext)
    }
    translatedBody :+ s"return ${translateExpressionArg(ret, tm, newDefContext)};"
  }

  /**
   * Translates an ExternExpression. If the extern is of function type a lambda expression is wrapped around it.
   * If the extern is directly applied this lambda is most likely unnecessary and this function should not be
   * used for translation of the called extern.
   * @param e          The[[ExternExpression]]
   * @param tm         The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated expression
   */
  override def translateExtern(
    e: ExternExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): String = {
    e match {
      case ExternExpression("true", _, _) =>
        "true"
      case ExternExpression("false", _, _) =>
        "false"
      case ExternExpression("None", InstantiatedType("Option", _, _), _) =>
        "None"
      case ExternExpression(_, typ: Core.FunctionType, _) =>
        val newTm = tm.parsKnown(typ.typeParams)
        val argNames = typ.paramTypes.indices.map(i => s"tLPar_$i")
        val argTypes = typ.paramTypes
          .map(_._2.resolve(newTm.resMap))
          .map(RustUtils.convertType)
          .zip(typ.paramTypes.map(_._1))
          .map {
            case (t, StrictEvaluation) => t
            case (t, LazyEvaluation)   => s"Lazy($t)" // TODO lazy again...
          }
        val ret = translateFunctionCall(e, argNames, newTm, defContext)
        s"|${argNames.mkString(", ")}|{ return $ret }"
    }
  }

  /**
   * Translates an ExpressionArg to a corresponding Rust expression
   * @param e The expression to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @return The translated expression
   */
  override def translateExpressionArg(
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): String = {
    e match {
      case TypeApplicationExpression(e, tArgs, _) =>
        translateExpressionArg(e, tm.typeApp(tArgs), defContext)
      case f: FunctionExpression =>
        translateFunction(f, tm, defContext)
      case ApplicationExpression(e, args, _) =>
        translateFunctionCall(
          e,
          getInlinedArgs(args, defContext).map(translateExpressionArg(_, tm, defContext)),
          tm,
          defContext
        )
      case StringLiteralExpression(value, _) =>
        // FIXME: it would be nice to standardise this functionality somewhere...
        s"""\"${value.replace("\"", "\\\"").replace("$", "\\$")}\""""
      case IntLiteralExpression(value, _) =>
        s"${value.toLong}_i64"
      case FloatLiteralExpression(value, _) =>
        s"${value}_f64"
      case ExpressionRef(id, Core.FunctionType(_, _, _, _), _) =>
        s"fun_${id.fullName}"
      case ExpressionRef(id, _, _) =>
        s"var_${id.fullName}"
      case x: ExternExpression =>
        translateExtern(x, tm, defContext)
      case RecordConstructorExpression(entries, _) if entries.isEmpty =>
        "()" // Unit value
      case RecordConstructorExpression(entries, _) =>
        s"{ ${entries.toSeq
          .map { case (name, (ea, _)) => s"$name: ${translateExpressionArg(ea, tm, defContext)}" }
          .mkString(", ")} }"
      case RecordAccessorExpression(name, target, _, _) =>
        s"${translateExpressionArg(target, tm, defContext)}.$name"
      case _ =>
        throw Diagnostics.CoreASTError("Unexpected ExpressionArg cannot be translated", e.location)
    }
  }
}

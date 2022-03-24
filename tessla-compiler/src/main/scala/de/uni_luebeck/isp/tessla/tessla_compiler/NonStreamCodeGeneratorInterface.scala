/*
 * Copyright 2022 The TeSSLa Community
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

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils._

/**
 * Class for the translation of TeSSLa expressions of non-stream type
 */
abstract class NonStreamCodeGeneratorInterface[ExpressionType, StatementType](extSpec: ExtendedSpecification) {

  /**
   * Set of identifiers which are assigned with final assignments.
   * If an assignment uses exclusively existing assignments it can also be finally assigned
   * Otherwise (in case of recursion) it must be assigned with a dummy default value first
   */
  protected var definedIdentifiers: Map[Identifier, DeclarationType] = Map()

  /**
   * Translates a function application to the target representation
   * @param e The function expression which is applied
   * @param args The argument expressions of the application
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function application
   */
  def translateFunctionCall(
    e: ExpressionArg,
    args: Seq[ExpressionType],
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): ExpressionType

  /**
   * Translates an assignment from TeSSLa Core into the target representation
   * @param id The id which is assigned
   * @param e The expression assigned to id
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated assignment
   */
  def translateAssignment(
    id: Identifier,
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): StatementType

  /**
   * Translates a TeSSLa Core FunctionExpression to an expression
   * @param e The function to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function expression
   */
  def translateFunction(
    e: FunctionExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): ExpressionType

  /**
   * Translates a block of statements with return expression (i.e. the body of a lambda) to statements
   * @param body The sequence of statements to be translated
   * @param ret The return expression of the block
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated block. Contains a [[IntermediateCode.ReturnStatement]] at the end
   */
  def translateBody(
    body: Map[Identifier, DefinitionExpression],
    ret: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): Seq[StatementType]

  /**
   *
   * @param e          Translates an ExternExpression. If the extern is of function type a lambda expression is wrapped around it.
   *                   If the extern is directly applied this lambda is most likely unnecessary and this function should not be
   *                   used for translation of the called extern.
   * @param tm         The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated expression
   */
  def translateExtern(
    e: ExternExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): ExpressionType

  /**
   * Translates an ExpressionArg to a corresponding expression
   * @param e The expression to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated expression
   */
  def translateExpressionArg(
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): ExpressionType

  /**
   * Inlines all references in a function application's parameters according to inlining information from [[ExtendedSpecification]]
   * @param args The arguments applied to e
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The modified parameter expressions
   */
  protected def getInlinedArgs(
    args: Seq[ExpressionArg],
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): Seq[ExpressionArg] = {
    args.map(e => inlineVars(e, defContext))
  }

  /**
   * Inlines all [[ExpressionRef]] (variable references) to variables in the inlining set of the [[ExtendedSpecification]] in an
   * [[ExpressionArg]] except those of function type
   * @param e The expression where variable references should be inlined
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The modified expression
   */
  protected def inlineVars(e: ExpressionArg, defContext: Map[Identifier, DefinitionExpression]): ExpressionArg = {
    e match {
      case e: Expression => inlineVars(e, defContext)
      case ExpressionRef(id, tpe, _)
          if defContext.contains(id) && !tpe.isInstanceOf[Core.FunctionType] && extSpec.inlining.get.contains(id) =>
        inlineVars(defContext(id), defContext)
      case _ => e
    }
  }

  /**
   * Inlines all [[ExpressionRef]] (variable references) to variables in the inlining set of the [[ExtendedSpecification]] in a
   * [[DefinitionExpression]] except those of function type
   * @param e The expression where variable references should be inlined
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The modified expression
   */
  protected def inlineVars(
    e: DefinitionExpression,
    defContext: Map[Identifier, DefinitionExpression]
  ): DefinitionExpression = {
    e match {
      case e: Expression =>
        e match {
          case FunctionExpression(typeParams, params, body, result, location) =>
            FunctionExpression(
              typeParams,
              params,
              inlineVarsBody(body, defContext),
              inlineVars(result, defContext),
              location
            )
          case ApplicationExpression(applicable, args, location) =>
            ApplicationExpression(
              inlineVars(applicable, defContext),
              args.map { a => inlineVars(a, defContext) },
              location
            )
          case TypeApplicationExpression(applicable, typeArgs, location) =>
            TypeApplicationExpression(inlineVars(applicable, defContext), typeArgs, location)
          case RecordConstructorExpression(entries, location) =>
            RecordConstructorExpression(
              entries.map { case (n, (e, l)) => (n, (inlineVars(e, defContext), l)) },
              location
            )
          case RecordAccessorExpression(name, target, nameLoc, location) =>
            RecordAccessorExpression(name, inlineVars(target, defContext), nameLoc, location)
          case _ => e
        }
      case _ => e
    }
  }

  /**
   * Checks whether a given expression only uses identifiers from [[definedIdentifiers]], i.e. those already defined
   * @param e Expression to be examined
   * @return  Whether all subexpressions are already defined and and the expression can hence be evaluated
   */
  protected def finalAssignmentPossible(e: ExpressionArg): Boolean = {
    //Boolean parameter indicates whether variable already has to be evaluateable
    def getSubIDs(e: ExpressionArg, ignore: Set[Identifier] = Set()): Set[(Identifier, Boolean)] = e match {
      case FunctionExpression(_, params, body, result, _) =>
        (body.values.toSeq :+ result)
          .map(getSubIDs(_, ignore ++ params.map(_._1) ++ body.keys))
          .reduce(_ ++ _)
          .map(e => (e._1, false))
      case ApplicationExpression(applicable, args, _) =>
        (args :+ applicable).map(getSubIDs(_, ignore)).reduce(_ ++ _)
      case TypeApplicationExpression(applicable, _, _) => getSubIDs(applicable, ignore)
      case RecordConstructorExpression(entries, _) =>
        entries.map(e => getSubIDs(e._2._1, ignore)).fold(Set()) { case (x, y) => x ++ y }
      case RecordAccessorExpression(_, target, _, _)       => getSubIDs(target, ignore)
      case ExpressionRef(id, _, _) if !ignore.contains(id) => Set((id, true))
      case _                                               => Set()
    }

    getSubIDs(e).forall(d =>
      definedIdentifiers.contains(d._1) && (!d._2 || definedIdentifiers(d._1) != VariableDeclaration)
    )
  }

  /**
   * Applies [[inlineVars]] on all statements in body
   * @param body Sequence of statements where [[inlineVars]] is applied on
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The modified body
   */
  protected def inlineVarsBody(
    body: Map[Identifier, DefinitionExpression],
    defContext: Map[Identifier, DefinitionExpression]
  ): Map[Identifier, DefinitionExpression] = {
    body.map { case (id, de) => (id, inlineVars(de, defContext)) }
  }
}

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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.preprocessing

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.StrictEvaluation
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics

import scala.collection.immutable.{ArraySeq, Map}
import scala.collection.mutable

/**
 * In this phase, nested functions are unwrapped so that they can call themselves recursively.
 */
class ExtractAndWrapFunctions extends TranslationPhase[Specification, Specification] {

  private val extractedFunctions = mutable.Map.empty[Identifier, (Identifier, FunctionExpression)]

  private val knownNamedFunctions = mutable.Set.empty[Identifier]

  override def translate(spec: Specification): TranslationPhase.Result[Specification] = {
    val globalScope = spec.definitions.keySet

    spec.definitions.foreach {
      case (id, definition) =>
        if (definition.isInstanceOf[FunctionExpression]) {
          knownNamedFunctions.addOne(id)
        }
        findFunctionNames(definition)
    }

    val definitions = spec.definitions.map {
      case (id, definition) =>
        val expression = extractFunctionExpressions(definition, globalScope, Set(), id.fullName)
        id -> expression.asInstanceOf[DefinitionExpression]
    }

    val remappedDefinitions = (definitions ++ extractedFunctions.values).map {
      case (id, definition) =>
        val expression = remapFunctionApplications(definition, Set())
        id -> expression.asInstanceOf[DefinitionExpression]
    }

    Success(Specification(spec.annotations, spec.in, remappedDefinitions, spec.out, spec.maxIdentifier), Seq())
  }

  /**
   * Traverses an expression recursively and stores the names of all named functions in [[knownNamedFunctions]].
   * @param e The expression to evaluate.
   */
  private def findFunctionNames(
    e: ExpressionArg
  ): Unit = e match {
    case FunctionExpression(_, _, body, result, _) =>
      knownNamedFunctions.addAll(body.flatMap {
        case (id, definition) => if (definition.isInstanceOf[FunctionExpression]) Some(id) else None
      })
      body.foreach { case (_, definition) => findFunctionNames(definition) }
      findFunctionNames(result)
    case ApplicationExpression(applicable, args, _) =>
      findFunctionNames(applicable)
      args.foreach { e => findFunctionNames(e) }
    case TypeApplicationExpression(applicable, _, _) =>
      findFunctionNames(applicable)
    case RecordConstructorExpression(entries, _) =>
      entries.foreach { case (_, (expr, _)) => findFunctionNames(expr) }
    case RecordAccessorExpression(_, target, _, _) =>
      findFunctionNames(target)

    case _: ExpressionRef | _: ExternExpression | _: StringLiteralExpression | _: IntLiteralExpression |
        _: FloatLiteralExpression =>
  }

  /**
   * Traverses an expression recursively and collects a list of all [[Identifier]]s,
   * that are not accessible in the current scope.
   * @param e The expression to evaluate.
   * @param currentScope Contains all identifier that are accessible in the local and global scope.
   * @param outerScope Contains all identifier that aren't in currentScope.
   * @return A list of all [[Identifier]]s and their types that are not accessible in the current scope.
   */
  private def findClosureArgs(
    e: ExpressionArg,
    currentScope: Set[Identifier],
    outerScope: Set[Identifier]
  ): Set[(Identifier, Evaluation, Type)] = e match {
    case ExpressionRef(id, _, _) if currentScope.contains(id) => Set()
    case ExpressionRef(id, tpe, _) if outerScope.contains(id) => Set((id, StrictEvaluation, tpe))

    case ref: ExpressionRef => throw Diagnostics.CoreASTError(s"ExpressionRef('${ref.id}') was not found in any scope")

    case FunctionExpression(_, params, body, result, _) =>
      val functionScope = currentScope ++ params.map { case (id, _, _) => id } ++ body.map { case (id, _) => id }
      body.flatMap {
        case (_, definition) => findClosureArgs(definition, functionScope, outerScope)
      }.toSet ++ findClosureArgs(result, functionScope, outerScope)
    case ApplicationExpression(applicable, args, _) =>
      findClosureArgs(applicable, currentScope, outerScope) ++ args.flatMap { e =>
        findClosureArgs(e, currentScope, outerScope)
      }
    case TypeApplicationExpression(applicable, _, _) =>
      findClosureArgs(applicable, currentScope, outerScope)
    case RecordConstructorExpression(entries, _) =>
      entries.flatMap { case (_, (expr, _)) => findClosureArgs(expr, currentScope, outerScope) }.toSet
    case RecordAccessorExpression(_, target, _, _) =>
      findClosureArgs(target, currentScope, outerScope)

    case _: ExternExpression | _: StringLiteralExpression | _: IntLiteralExpression | _: FloatLiteralExpression => Set()
  }

  /**
   * Recursively traverse through a DefinitionExpression and extract all [[FunctionExpression]]s into the global scope.
   * If the function in question requires data from the surrounding scope, we add those as parameters to a function
   * that then returns a closure with them captured.
   * The extracted global function name is a concatenation of all names of the extracted functions parents.
   * The original and the new global name of each extracted function is stored in [[extractedFunctions]].
   *
   * @param e The expression to evaluate.
   * @param currentScope Contains all identifier that are accessible in the local and global scope.
   * @param outerScope Contains all identifier that aren't in currentScope.
   * @param definitionPath The concatenation of all parent function names.
   * @return The modified [[Expression]].
   */
  def extractFunctionExpressions(
    e: ExpressionArg,
    currentScope: Set[Identifier],
    outerScope: Set[Identifier],
    definitionPath: String
  ): ExpressionArg = {
    e match {
      case FunctionExpression(typeParams, params, body, result, location) =>
        val functionScope = (params.map { case (id, _, _) => id } ++ body.map { case (id, _) => id }).toSet
        val functionOuterScope = outerScope ++ currentScope
        val functionExpr = FunctionExpression(
          typeParams,
          params,
          body.flatMap {
            case (id, definition) =>
              val modifiedPath = s"${definitionPath}_${id.fullName}"
              extractFunctionExpressions(
                definition,
                functionScope,
                functionOuterScope,
                modifiedPath
              ) match {
                case modifiedFunction: FunctionExpression =>
                  // extract this inner function definition into global scope, mapping id -> modifiedPath
                  extractedFunctions += id -> (Identifier(modifiedPath), modifiedFunction)
                  None
                case modifiedDefinition =>
                  Some(id -> modifiedDefinition.asInstanceOf[DefinitionExpression])
              }
          },
          extractFunctionExpressions(result, functionScope, functionOuterScope, s"${definitionPath}_return"),
          location
        )
        // find all ExpressionRefs that do not refer to something in current+global scope
        val closureArgs = findClosureArgs(e, functionScope, functionOuterScope).filterNot {
          case (id, _, _) => knownNamedFunctions.contains(id)
        }
        if (closureArgs.isEmpty) {
          functionExpr
        } else {
          // this function takes all externally scoped arguments, and moves them into a box around the inner function
          FunctionExpression(
            List(),
            closureArgs.toList,
            Map(),
            functionExpr
          )
        }

      /**
       * def mult(x: Int): (Int) => Int = {
       *   def prod(a: Int): Int = if a < 1 then x else prod(a - 1) * x
       *   prod
       * }
       *
         * mult_prod(a: Int, x: Int)
       */
      // global name: outer_inner__(..params, ..outer_params) {

      case ApplicationExpression(applicable, args, location) =>
        ApplicationExpression(
          extractFunctionExpressions(applicable, currentScope, outerScope, definitionPath),
          args.map { exprArg =>
            extractFunctionExpressions(exprArg, currentScope, outerScope, definitionPath)
          },
          location
        )
      case TypeApplicationExpression(applicable, typeArgs, location) =>
        TypeApplicationExpression(
          extractFunctionExpressions(applicable, currentScope, outerScope, definitionPath),
          typeArgs,
          location
        )
      case RecordAccessorExpression(name, target, nameLocation, location) =>
        RecordAccessorExpression(
          name,
          extractFunctionExpressions(target, currentScope, outerScope, definitionPath),
          nameLocation,
          location
        )
      case RecordConstructorExpression(entries, location) =>
        RecordConstructorExpression(
          entries.map {
            case (name, (exprArg, location)) =>
              name -> (extractFunctionExpressions(exprArg, currentScope, outerScope, definitionPath), location)
          },
          location
        )

      case ref: ExpressionRef              => ref
      case extern: ExternExpression        => extern
      case float: FloatLiteralExpression   => float
      case int: IntLiteralExpression       => int
      case string: StringLiteralExpression => string
    }
  }

  /**
   * Recursively traverses the specified expression and replaces [[ExpressionRef]]s to extracted functions with
   * either a direct reference to the global function or with a call to the global function that passes in required
   * scope variables.
   *
   * @param e The expression to evaluate.
   * @param outerScope Contains all identifier that aren't in currentScope.
   * @return The modified expression.
   */
  def remapFunctionApplications(
    e: ExpressionArg,
    outerScope: Set[Identifier]
  ): ExpressionArg = {
    e match {
      case ExpressionRef(id, typ: FunctionType, location) =>
        extractedFunctions.get(id) match {
          case Some((extractedId, FunctionExpression(_, params, body, result, location)))
              if body.isEmpty && result.tpe.isInstanceOf[FunctionType] =>
            // If the function body is empty, but it returns a function, it is one of our functions that boxes some scope
            ApplicationExpression(
              ExpressionRef(
                extractedId,
                FunctionType(
                  List(),
                  params.map { case (_, eval, typ) => (eval, typ) },
                  result.tpe
                )
              ),
              ArraySeq.from(params.map { case (id, _, typ) => ExpressionRef(id, typ) }),
              location
            )
          case Some((extractedId, function)) =>
            // Otherwise it is just extracted from its original location and can be referred to by reference
            ExpressionRef(extractedId, function.tpe, function.location)
          case None =>
            ExpressionRef(id, typ, location)
        }

      case FunctionExpression(typeParams, params, body, result, location) =>
        FunctionExpression(
          typeParams,
          params,
          body.map {
            case (id, definition) =>
              id -> remapFunctionApplications(definition, outerScope).asInstanceOf[DefinitionExpression]
          },
          remapFunctionApplications(result, outerScope),
          location
        )
      case ApplicationExpression(applicable, args, location) =>
        ApplicationExpression(
          remapFunctionApplications(applicable, outerScope),
          args.map { exprArg =>
            remapFunctionApplications(exprArg, outerScope)
          },
          location
        )
      case TypeApplicationExpression(applicable, typeArgs, location) =>
        TypeApplicationExpression(
          remapFunctionApplications(applicable, outerScope),
          typeArgs,
          location
        )
      case RecordAccessorExpression(name, target, nameLocation, location) =>
        RecordAccessorExpression(
          name,
          remapFunctionApplications(target, outerScope),
          nameLocation,
          location
        )
      case RecordConstructorExpression(entries, location) =>
        RecordConstructorExpression(
          entries.map {
            case (name, (exprArg, location)) =>
              name -> (remapFunctionApplications(exprArg, outerScope), location)
          },
          location
        )

      case ref: ExpressionRef              => ref
      case extern: ExternExpression        => extern
      case float: FloatLiteralExpression   => float
      case int: IntLiteralExpression       => int
      case string: StringLiteralExpression => string
    }
  }
}

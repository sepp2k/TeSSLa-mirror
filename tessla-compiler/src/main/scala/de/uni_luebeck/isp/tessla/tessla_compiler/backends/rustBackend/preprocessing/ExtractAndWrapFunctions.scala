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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.preprocessing

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.StrictEvaluation
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics

import scala.collection.immutable.{ArraySeq, Map}
import scala.collection.mutable

class ExtractAndWrapFunctions extends TranslationPhase[Specification, Specification] {

  private val extractedFunctions = mutable.Map.empty[Identifier, (Identifier, FunctionExpression)]

  override def translate(spec: Specification): TranslationPhase.Result[Specification] = {
    val globalScope = spec.definitions.keySet

    val definitions = spec.definitions.map {
      case (id, definition) =>
        val expression = extractFunctionExpressions(definition, globalScope, Set(), id.fullName, isGlobalDef = true)
        id -> expression.asInstanceOf[DefinitionExpression]
    }

    val remappedDefinitions = (definitions ++ extractedFunctions.values).map {
      case (id, definition) =>
        val expression = remapFunctionApplications(definition, Set())
        id -> expression.asInstanceOf[DefinitionExpression]
    }

    val newSpec = Specification(
      spec.annotations,
      spec.in,
      remappedDefinitions,
      spec.out,
      spec.maxIdentifier
    )
    println(newSpec)
    Success(
      newSpec,
      Seq()
    )
  }

  /**
   * Traverse an expression, and collect a list of all [[ExpressionRef]]s, that are not defined in the current scope
   * @param e
   * @param currentScope
   * @param outerScope
   * @return
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
   * Recursively traverse through a DefinitionExpression and replace all [[FunctionExpression]]s with
   * a boxed lambda calling an [[ExpressionRef]] pointing to a global function. // TODO box those
   * If the function in question requires data from the surrounding scope, we add those as parameters to the global
   * function and box that data within a clojure.
   * The extracted global functions have all references to variables outside its scope redirected to additional arguments,
   * which are partially applied in the boxed closure.
   *
   * @param ???
   * @return
   */
  def extractFunctionExpressions(
    e: ExpressionArg,
    currentScope: Set[Identifier],
    outerScope: Set[Identifier],
    definitionPath: String,
    isGlobalDef: Boolean = false
  ): ExpressionArg = {
    e match {
      case FunctionExpression(typeParams, params, body, result, location) =>
        val functionScope = (params.map { case (id, _, _) => id } ++ body.map { case (id, _) => id }).toSet
        val functionOuterScope = outerScope ++ currentScope
        val closureArgs = findClosureArgs(e, functionScope, functionOuterScope).filterNot {
          // FIXME I imagine this filter is not exhaustive...
          // don't include reference to self in arguments to be captured
          case (id, _, typ) => definitionPath.endsWith(id.fullName) && e.tpe == typ
        }.toList
        println(s"function $definitionPath needs $closureArgs")
        val functionExpr = FunctionExpression(
          typeParams,
          params,
          body.flatMap {
            case (id, definition) =>
              val modifiedPath = s"${definitionPath}λ${id.fullName}"
              extractFunctionExpressions(
                definition,
                functionScope,
                functionOuterScope,
                modifiedPath
              ) match {
                case functionDefinition: FunctionExpression =>
                  println(s"$modifiedPath needs to be extracted")
                  extractedFunctions += id -> (Identifier(modifiedPath), functionDefinition)
                  println(s"map $id -> $modifiedPath")
                  None
                case modifiedDefinition =>
                  Some(id -> modifiedDefinition.asInstanceOf[DefinitionExpression])
              }
          },
          extractFunctionExpressions(result, functionScope, functionOuterScope, s"${definitionPath}λreturn"),
          location
        )
        if (isGlobalDef) {
          functionExpr
        } else {
          // this function takes all externally scoped arguments, and moves them into a box around the inner function
          FunctionExpression(
            List(),
            closureArgs,
            Map(),
            boxExpression(functionExpr)
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

      case ref: ExpressionRef =>
        if (ref.tpe.isInstanceOf[FunctionType]) {
          println(s"yeaaaa this is a function: ${ref.id}, in current: ${currentScope
            .contains(ref.id)}, in outer: ${outerScope.contains(ref.id)}, $definitionPath")
        }
        ref
      case extern: ExternExpression        => extern
      case float: FloatLiteralExpression   => float
      case int: IntLiteralExpression       => int
      case string: StringLiteralExpression => string
    }
  }

  /**
   * map exprRefs to functions to the respective global extract, with the added closure scope params
   *
   * @param ???
   * @return
   */ // TODO
  def remapFunctionApplications(
    e: ExpressionArg,
    outerScope: Set[Identifier]
  ): ExpressionArg = {
    e match {
      case ExpressionRef(id, typ: FunctionType, location) =>
        extractedFunctions.get(id) match {
          case None => ExpressionRef(id, typ, location)
          case Some((extractedId, FunctionExpression(_, params, _, result, location))) =>
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

  /*
  private def boxScopedDependencies(
    function: FunctionExpression,
    isGlobalDefinition: Boolean = false
  ): ExpressionArg = function match {
    case FunctionExpression(typeParams, params, body, result, location) =>
      argumentScopeMap.push(mutable.Map.empty[Identifier, (Identifier, Type)])

      def inCurrentScope(name: Identifier): Boolean = {
        params.exists { case (id, _, _) => id == name } || body.exists { case (id, _) => id == name }
      }

      val sanitizedBody = body.map {
        case (name, definition) =>
          nameStack.push(name)
          val expression = extractFunctionExpressions(definition, inCurrentScope)
          if (nameStack.pop() != name) {
            System.err.println("Uneven name defs")
          }
          name -> expression.asInstanceOf[DefinitionExpression]
      }
      val sanitizedResult =
        extractFunctionExpressions(result, inCurrentScope)

      val remappedScope = argumentScopeMap.pop()

      // TODO:
      //  fun name_extracted(arg1, arg2, scope1, scope2) { ... }
      //  fun name_boxing(scope1, scope2) { Box::new(move |arg1, arg2| name_extracted(arg1, arg2, scope1, scope2)) }
      //

      if (isGlobalDefinition) {
        // this function is already defined globally
        FunctionExpression(typeParams, params, sanitizedBody, sanitizedResult, location)
      } else {
        // this function is not defined globally

        val remappedParams = remappedScope.toList.map { case (_, (param, typ)) => (param, StrictEvaluation, typ) }

        val name = nameStack.head

        val functionName = appendToIdentifier(name, "$fun")
        val functionExpr = FunctionExpression(
          typeParams,
          params ++ remappedParams,
          sanitizedBody,
          sanitizedResult,
          location
        )

        if (recursivelyCalledNames.contains(name)) {
          extractedFunctions.addOne(
            functionName -> resolveRecursiveCall(
              name,
              functionName,
              functionExpr.tpe,
              remappedScope.toSeq.map { case (_, (param, typ)) => ExpressionRef(param, typ) },
              functionExpr
            ).asInstanceOf[DefinitionExpression]
          )
        } else {
          extractedFunctions.addOne(functionName -> functionExpr)
        }

        val boxName = appendToIdentifier(name, "$box")
        val boxExpr = FunctionExpression(
          typeParams,
          remappedParams,
          Map(),
          boxExpression(
            FunctionExpression(
              typeParams,
              params,
              Map(),
              ApplicationExpression(
                TypeApplicationExpression(
                  ExpressionRef(functionName, functionExpr.tpe, location),
                  typeParams.map { typeName => TypeParam(typeName) }
                ),
                ArraySeq.from(
                  params.map { case (id, _, typ) => ExpressionRef(id, typ) }
                    ++ remappedScope.map { case (_, (param, typ)) => ExpressionRef(param, typ) }
                ),
                location
              ),
              location
            )
          )
        )
        extractedFunctions.addOne(boxName -> boxExpr)

        ApplicationExpression(
          TypeApplicationExpression(
            ExpressionRef(boxName, boxExpr.tpe, location),
            typeParams.map { typeName => TypeParam(typeName) }
          ),
          ArraySeq.from(remappedScope.map {
            case (id, (_, typ)) => ExpressionRef(id, typ, location)
          }),
          location
        )
      }
  }
   */

  private def boxExpression(expr: ExpressionArg): ApplicationExpression = {
    ApplicationExpression(
      TypeApplicationExpression(
        ExternExpression(
          "[rust]box",
          FunctionType(
            List(),
            List((StrictEvaluation, expr.tpe)),
            expr.tpe
          )
        ),
        List()
      ),
      ArraySeq(expr)
    )
  }

  private def resolveRecursiveCall(
    initialName: Identifier,
    resolvedName: Identifier,
    resolvedType: Type,
    additionalArgs: Seq[ExpressionRef],
    expression: ExpressionArg
  ): ExpressionArg = expression match {
    case ApplicationExpression(
          TypeApplicationExpression(ExpressionRef(id, _: FunctionType, _), typeArgs, _),
          args,
          location
        ) if id == initialName =>
      ApplicationExpression(
        TypeApplicationExpression(ExpressionRef(resolvedName, resolvedType), typeArgs),
        args ++ additionalArgs,
        location
      )

    case FunctionExpression(typeParams, params, body, result, location) =>
      FunctionExpression(
        typeParams,
        params,
        body.map {
          case (id, expr) =>
            id -> resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, expr)
              .asInstanceOf[DefinitionExpression]
        },
        resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, result),
        location
      )

    case ApplicationExpression(applicable, args, location) =>
      ApplicationExpression(
        resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, applicable),
        args.map { exprArg =>
          resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, exprArg)
        },
        location
      )
    case TypeApplicationExpression(applicable, typeArgs, location) =>
      TypeApplicationExpression(
        resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, applicable),
        typeArgs,
        location
      )
    case RecordAccessorExpression(name, target, nameLocation, location) =>
      RecordAccessorExpression(
        name,
        resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, target),
        nameLocation,
        location
      )
    case RecordConstructorExpression(entries, location) =>
      RecordConstructorExpression(
        entries.map {
          case (name, (exprArg, location)) =>
            name -> (resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, exprArg), location)
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

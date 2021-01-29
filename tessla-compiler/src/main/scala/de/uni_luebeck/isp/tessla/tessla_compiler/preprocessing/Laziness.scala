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

package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.LazyEvaluation
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.ExtendedSpecification

/**
 * Adds laziness information to an [[ExtendedSpecification]], i.e. which variables have to be assigned in a
 * lazy manner and which variables are inlined.
 * A variable is defined lazy if it is used as sub-expression of a lazy argument in a function call and not inlined.
 * It is inlined if there is no other usage.
 */
object Laziness extends TranslationPhase[ExtendedSpecification, ExtendedSpecification] {

  /**
   * Function triggering the translation from a [[ExtendedSpecification]] to another one
   * with lazyness information i.e. which variables have to be assigned in a lazy manner and which variables
   * are inlined
   * @param extSpec The extended specification to be examined
   * @return extSpec plus lazyness information
   */
  override def translate(extSpec: ExtendedSpecification): Result[ExtendedSpecification] = {

    val lazyIds: collection.mutable.Set[Identifier] = collection.mutable.HashSet()
    val lazyByInlining: collection.mutable.Set[Identifier] = collection.mutable.HashSet()
    val spec = extSpec.spec

    def extractSubIDs(arg: ExpressionArg): Set[Identifier] = {
      arg match {
        case ApplicationExpression(applicable, args, _) =>
          args.foldLeft[Set[Identifier]](extractSubIDs(applicable)) { case (s, i) => s ++ extractSubIDs(i) }
        case TypeApplicationExpression(applicable, _, _) => extractSubIDs(applicable)
        case RecordConstructorExpression(entries, _) =>
          entries.map(_._2._1).foldLeft[Set[Identifier]](Set()) { case (s, i) => s ++ extractSubIDs(i) }
        case RecordAccessorExpression(_, target, _, _)                    => extractSubIDs(target)
        case ExpressionRef(id, tpe, _) if !tpe.isInstanceOf[FunctionType] => Set(id)
        case _                                                            => Set() //Note FuncExp. are not inlined and always lazy
      }
    }

    def examineLazynessCandidate(
      e: ExpressionArg,
      scope: Map[Identifier, DefinitionExpression],
      stack: Set[Identifier] = Set()
    ): Unit = {
      extractSubIDs(e).foreach(examineLazynessCandidateID(_, scope, stack))
    }

    def examineLazynessCandidateID(
      id: Identifier,
      scope: Map[Identifier, DefinitionExpression],
      stack: Set[Identifier] = Set()
    ): Unit = {
      if (!lazyIds.contains(id)) {
        if (stack.contains(id)) {
          lazyByInlining -= id
          lazyIds += id
        } else {
          if (extSpec.usageInfo.get.getOrElse(id, Set()).size <= 1 && !lazyIds.contains(id)) {
            //TODO one could use a better heuristic here to decide when to inline
            lazyByInlining += id
          } else {
            lazyIds += id
          }
          if (scope.contains(id)) {
            examineLazynessCandidate(scope(id), scope, stack + id)
          }
        }
      }
    }

    def mapExpressionsForLazyness(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): Unit = {
      e match {
        case ApplicationExpression(applicable, args, _) =>
          args.zip(applicable.tpe.asInstanceOf[FunctionType].paramTypes).foreach {
            case (arg, (LazyEvaluation, _)) => examineLazynessCandidate(arg, scope)
            case _                          =>
          }
          args.foreach(mapExpressionsForLazyness(_, scope))
        case FunctionExpression(_, _, body, result, _) =>
          mapExpressionsForLazyness(result, scope ++ body); mapDefinitionsForLazyness(body, scope ++ body)
        case TypeApplicationExpression(applicable, _, _) =>
          mapExpressionsForLazyness(applicable, scope)
        case RecordConstructorExpression(entries, _) =>
          entries.foreach { case (_, (e, _)) => mapExpressionsForLazyness(e, scope) }
        case RecordAccessorExpression(_, target, _, _) =>
          mapExpressionsForLazyness(target, scope)
        case _ =>
      }
    }

    def mapDefinitionsForLazyness(
      m: Map[Identifier, DefinitionExpression],
      scope: Map[Identifier, DefinitionExpression]
    ): Unit = {
      m.foreach { case (_, d) => mapExpressionsForLazyness(d, scope) }
    }

    mapDefinitionsForLazyness(spec.definitions, spec.definitions)

    Success(ExtendedSpecification(spec, extSpec.usageInfo, Some(lazyIds.toSet), Some(lazyByInlining.toSet)), Seq())

  }

}

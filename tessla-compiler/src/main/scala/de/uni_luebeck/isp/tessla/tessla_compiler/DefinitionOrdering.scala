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

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core.DefinitionExpression

import scala.collection.mutable

/**
 * Class for calculating a topological sorting on the assignments inside functions and specifications so they
 * can be translated to imperative code
 */
object DefinitionOrdering {

  /**
   * Orders a Map Identifier -> Defining expression to a sequence s.t. every variable is assigned before it is used
   * Exceptions exist for identifiers which have last/delay expressions assigned since they may cause recursions
   * @param input The map Identifier -> Defining to be sorted
   * @return The Identifier -> Defining mappings in an ordered sequence
   */
  def order(input: Map[Identifier, DefinitionExpression]): Seq[(Identifier, DefinitionExpression)] = {

    val ordered: mutable.ArrayBuffer[(Identifier, DefinitionExpression)] = mutable.ArrayBuffer()

    def calcExpressionDependencies(exp: ExpressionArg, ignore: Set[Identifier]): Unit = {
      exp match {
        case ApplicationExpression(TypeApplicationExpression(ExternExpression("last", _, _), _, _), args, _) =>
          calcExpressionDependencies(args(1), ignore)
        case ApplicationExpression(TypeApplicationExpression(ExternExpression("delay", _, _), _, _), args, _) =>
          calcExpressionDependencies(args(1), ignore)
        case ApplicationExpression(app, args, _) =>
          args.appended(app).foreach(exp => calcExpressionDependencies(exp, ignore))
        case ExpressionRef(id, _, _) if input.contains(id) => calcMissingDependencies(id, input(id), ignore)
        case TypeApplicationExpression(e, _, _)            => calcExpressionDependencies(e, ignore)
        case RecordConstructorExpression(entries, _) =>
          entries.foreach { case (_, (exp, _)) => calcExpressionDependencies(exp, ignore) }
        case RecordAccessorExpression(_, target, _, _) => calcExpressionDependencies(target, ignore)
        case _                                         => ()
      }
    }

    def calcMissingDependencies(id: Identifier, dExp: DefinitionExpression, ignore: Set[Identifier]): Unit = {
      if (!ignore.contains(id) && !ordered.contains((id, dExp))) {
        calcExpressionDependencies(dExp, ignore + id)
        ordered += ((id, dExp))
      }
    }

    input.foreach { case (i, d) => calcMissingDependencies(i, d, Set()) }

    ordered.toSeq
  }

}

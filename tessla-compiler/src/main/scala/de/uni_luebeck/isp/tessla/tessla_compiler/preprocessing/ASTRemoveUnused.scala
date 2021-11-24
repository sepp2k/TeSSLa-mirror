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

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core.{DefinitionExpression, _}
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.ExtendedSpecification

class ASTRemoveUnused extends TranslationPhase[ExtendedSpecification, ExtendedSpecification] {

  override def translate(espec: ExtendedSpecification): Result[ExtendedSpecification] = {
    val spec = espec.spec

    var warnings = Seq()
    var usages = collection.mutable.Map.from(espec.usageInfo.get)

    usages --= spec.out.map(_._1.id)
    val dels: collection.mutable.HashSet[Identifier] = collection.mutable.HashSet()
    var newDels: Set[Identifier] = Set()

    do {
      newDels = usages.filter(_._2.isEmpty).keys.toSet
      dels ++= newDels
      usages = (usages -- dels).map { case (i, s) => (i, s -- dels) }
    } while (newDels.nonEmpty)

    def mapExpArg(ea: ExpressionArg): ExpressionArg = {
      ea match {
        case e: Expression    => mapDefExp(e)
        case _: ExpressionRef => ea
      }
    }

    def mapDefExp(de: DefinitionExpression): DefinitionExpression = {
      de match {
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, mapExpArg(target), nameLocation, location)
        case FunctionExpression(typeParams, params, body, result, location) =>
          FunctionExpression(typeParams, params, mapDefs(body), mapExpArg(result), location)
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(mapExpArg(applicable), args.map(mapExpArg), location)
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(mapExpArg(applicable), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          RecordConstructorExpression(entries.map { case (n, (e, l)) => (n, (mapExpArg(e), l)) }, location)
        case _ => de
      }
    }

    def mapDefs(defs: Map[Identifier, DefinitionExpression]): Map[Identifier, DefinitionExpression] = {
      (defs -- dels).map { case (id, d) => (id, mapDefExp(d)) }
    }

    Success(espec.copy(spec = spec.copy(definitions = mapDefs(spec.definitions))), warnings)
  }

}

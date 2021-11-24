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

import cats.data.Ior
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.{Location, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}

import scala.collection.mutable

class StreamDefFlattener extends TranslationPhase[Core.Specification, Core.Specification] {

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    var maxId: Long = spec.maxIdentifier
    val definitions = spec.definitions
    val newDef: mutable.Map[Identifier, DefinitionExpression] = mutable.Map()
    var warnings = Seq()

    def getId(id: Option[Identifier]): Identifier = {
      id.getOrElse(new Identifier(Ior.right { maxId += 1; maxId }))
    }

    def flatten(id: Option[Identifier], e: ExpressionArg): ExpressionArg = {
      e match {
        case ApplicationExpression(
              TypeApplicationExpression(e: ExternExpression, typeArgs, location),
              args,
              location2
            ) => {
          val nid = getId(id)
          newDef += (nid -> ApplicationExpression(
            TypeApplicationExpression(e, typeArgs, location),
            args.map { a => flatten(None, a) },
            location2
          ))
          ExpressionRef(nid, e.tpe, Location.unknown)
        }
        case ApplicationExpression(e: ExternExpression, args, location) => {
          val nid = getId(id)
          newDef += (nid -> ApplicationExpression(e, args.map { a => flatten(None, a) }, location))
          ExpressionRef(nid, e.tpe, Location.unknown)
        }
        case e: ExpressionRef => e
        case _                => e
      }
    }

    definitions.foreach {
      case (id, definition) => {
        definition.tpe match {
          case InstantiatedType("Events", _, _) => flatten(Some(id), definition)
          case _                                => newDef += (id -> definition)
        }
      }
    }

    Success(spec.copy(definitions = newDef.toMap), warnings)
  }

}

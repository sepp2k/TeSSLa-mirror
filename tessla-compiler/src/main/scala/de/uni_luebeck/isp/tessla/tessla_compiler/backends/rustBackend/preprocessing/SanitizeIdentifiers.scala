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

import cats.data.Ior
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}

/**
 * This translation step remaps all identifiers contained in the spec to remove the '$' character.
 * This is necessary because in rust the dollar sign is restricted to use in macros and thus not allowed in identifiers.
 *
 * Additionally any additional unicode characters that are not allowed in identifiers by <a href="https://unicode.org/reports/tr31/#R1">UAX31-R1</a> are also mapped out.
 */
object SanitizeIdentifiers extends TranslationPhase[Specification, Specification] {
  override def translate(spec: TesslaAST.Core.Specification): TranslationPhase.Result[TesslaAST.Core.Specification] = {

    val in = spec.in.map {
      case (id, (typ, annotations)) => (escapeIdentifier(id), (typ, annotations))
    }

    val out = spec.out.map {
      case (expr, annotations) => (expr, annotations)
    }

    val definitions = spec.definitions.map {
      case (id, definition) =>
        (escapeIdentifier(id), findAndEscapeIdentifiers(definition).asInstanceOf[DefinitionExpression])
    }

    Success(
      Specification(spec.annotations, in, definitions, out, spec.maxIdentifier),
      Seq()
    )
  }

  private def findAndEscapeIdentifiers(expr: ExpressionArg): ExpressionArg = expr match {
    case ExpressionRef(id, tpe, location) => ExpressionRef(escapeIdentifier(id), tpe, location)
    case FunctionExpression(typeParams, params, body, result, location) =>
      FunctionExpression(
        typeParams.map(escapeIdentifier),
        params.map {
          case (id, eval, typ) =>
            (escapeIdentifier(id), eval, typ)
        },
        body.map {
          case (id, definition) =>
            (escapeIdentifier(id), findAndEscapeIdentifiers(definition).asInstanceOf[DefinitionExpression])
        },
        findAndEscapeIdentifiers(result),
        location
      )
    case ApplicationExpression(applicable, args, location) =>
      ApplicationExpression(findAndEscapeIdentifiers(applicable), args.map(findAndEscapeIdentifiers), location)
    case TypeApplicationExpression(applicable, typeArgs, location) =>
      TypeApplicationExpression(findAndEscapeIdentifiers(applicable), typeArgs, location)
    case RecordConstructorExpression(entries, location) =>
      RecordConstructorExpression(
        entries.map {
          case (name, (expr, location)) =>
            (escapeName(name), (findAndEscapeIdentifiers(expr), location))
        },
        location
      )
    case RecordAccessorExpression(name, target, nameLocation, location) =>
      RecordAccessorExpression(escapeName(name), findAndEscapeIdentifiers(target), nameLocation, location)
    case _: ExternExpression | _: StringLiteralExpression | _: IntLiteralExpression | _: FloatLiteralExpression => expr
  }

  def escapeName(name: String): String = {
    name.replace("_", "__").flatMap { c =>
      if (c.isUnicodeIdentifierPart) s"$c" else f"u${c * 1}%04X_"
    }
  }

  private def escapeIdentifier(id: Identifier): Identifier = {
    id.idOrName match {
      case Ior.Left(name)      => Identifier(Ior.Left(s"${escapeName(name)}"), id.location)
      case Ior.Right(num)      => Identifier(Ior.Left(s"${num}_"), id.location)
      case Ior.Both(name, num) => Identifier(Ior.Left(s"${escapeName(name)}_${num}_"), id.location)
    }
  }
}

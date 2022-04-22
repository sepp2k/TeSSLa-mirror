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

import scala.collection.immutable.ArraySeq

/**
 * This translation step remaps all identifiers contained in the spec to remove the '$' character.
 * This is necessary because in rust the dollar sign is restricted to use in macros and thus not allowed in identifiers.
 *
 * Additionally any additional unicode characters that are not allowed in identifiers by <a href="https://unicode.org/reports/tr31/#R1">UAX31-R1</a> are also mapped out.
 */
object SanitizeIdentifiers extends TranslationPhase[Specification, Specification] {
  override def translate(spec: TesslaAST.Core.Specification): TranslationPhase.Result[TesslaAST.Core.Specification] = {

    val in = spec.in.map {
      case (id, (typ, annotations)) =>
        val inputAnnotations = annotations ++ Map("$name" -> ArraySeq(StringLiteralExpression(id.toString)))
        (escapeIdentifier(id), (escapeType(typ), inputAnnotations))
    }

    val out = spec.out.map {
      case (ExpressionRef(id, typ, location), annotations) =>
        (ExpressionRef(escapeIdentifier(id), escapeType(typ), location), annotations)
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
    case ExpressionRef(id, typ, location) => ExpressionRef(escapeIdentifier(id), escapeType(typ), location)
    case FunctionExpression(typeParams, params, body, result, location) =>
      FunctionExpression(
        typeParams.map(escapeIdentifier),
        params.map {
          case (id, eval, typ) =>
            (escapeIdentifier(id), eval, escapeType(typ))
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
      TypeApplicationExpression(findAndEscapeIdentifiers(applicable), typeArgs.map(escapeType), location)
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
    case ExternExpression(externName, typ, location) =>
      ExternExpression(externName, escapeType(typ), location)
    case _: StringLiteralExpression | _: IntLiteralExpression | _: FloatLiteralExpression => expr
  }

  /**
   * Replace underscores with double underscores, and any non UAX31-R1 characters
   * with their hex representation as `uXXXX_`.
   * @param name The name to escape.
   * @return The escaped name.
   */
  def escapeName(name: String): String = {
    name.replace("_", "__").flatMap { c =>
      if (c.isUnicodeIdentifierPart) s"$c" else f"u${c * 1}%04X_"
    }
  }

  /**
   * Reverse [[escapeName]], needed for struct display/parsing
   * @param name The name to unescape.
   * @return The original name.
   */
  def unescapeName(name: String): String = {
    val pattern = "u([0-9A-F]{4})_".r
    pattern.replaceAllIn(name.replace("__", "_"), unicode => Character.toString(Integer.valueOf(unicode.group(1), 16)))
  }

  private def escapeIdentifier(id: Identifier): Identifier = {
    id.idOrName match {
      case Ior.Left(name)      => Identifier(Ior.Left(s"${escapeName(name)}"), id.location)
      case Ior.Right(num)      => Identifier(Ior.Left(s"${num}_"), id.location)
      case Ior.Both(name, num) => Identifier(Ior.Left(s"${escapeName(name)}_${num}_"), id.location)
    }
  }

  private def escapeType(typ: Type): Type = {
    typ match {
      case InstantiatedType(name, typeArgs, location) =>
        InstantiatedType(escapeName(name), typeArgs.map(escapeType), location)
      case TypeParam(name, location) =>
        TypeParam(escapeIdentifier(name), location)
      case FunctionType(typeParams, paramTypes, resultType, location) =>
        FunctionType(
          typeParams.map(escapeIdentifier),
          paramTypes.map { case (evaluation, typ) => (evaluation, escapeType(typ)) },
          escapeType(resultType),
          location
        )
      case RecordType(entries, location) =>
        RecordType(
          entries.map { case (name, (typ, location)) => (escapeName(name), (escapeType(typ), location)) },
          location
        )
    }
  }
}

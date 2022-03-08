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
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.tessla_compiler.TypeArgManagement
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.{RustUtils, TesslaCoreToRust}

import scala.collection.mutable

/**
 * Creates Struct [[ExternExpression]] definitions for all unique struct/record types found in the specification
 * These are then used in [[TesslaCoreToRust]] to generate the rust structs
 */

object GenerateStructDefinitions extends TranslationPhase[Specification, Specification] {

  /**
   * Extracts all [[RecordType]]s used in the specification and adds an [[ExternExpression]] definition for each
   * unique (names + types) record or struct encountered
   *
   * @param spec The TeSSLa Core specification to be examined
   * @return The same spec with the added definitions
   */
  override def translate(spec: Specification): TranslationPhase.Result[Specification] = {
    val structDefinitions = mutable.Map.empty[Identifier, DefinitionExpression]

    spec.definitions.foreach {
      case (_, definition) => findRequiredStructs(definition, TypeArgManagement.empty, structDefinitions)
    }

    // we need to also consider input stream types, since they could just get passed through
    // without being modified, thus not ever appearing in the definitions
    spec.in.foreach { case (_, (typ, _)) => findRecordTypes(typ, TypeArgManagement.empty, structDefinitions) }
    spec.out.foreach { case (expr, _) => findRequiredStructs(expr, TypeArgManagement.empty, structDefinitions) }

    Success(
      Specification(spec.annotations, spec.in, spec.definitions ++ structDefinitions, spec.out, spec.maxIdentifier),
      Seq()
    )
  }

  /**
   * Go through an expression recursively inspecting all types to find every [[RecordType]] in use,
   * adding a rust specific [[ExternExpression]] that will generate the necessary struct definition
   * later on.
   *
   * @param expression an expression
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param structDefinitions a definition map containing every unique record type
   */
  private def findRequiredStructs(
    expression: ExpressionArg,
    tm: TypeArgManagement,
    structDefinitions: mutable.Map[Identifier, DefinitionExpression]
  ): Unit = expression match {
    case FunctionExpression(_, params, body, result, _) =>
      params.foreach { case (_, _, tpe) => findRecordTypes(tpe, tm, structDefinitions) }
      body.foreach { case (_, definition) => findRequiredStructs(definition, tm, structDefinitions) }
      findRequiredStructs(result, tm, structDefinitions)
    case ApplicationExpression(applicable, args, _) =>
      findRequiredStructs(applicable, tm, structDefinitions)
      args.foreach(findRequiredStructs(_, tm, structDefinitions))
    case TypeApplicationExpression(applicable, typeArgs, _) =>
      findRequiredStructs(applicable, tm.typeApp(typeArgs), structDefinitions)
      typeArgs.foreach(findRecordTypes(_, tm, structDefinitions))
    case RecordConstructorExpression(entries, _) =>
      entries.foreach { case (_, (value, _)) => findRequiredStructs(value, tm, structDefinitions) }
    case RecordAccessorExpression(_, target, _, _) =>
      findRequiredStructs(target, tm, structDefinitions)

    case ExpressionRef(_, tpe, _)    => findRecordTypes(tpe, tm, structDefinitions)
    case ExternExpression(_, tpe, _) => findRecordTypes(tpe, tm, structDefinitions)

    case _: StringLiteralExpression | _: IntLiteralExpression | _: FloatLiteralExpression => {}
  }

  /**
   * Go through a given type recursively, noting any record types found in the definitions map.
   *
   * @param typ any type
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param structDefinitions a definition map containing every encountered unique record type
   */
  private def findRecordTypes(
    typ: Type,
    tm: TypeArgManagement,
    structDefinitions: mutable.Map[Identifier, DefinitionExpression]
  ): Unit = typ match {
    case RecordType(entries, _) if entries.nonEmpty =>
      // check if this type has already been added
      val structName = Identifier(RustUtils.getStructName(entries))

      if (!structDefinitions.contains(structName)) {
        entries.foreach { case (_, (typ, _)) => findRecordTypes(typ.resolve(tm.resMap), tm, structDefinitions) }

        structDefinitions.addOne(
          structName -> ExternExpression("[rust]Struct", typ.resolve(tm.resMap)).asInstanceOf[DefinitionExpression]
        )
      }

    case InstantiatedType("Events", Seq(t), _) => findRecordTypes(t.resolve(tm.resMap), tm, structDefinitions)
    case InstantiatedType("Option", Seq(t), _) => findRecordTypes(t.resolve(tm.resMap), tm, structDefinitions)
    case InstantiatedType(n, types, _) if n.startsWith("native:") =>
      types.foreach(typ => findRecordTypes(typ.resolve(tm.resMap), tm, structDefinitions))
    case FunctionType(_, paramTypes, resultType, _) =>
      paramTypes.foreach { case (_, typ) => findRecordTypes(typ.resolve(tm.resMap), tm, structDefinitions) }
      findRecordTypes(resultType.resolve(tm.resMap), tm, structDefinitions)

    case _ => ()
  }
}

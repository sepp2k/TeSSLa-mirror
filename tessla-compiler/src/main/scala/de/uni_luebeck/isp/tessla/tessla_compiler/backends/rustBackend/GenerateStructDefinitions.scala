package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.{TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

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
  override def translate(spec: TesslaAST.Core.Specification): TranslationPhase.Result[TesslaAST.Core.Specification] = {
    val structDefinitions = mutable.Map.empty[Identifier, DefinitionExpression]

    spec.definitions.foreach { case (_, definition) => findRequiredStructs(definition, structDefinitions) }

    // we need to also consider input stream types, since they could just get passed through
    // without being modified, thus not ever appearing in the definitions
    spec.in.foreach { case (_, (typ, _)) => findRecordTypes(typ, structDefinitions) }

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
   * @param structDefinitions a definition map containing every unique record type
   */
  private def findRequiredStructs(
    expression: ExpressionArg,
    structDefinitions: mutable.Map[Identifier, DefinitionExpression]
  ): Unit = expression match {
    case FunctionExpression(_, params, body, result, _) =>
      params.foreach { case (_, _, tpe) => findRecordTypes(tpe, structDefinitions) }
      body.foreach { case (_, definition) => findRequiredStructs(definition, structDefinitions) }
      findRequiredStructs(result, structDefinitions)
    case ApplicationExpression(applicable, args, _) =>
      findRequiredStructs(applicable, structDefinitions)
      args.foreach(findRequiredStructs(_, structDefinitions))
    case TypeApplicationExpression(applicable, typeArgs, _) =>
      findRequiredStructs(applicable, structDefinitions)
      typeArgs.foreach(findRecordTypes(_, structDefinitions))
    case RecordConstructorExpression(entries, _) =>
      entries.foreach { case (_, (value, _)) => findRequiredStructs(value, structDefinitions) }
    case RecordAccessorExpression(_, target, _, _) =>
      findRequiredStructs(target, structDefinitions)

    case ExpressionRef(_, tpe, _)    => findRecordTypes(tpe, structDefinitions)
    case ExternExpression(_, tpe, _) => findRecordTypes(tpe, structDefinitions)

    case _: StringLiteralExpression | _: IntLiteralExpression | _: FloatLiteralExpression => {}
  }

  /**
   * Go through a given type recursively, noting any record types found in the definitions map.
   *
   * @param typ any type
   * @param structDefinitions a definition map containing every encountered unique record type
   */
  private def findRecordTypes(
    typ: Type,
    structDefinitions: mutable.Map[Identifier, DefinitionExpression]
  ): Unit = typ match {
    case RecordType(entries, _) =>
      // check if this type has already been added
      val structName = Identifier(RustUtils.getStructName(entries))

      if (!structDefinitions.contains(structName)) {
        entries.foreach { case (_, (typ, _)) => findRecordTypes(typ, structDefinitions) }

        structDefinitions.addOne(structName -> ExternExpression("[rust]Struct", typ).asInstanceOf[DefinitionExpression])
      }

    case InstantiatedType("Events", Seq(t), _) => findRecordTypes(t, structDefinitions)
    case InstantiatedType("Option", Seq(t), _) => findRecordTypes(t, structDefinitions)
    case InstantiatedType(n, types, _) if n.startsWith("native:") =>
      types.foreach(findRecordTypes(_, structDefinitions))
    case FunctionType(_, paramTypes, resultType, _) =>
      paramTypes.foreach { case (_, typ) => findRecordTypes(typ, structDefinitions) }
      findRecordTypes(resultType, structDefinitions)

    case _ => ()
  }
}

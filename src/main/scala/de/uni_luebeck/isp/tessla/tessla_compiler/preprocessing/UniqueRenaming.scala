package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import cats.data.Ior
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

class UniqueRenaming extends TranslationPhase[Core.Specification, Core.Specification] {

  override def translate(spec: Core.Specification): Result[Core.Specification] = {
    var maxId: Long = spec.maxIdentifier

    def freshID(oldId: Identifier): Identifier = {
      if (oldId.fullName == "_") {
        oldId
      } else {
        val newN = {
          maxId += 1; maxId
        }
        if (oldId.idOrName.isLeft || oldId.idOrName.isBoth) {
          new Identifier(Ior.both(oldId.idOrName.left.get, newN), oldId.location)
        } else {
          new Identifier(Ior.right(newN), oldId.location)
        }
      }
    }

    def renameExpressionArg(e: ExpressionArg, nameLookup: Map[Identifier, Identifier]): ExpressionArg = {
      e match {
        case e: Expression    => renameExpression(e, nameLookup)
        case e: ExpressionRef => renameExpressionRef(e, nameLookup)
      }
    }

    def renameExpressionRef(e: ExpressionRef, nameLookup: Map[Identifier, Identifier]): ExpressionRef = {
      ExpressionRef(nameLookup.getOrElse(e.id, e.id), e.tpe, e.location)
    }

    def renameExpression(e: DefinitionExpression, nameLookup: Map[Identifier, Identifier]): DefinitionExpression = {
      e match {
        case FunctionExpression(typeParams, params, body, result, location) =>
          val newNameLookup = nameLookup ++ params.map { case (id, _, _) => (id, freshID(id)) }
          val (newBody, finNameLookup) = renameDefinitions(body, newNameLookup)
          FunctionExpression(
            typeParams,
            params.map { case (id, ev, tpe) => (newNameLookup.getOrElse(id, id), ev, tpe) },
            newBody,
            renameExpressionArg(result, finNameLookup),
            location
          )
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(
            renameExpressionArg(applicable, nameLookup),
            args.map(renameExpressionArg(_, nameLookup)),
            location
          )
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(renameExpressionArg(applicable, nameLookup), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          RecordConstructorExpression(
            entries.map { case (n, (e, loc)) => (n, (renameExpressionArg(e, nameLookup), loc)) },
            location
          )
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, renameExpressionArg(target, nameLookup), nameLocation, location)
        case _ => e
      }
    }

    def renameDefinitions(
      defs: Map[Identifier, DefinitionExpression],
      nameLookup: Map[Identifier, Identifier]
    ): (Map[Identifier, DefinitionExpression], Map[Identifier, Identifier]) = {
      val newNameLookup = nameLookup ++ defs.map { case (id, _) => (id, freshID(id)) }
      val newDefs = defs.map {
        case (id, exp) =>
          (newNameLookup(id), renameExpression(exp, newNameLookup))
      }
      (newDefs, newNameLookup)
    }

    val (newDef, lookup) = renameDefinitions(spec.definitions, Map())
    val newOut = spec.out.map { case (er, ann) => (renameExpressionRef(er, lookup), ann) }
    Success(Core.Specification(spec.in, newDef, newOut, maxId), Seq())
  }

}

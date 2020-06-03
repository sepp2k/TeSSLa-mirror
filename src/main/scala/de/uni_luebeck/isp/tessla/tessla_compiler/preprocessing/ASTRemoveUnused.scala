package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TesslaAST.Core.{DefinitionExpression, _}
import de.uni_luebeck.isp.tessla.{Errors, TranslationPhase}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.ExtendedSpecification

//Note: Currently not in use
class ASTRemoveUnused() extends TranslationPhase[ExtendedSpecification, ExtendedSpecification] {

  override def translate(extSpec: ExtendedSpecification): Result[ExtendedSpecification] = {

    val spec = extSpec.spec
    var usages : collection.mutable.Map[Identifier, Set[Identifier]] =
      collection.mutable.Map() ++ extSpec.usageInfo.getOrElse(throw Errors.InternalError("No usage information available for RemoveUnused phase"))

    usages --= spec.out.map(_._1.id)
    var dels : collection.mutable.HashSet[Identifier] = collection.mutable.HashSet()
    var newDels : Set[Identifier] = Set()

    do {
      newDels = usages.filter(_._2.isEmpty).keys.toSet
      dels ++= newDels
      usages = (usages -- dels).map{case (i, s) => (i, s -- dels)}
    } while (newDels.nonEmpty)

    def mapExpArg(ea: ExpressionArg) : ExpressionArg = {
      ea match {
        case e: Expression => mapDefExp(e)
        case _: ExpressionRef => ea
      }
    }

    def mapDefExp(de: DefinitionExpression) : DefinitionExpression = {
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
          RecordConstructorExpression(entries.map{case (n, (e, l)) => (n , (mapExpArg(e), l))}, location)
        case _ => de
      }
    }

    def mapDefs(defs: Map[Identifier, DefinitionExpression]) : Map[Identifier, DefinitionExpression] = {
      (defs -- dels).map{case (id, d) => (id, mapDefExp(d))}
    }

    val newSpec = Core.Specification(spec.in, mapDefs(spec.definitions), spec.out, spec.maxIdentifier)
    Success(ExtendedSpecification(newSpec, extSpec.usageInfo, extSpec.lazyVars, extSpec.inlining), Seq())
  }

}

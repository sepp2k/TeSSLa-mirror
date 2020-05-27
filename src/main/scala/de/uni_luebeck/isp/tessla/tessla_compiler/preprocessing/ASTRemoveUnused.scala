package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TesslaAST.Core.{DefinitionExpression, _}
import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

//Note: Currently not in use
class ASTRemoveUnused() extends TranslationPhase[Core.Specification, Core.Specification] {

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    var usages : collection.mutable.HashMap[Identifier, Set[Identifier]] = collection.mutable.HashMap()

    def findUsages(ea: ExpressionArg, usedIn: Identifier): Unit = {
      ea match {
        case RecordAccessorExpression(_, target, _, _) => findUsages(target, usedIn)
        case FunctionExpression(_, _, body, result, _) => findUsagesInDefs(body); findUsages(result, usedIn)
        case ApplicationExpression(applicable, args, _) => (args :+ applicable).foreach(findUsages(_, usedIn))
        case TypeApplicationExpression(applicable, _, _) => findUsages(applicable, usedIn)
        case RecordConstructorExpression(entries, _) => entries.foreach(e => findUsages(e._2._1, usedIn))
        case ExpressionRef(id, _, _) => usages += (id -> (usages.getOrElse(id, Set()) + usedIn))
        case _ =>
      }
    }

    def findUsagesInDefs(defs: Map[Identifier, DefinitionExpression]): Unit = {
      defs.foreach{case (id, d) =>
        if (!usages.contains(id)) {
          usages += (id -> Set())
        }
        findUsages(d, id)
      }
    }

    findUsagesInDefs(spec.definitions)

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

    Success(Core.Specification(spec.in, mapDefs(spec.definitions), spec.out, spec.maxIdentifier), Seq())
  }

}

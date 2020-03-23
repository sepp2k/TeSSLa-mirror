package de.uni_luebeck.isp.tessla.tessla_compiler

import cats.data.Ior
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TesslaAST.Core.{DefinitionExpression, _}
import de.uni_luebeck.isp.tessla.{Location, TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

import scala.collection.mutable

//TODO: Always pull out Set/Map/... constructor calls
//TODO: Make all identifier unique

class ASTPreprocessor extends TranslationPhase[Core.Specification, Core.Specification] {

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    var maxId: Long = spec.maxIdentifier
    val definitions = spec.definitions
    val newIDs: mutable.Map[Identifier, Identifier] = mutable.Map()
    var warnings = Seq()

    def nextIDNum : Long = {
      maxId += 1
      maxId
    }

    def newIDFromOld(id: Identifier) : Identifier = {
      val newID = if (id.idOrName.left.isDefined) {
        Identifier(Ior.both(id.idOrName.left.get, nextIDNum))
      } else {
        freshId()
      }
      newIDs += (id -> newID)
      newID
    }

    def freshId() : Identifier = {
      Identifier(Ior.right(nextIDNum))
    }

    def getActID(id: Identifier): Identifier = {
      newIDs.getOrElse(id, id)
    }

    def getExpRef(exp: ExpressionArg, add: mutable.Map[Identifier, DefinitionExpression]):  ExpressionArg = {
      exp match {
        case exp : Expression => {
          val newID = freshId()
          add += (newID -> exp)
          ExpressionRef(newID, exp.tpe, Location.unknown)
        }
        case e: ExpressionRef => e
      }
    }

    def flattenDefExp(exp: DefinitionExpression, add: mutable.Map[Identifier, DefinitionExpression]) : DefinitionExpression = {
      exp match {
        case FunctionExpression(typeParams, params, body, result, location) => {
          val newBody = mutable.Map.from(flattenDefs(body))
          val newResult = flattenExpArg(result, newBody)
          FunctionExpression(typeParams, params.map{case (id, s, t) => (newIDFromOld(id), s, t)}, newBody.toMap, newResult, location)
        }
        case ApplicationExpression(applicable, args, location) =>
          applicable match {
            case ExternExpression(_, _, _, "lift", _)
               | ExternExpression(_, _, _, "slift", _)
               | TypeApplicationExpression(ExternExpression(_, _, _, "lift", _), _, _)
               | TypeApplicationExpression(ExternExpression(_, _, _, "slift", _), _, _) =>
              ApplicationExpression(applicable, args.dropRight(1).map(getExpRef(_, add)) :+ args.last, location)
            case _: ExternExpression |
                 TypeApplicationExpression(_: ExternExpression, _, _) =>
              ApplicationExpression(applicable, args.map(getExpRef(_, add)), location)
            case _ =>
              ApplicationExpression(getExpRef(applicable, add), args.map(getExpRef(_, add)), location)
          }
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(flattenExpArg(applicable, add), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          val newEntries = entries.map{case (n, (e, l)) => (n, (getExpRef(e, add), l))}
          RecordConstructorExpression(newEntries, location)
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, getExpRef(target, add), nameLocation, location)
        case _ => exp
      }
    }

    def flattenExpArg(exp: ExpressionArg, add: mutable.Map[Identifier, DefinitionExpression]) : ExpressionArg = {
      exp match {
        case exp: Expression => flattenExpArg(exp, add)
        case exp: ExpressionRef => exp
      }
    }

    def flattenDefs(defs: Map[Identifier, DefinitionExpression]): Map[Identifier, DefinitionExpression] = {
      val newDefs : mutable.Map[Identifier, DefinitionExpression] = mutable.Map()
      val transDefs = defs.map{case (id, defExp) => (newIDFromOld(id) -> flattenDefExp(defExp, newDefs))}
      transDefs ++ newDefs.toMap
    }

    def exchangeOldRefsDefExp(exp: DefinitionExpression) : DefinitionExpression = {
      exp match {
        case FunctionExpression(typeParams, params, body, result, location) =>
          FunctionExpression(typeParams, params, body.view.mapValues(exchangeOldRefsDefExp).toMap, exchangeOldRefsExpArg(result), location)
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(exchangeOldRefsExpArg(applicable), args.map(exchangeOldRefsExpArg), location)
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(exchangeOldRefsExpArg(applicable), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          RecordConstructorExpression(entries.view.mapValues{case (e,l) => (exchangeOldRefsExpArg(e), l)}.toMap, location)
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, exchangeOldRefsExpArg(target), nameLocation, location)
        case _ =>
          exp
      }
    }

    def exchangeOldRefsExpArg(exp: ExpressionArg) : ExpressionArg = {
      exp match {
        case e: Expression => exchangeOldRefsDefExp(e)
        case ExpressionRef(id, tpe, location) => ExpressionRef(getActID(id), tpe, location)
      }
    }

    val newDefs = flattenDefs(definitions)

    Success(Core.Specification(spec.in.map{case (id, v) => (getActID(id), v)},
                               newDefs.view.mapValues(exchangeOldRefsDefExp).toMap,
                               spec.out.map{case (id, a) => (getActID(id), a)},
                               maxId),
            warnings)
  }

}

package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import cats.data.Ior
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TesslaAST.Core.{DefinitionExpression, _}
import de.uni_luebeck.isp.tessla.{Location, TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.ASTTransformation

import scala.collection.mutable

//TODO: Always pull out Set/Map/... constructor calls
//TODO: Make all identifier unique

class ASTPreprocessor(duplicateInlineCandidates: Boolean) extends TranslationPhase[Core.Specification, Core.Specification] {

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    var maxId: Long = spec.maxIdentifier
    val definitions = spec.definitions
    val newIDs: mutable.Map[(Identifier, Option[Identifier]), Identifier] = mutable.Map()

    var warnings = Seq()

    def nextIDNum : Long = {
      maxId += 1
      maxId
    }

    def newIDFromOld(id: Identifier, f: Option[Identifier]) : Identifier = {
      val newID = if (id.idOrName.left.isDefined) {
        Identifier(Ior.both(id.idOrName.left.get, nextIDNum))
      } else {
        freshId()
      }
      newIDs += ((id, f) -> newID)
      newID
    }

    def freshId() : Identifier = {
      Identifier(Ior.right(nextIDNum))
    }

    def getActID(id: Identifier, f: Option[Identifier]): Identifier = {
      newIDs.getOrElse((id, f), id)
    }

    def getExpRef(exp: ExpressionArg, add: mutable.Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression],
                  fPars: Map[Identifier, Identifier]):  ExpressionArg = {
      exp match {
        case TypeApplicationExpression(app, tArgs, loc) =>
          TypeApplicationExpression(getExpRef(app, add, scope, fPars), tArgs, loc)
        case exp : Expression =>
          val newID = freshId()
          add += (newID -> flattenDefExp(exp, add, scope, fPars, newID))
          ExpressionRef(newID, exp.tpe, Location.unknown)
        case e: ExpressionRef if duplicateInlineCandidates && ASTTransformation.isInlineExpression(e, scope) =>
          val newID = freshId()
          val newExp = ASTTransformation.getInlineExpression(e, scope).asInstanceOf[DefinitionExpression]
          add += (newID -> flattenDefExp(newExp, add, scope, fPars, newID))
          ExpressionRef(newID, exp.tpe, Location.unknown)
        case e: ExpressionRef => e
      }
    }

    def flattenDefExp(exp: DefinitionExpression, add: mutable.Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression],
                      fPars: Map[Identifier, Identifier], assignID: Identifier) : DefinitionExpression = {
      exp match {
        case FunctionExpression(typeParams, params, body, result, location) => {
          val newFPars = fPars ++ params.map{case (id, _, _) => (id, assignID)}
          val newBody = mutable.Map.from(flattenDefs(body, scope ++ body, newFPars))
          val newResult = getExpRef(result, newBody, scope ++ body, newFPars)
          FunctionExpression(typeParams, params.map{case (id, s, t) => (newIDFromOld(id, Some(assignID)), s, t)}, newBody.toMap, newResult, location)
        }
        case ApplicationExpression(applicable, args, location) =>
          applicable match {
            case ExternExpression(_, _, _, "lift", _)
               | ExternExpression(_, _, _, "slift", _)
               | TypeApplicationExpression(ExternExpression(_, _, _, "lift", _), _, _)
               | TypeApplicationExpression(ExternExpression(_, _, _, "slift", _), _, _) =>
              ApplicationExpression(applicable, args.dropRight(1).map(getExpRef(_, add, scope, fPars)) :+ args.last, location)
            case _: ExternExpression |
                 TypeApplicationExpression(_: ExternExpression, _, _) =>
              ApplicationExpression(applicable, args.map(getExpRef(_, add, scope, fPars)), location)
            case _ =>
              ApplicationExpression(getExpRef(applicable, add, scope, fPars), args.map(getExpRef(_, add, scope, fPars)), location)
          }
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(flattenExpArg(applicable, add, scope, fPars, assignID), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          val newEntries = entries.map{case (n, (e, l)) => (n, (getExpRef(e, add, scope, fPars), l))}
          RecordConstructorExpression(newEntries, location)
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, getExpRef(target, add, scope, fPars), nameLocation, location)
        case _ => exp
      }
    }

    def flattenExpArg(exp: ExpressionArg, add: mutable.Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression],
                      fPars: Map[Identifier, Identifier], assignID: Identifier) : ExpressionArg = {
      exp match {
        case exp: Expression => flattenDefExp(exp, add, scope, fPars, assignID)
        case exp: ExpressionRef => getExpRef(exp, add, scope, fPars)
      }
    }

    def flattenDefs(defs: Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression],
                    fPars: Map[Identifier, Identifier]): Map[Identifier, DefinitionExpression] = {
      val newDefs : mutable.Map[Identifier, DefinitionExpression] = mutable.Map()
      val transDefs = defs.map{case (id, defExp) => val nid = newIDFromOld(id, None); (nid -> flattenDefExp(defExp, newDefs, scope, fPars, nid))}
      transDefs ++ newDefs.toMap
    }

    def exchangeOldRefsDefExp(exp: DefinitionExpression, fPars: Map[Identifier, Identifier]) : DefinitionExpression = {
      def invNewIDs(id: Identifier) : (Identifier, Identifier) = {
        val entry = newIDs.map(_.swap)(id)
        (entry._1, entry._2.get)
      }

      exp match {
        case FunctionExpression(typeParams, params, body, result, location) =>
          val newFPars = fPars ++ params.map{ case (id, _, _) => invNewIDs(id)}
          FunctionExpression(typeParams, params, body.map{case (id, d) => (id -> exchangeOldRefsDefExp(d, newFPars))}, exchangeOldRefsExpArg(result, newFPars), location)
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(exchangeOldRefsExpArg(applicable, fPars), args.map(exchangeOldRefsExpArg(_, fPars)), location)
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(exchangeOldRefsExpArg(applicable, fPars), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          RecordConstructorExpression(entries.view.mapValues{case (e,l) => (exchangeOldRefsExpArg(e, fPars), l)}.toMap, location)
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, exchangeOldRefsExpArg(target, fPars), nameLocation, location)
        case _ =>
          exp
      }
    }

    def exchangeOldRefsExpArg(exp: ExpressionArg, fPars: Map[Identifier, Identifier]) : ExpressionArg = {
      exp match {
        case e: Expression => exchangeOldRefsDefExp(e, fPars)
        case ExpressionRef(id, tpe, location) => ExpressionRef(getActID(id, fPars.get(id)), tpe, location)
      }
    }


    def dupIfContentDefExp(e: DefinitionExpression, add: mutable.Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]): DefinitionExpression = {
      e match {
        case FunctionExpression(typeParams, params, body, result, location) => {
          val newBody = dupIfContent(body, scope ++ body)
          FunctionExpression(typeParams, params, newBody, result, location)
        }
        case ApplicationExpression(applicable, args, location) =>
          applicable match {
            case TypeApplicationExpression(ExternExpression(_, _, _, s, _), _, _) if Set("ite", "staticite", "and", "or").contains(s) =>
              val newArgs = args(0) +: args.drop(1).map(dupInlinedVars(_, add, scope))
              ApplicationExpression(applicable, newArgs, location)
            case _ => e
          }
        case _ => e
      }
    }

    def dupIfContent(defs: Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]): Map[Identifier, DefinitionExpression] = {
      val newDefs : mutable.Map[Identifier, DefinitionExpression] = mutable.Map()
      val transDefs = defs.map{case (id, defExp) => (id -> dupIfContentDefExp(defExp, newDefs, scope))}
      transDefs ++ newDefs.toMap
    }

    def dupInlinedVars(e: ExpressionArg, add: mutable.Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]): ExpressionArg = {
      e match {
        case e: Expression => dupInlinedVarsDefExp(e, add, scope)
        case ExpressionRef(id, tpe, loc) if !tpe.isInstanceOf[FunctionType] && scope.contains(id) =>
          val newId = freshId()
          add += (newId -> dupInlinedVarsDefExp(dupIfContentDefExp(scope(id), add, scope), add, scope))
          ExpressionRef(newId, tpe, loc)
        case _ => e
      }
    }

    def dupInlinedVarsDefExp(e: DefinitionExpression, add: mutable.Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]): DefinitionExpression = {
      e match {
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, dupInlinedVars(target, add, scope), nameLocation, location)
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(dupInlinedVars(applicable, add, scope), args.map(dupInlinedVars(_, add, scope)), location)
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(dupInlinedVars(applicable, add, scope), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          RecordConstructorExpression(entries.map{case (n, (e,l)) => (n, (dupInlinedVars(e, add, scope), l))}, location)
        case _ => e
      }
    }


    val newDefs = flattenDefs(definitions, definitions, Map()).map{case (id, d) => (id -> exchangeOldRefsDefExp(d, Map()))}
    val dupInlines = dupIfContent(newDefs, newDefs)


    Success(Core.Specification(spec.in.map{case (id, v) => (getActID(id, None), v)},
                               dupInlines,
                               spec.out.map{case (id, a) => (getActID(id, None), a)},
                               maxId),
            warnings)
  }

}

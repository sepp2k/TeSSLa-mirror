package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core._

class ControlFlowAnalysis(spec: Specification) {

  val nonInline : collection.mutable.Set[Identifier] = collection.mutable.HashSet()
  val inlinings : collection.mutable.HashMap[Identifier, Set[Identifier]] = collection.mutable.HashMap()

  def parseDefs(defs: Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]) : Unit = {
    val newScope = scope ++ defs
    defs.foreach{ case (id, expression) =>
      parseExp(id, expression, None, newScope)
    }
  }

  def parseExp(currId: Identifier, ea: ExpressionArg, inlineTo: Option[Identifier], scope: Map[Identifier, DefinitionExpression]) : Unit = {
    ea match {
      case FunctionExpression(_, params, body, result, _) =>
        parseDefs(body, scope)
        parseExp(currId, result, inlineTo, scope ++ body) //Dirty hack, we use currId, since we have no other
        nonInline ++= params.map(_._1)
      case ApplicationExpression(applicable, args, _) =>
        applicable match {
          case TypeApplicationExpression(ExternExpression(_, _, _, s, _), _, _) if Set("ite", "staticite", "and", "or").contains(s) =>
            parseExp(currId, args(0), inlineTo, scope)
            args.drop(1).foreach(parseExp(currId, _, Some(currId), scope))
          case _ =>
            parseExp(currId, applicable, inlineTo, scope)
            args.foreach(parseExp(currId, _, inlineTo, scope))
        }
      case TypeApplicationExpression(applicable, _, _) => parseExp(currId, applicable, inlineTo, scope)
      case RecordAccessorExpression(_, target, _, _) => parseExp(currId, target, inlineTo, scope)
      case RecordConstructorExpression(entries, _) => entries.foreach(e => parseExp(currId, e._2._1, inlineTo, scope))
      case ExpressionRef(id, tpe, _) if !tpe.isInstanceOf[FunctionType] && inlineTo.isDefined && scope.contains(id) =>
          inlinings += (id -> (inlinings.getOrElse(id, Set()) + inlineTo.get))
          parseExp(currId, scope(id), inlineTo, scope)
      case _ =>
    }
  }

  parseDefs(spec.definitions, Map())

  spec.in.foreach{case (id, _) =>
    nonInline += id
  }

  def varSuitableForInlining(id: Identifier): Boolean = {
    !nonInline.contains(id)
  }

  def getInlineTo(id: Identifier) : Option[Set[Identifier]] = {
    inlinings.get(id)
  }
}

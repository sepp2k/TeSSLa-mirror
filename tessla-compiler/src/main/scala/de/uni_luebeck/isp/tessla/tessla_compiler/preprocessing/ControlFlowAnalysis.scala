package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import de.uni_luebeck.isp.tessla.TesslaAST
import de.uni_luebeck.isp.tessla.TesslaAST.Core._

import scala.collection.mutable

/**
  * Perform control flow analysis of specification.
  * Provide information which streams are only evaluated if a certain condition is true and provide information
  * which identifiers cannot be inlined (parameter ids and input streams)
  * @param spec The specification which is examined
  */
class ControlFlowAnalysis(spec: Specification) {

  val usages: mutable.Map[TesslaAST.Core.Identifier, Set[TesslaAST.Core.Identifier]] = (new UsageMapCreation(spec)).getUsages

  val nonInline : collection.mutable.Set[Identifier] = collection.mutable.HashSet()
  val conditions : collection.mutable.HashMap[Identifier, (Set[Identifier], Set[Identifier])] = collection.mutable.HashMap()

  def parseDefs(defs: Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]) : Unit = {
    val newScope = scope ++ defs
    defs.foreach{ case (id, expression) =>
      parseExp(id, expression, newScope)
    }
  }

  def isIfApplicable(app: ExpressionArg, args: Seq[ExpressionArg]) : Boolean = {
    app match {
      case TypeApplicationExpression(app, _, _) => isIfApplicable(app, args)
      case ExternExpression(_, _, _, s, _) if Set("ite", "staticite").contains(s) => true
      case ExternExpression(_, _, _, s, _) if Set("lift", "slift").contains(s) => isIfApplicable(args.last, args.dropRight(1))
      case _ => false
    }
  }

  def parseExp(currId: Identifier, ea: ExpressionArg, scope: Map[Identifier, DefinitionExpression]) : Unit = {
    ea match {
      case FunctionExpression(_, params, body, result, _) =>
        parseDefs(body, scope)
        parseExp(currId, result, scope ++ body) //Dirty hack, we use currId, since we have no other
        nonInline ++= params.map(_._1)
      case ApplicationExpression(applicable, args, _) =>
        if (isIfApplicable(applicable, args)) {
            handleIf(args(0), args(1), args(2), scope)
        }
        parseExp(currId, applicable, scope)
        args.foreach(parseExp(currId, _, scope))
      case TypeApplicationExpression(applicable, _, _) => parseExp(currId, applicable, scope)
      case RecordAccessorExpression(_, target, _, _) => parseExp(currId, target, scope)
      case RecordConstructorExpression(entries, _) => entries.foreach(e => parseExp(currId, e._2._1, scope))
      case _ =>
    }
  }

  def handleIf(cond: ExpressionArg, ic: ExpressionArg, ec: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): Unit = {
    cond match {
      case r: ExpressionRef =>
        addIfCond(r.id, false, ic, scope)
        addIfCond(r.id, true, ec, scope)
      case _ => //TODO: Error
    }
  }

  def noAppOf(exp: ExpressionArg, of: String): Boolean = {
    exp match {
      case TypeApplicationExpression(applicable, _, _) => noAppOf(applicable, of)
      case ApplicationExpression(ExternExpression(_, _, _, of, _), _, _) => false
      case _ => true
    }
  }

  def addIfCond(cond: Identifier, neg: Boolean, exp: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): Unit = {
    if (!exp.tpe.isInstanceOf[FunctionType] && noAppOf(exp, "last")) {
      exp match {
        case ApplicationExpression(_, args, _) => args.foreach {
          addIfCond(cond, neg, _, scope)
        }
        case TypeApplicationExpression(applicable, _, _) => addIfCond(cond, neg, applicable, scope)
        case RecordConstructorExpression(entries, _) => entries.foreach { case (_, (e, _)) => addIfCond(cond, neg, e, scope) }
        case RecordAccessorExpression(_, target, _, _) => addIfCond(cond, neg, target, scope)
        case ExpressionRef(id, _, _) if !scope.contains(id) || noAppOf(scope(id), "lift") => addIfCondId(cond, neg, id, scope)
        case _ =>
      }
    }
  }

  def addIfCondId(cond: Identifier, neg: Boolean, id: Identifier, scope: Map[Identifier, DefinitionExpression]): Unit = {
    if (usages(id).size == 1 && !spec.out.exists(_._1.id == id)) {
      val preCond : (Set[Identifier], Set[Identifier])= conditions.getOrElse(id, (Set(), Set()))
      val newCond = if (neg) {
        (preCond._1, preCond._2 + cond)
      } else {
        (preCond._1 + cond, preCond._2)
      }
      conditions += (id -> newCond)
      if (scope.contains(id)) {
        addIfCond(cond, neg, scope(id), scope)
      }
    }
  }

  parseDefs(spec.definitions, Map())

  spec.in.foreach{case (id, _) =>
    nonInline += id
  }

  def varSuitableForInlining(id: Identifier): Boolean = {
    !nonInline.contains(id)
  }

  def getAddConditions(id: Identifier): (Set[Identifier], Set[Identifier]) = {
    conditions.getOrElse(id, (Set(), Set()))
  }

  def getAddOrderingConstraints: Set[(Identifier, Identifier)] = {
    conditions.toSeq.flatMap{case (id, c) => (c._1 ++ c._2).map((_, id))}.toSet
  }

  def addOrderingConstraints(in: Map[Identifier, Set[Identifier]]): Map[Identifier, Set[Identifier]] = {
    val addConst = getAddOrderingConstraints.groupBy(_._2).view.mapValues(s => s.map(_._1)).toMap
    (in.keys ++ addConst.keys).map(i => (i, in.getOrElse(i, Set()) ++ addConst.getOrElse(i, Set()))).toMap
  }

}

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

package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import de.uni_luebeck.isp.tessla.core.TesslaAST
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.ExtendedSpecification

import scala.collection.mutable

/**
 * Perform control flow analysis of specification.
 * Provide information which streams are only evaluated if a certain condition is true and provide information
 * which identifiers cannot be inlined (parameter ids and input streams)
 * @param spec The specification which is examined
 */
class ControlFlowAnalysis(spec: ExtendedSpecification) {

  val usages: Map[TesslaAST.Core.Identifier, Set[TesslaAST.Core.Identifier]] = spec.usageInfo.get

  val nonInline: collection.mutable.Set[Identifier] = collection.mutable.HashSet()
  val conditions: collection.mutable.HashMap[Identifier, (Set[Identifier], Set[Identifier])] =
    collection.mutable.HashMap()

  def parseDefs(defs: Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]): Unit = {
    val newScope = scope ++ defs
    defs.foreach {
      case (id, expression) =>
        parseExp(id, expression, newScope)
    }
  }

  def isIfApplicable(app: ExpressionArg, args: Seq[ExpressionArg]): Boolean = {
    app match {
      case TypeApplicationExpression(app, _, _)                             => isIfApplicable(app, args)
      case ExternExpression(s, _, _) if Set("ite", "staticite").contains(s) => true
      case ExternExpression(s, _, _) if Set("lift", "slift").contains(s) =>
        isIfApplicable(args.last, args.dropRight(1))
      case _ => false
    }
  }

  def parseExp(currId: Identifier, ea: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): Unit = {
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
      case RecordAccessorExpression(_, target, _, _)   => parseExp(currId, target, scope)
      case RecordConstructorExpression(entries, _)     => entries.foreach(e => parseExp(currId, e._2._1, scope))
      case _                                           =>
    }
  }

  def handleIf(
    cond: ExpressionArg,
    ic: ExpressionArg,
    ec: ExpressionArg,
    scope: Map[Identifier, DefinitionExpression]
  ): Unit = {
    cond match {
      case r: ExpressionRef =>
        addIfCond(r.id, false, ic, scope)
        addIfCond(r.id, true, ec, scope)
      case _ => //TODO: Error
    }
  }

  def noAppOf(exp: ExpressionArg, of: String): Boolean = {
    exp match {
      case TypeApplicationExpression(applicable, _, _)                       => noAppOf(applicable, of)
      case ApplicationExpression(ExternExpression(n, _, _), _, _) if n == of => false
      case _                                                                 => true
    }
  }

  def addIfCond(
    cond: Identifier,
    neg: Boolean,
    exp: ExpressionArg,
    scope: Map[Identifier, DefinitionExpression]
  ): Unit = {
    if (!exp.tpe.isInstanceOf[FunctionType] && noAppOf(exp, "last")) {
      exp match {
        case ApplicationExpression(_, args, _) =>
          args.foreach {
            addIfCond(cond, neg, _, scope)
          }
        case TypeApplicationExpression(applicable, _, _) => addIfCond(cond, neg, applicable, scope)
        case RecordConstructorExpression(entries, _) =>
          entries.foreach { case (_, (e, _)) => addIfCond(cond, neg, e, scope) }
        case RecordAccessorExpression(_, target, _, _) => addIfCond(cond, neg, target, scope)
        case ExpressionRef(id, _, _) if !scope.contains(id) || noAppOf(scope(id), "lift") =>
          addIfCondId(cond, neg, id, scope)
        case _ =>
      }
    }
  }

  def addIfCondId(
    cond: Identifier,
    neg: Boolean,
    id: Identifier,
    scope: Map[Identifier, DefinitionExpression]
  ): Unit = {
    if (usages(id).size == 1 && !spec.spec.out.exists(_._1.id == id)) {
      val preCond: (Set[Identifier], Set[Identifier]) = conditions.getOrElse(id, (Set(), Set()))
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

  parseDefs(spec.spec.definitions, Map())

  spec.spec.in.foreach {
    case (id, _) =>
      nonInline += id
  }

  def varSuitableForInlining(id: Identifier): Boolean = {
    !nonInline.contains(id)
  }

  def getAddConditions(id: Identifier): (Set[Identifier], Set[Identifier]) = {
    conditions.getOrElse(id, (Set(), Set()))
  }

  def getAddOrderingConstraints: Set[(Identifier, Identifier)] = {
    conditions.toSeq.flatMap { case (id, c) => (c._1 ++ c._2).map((_, id)) }.toSet
  }

  def addOrderingConstraints(in: Map[Identifier, Set[Identifier]]): Map[Identifier, Set[Identifier]] = {
    val addConst = getAddOrderingConstraints.groupBy(_._2).view.mapValues(s => s.map(_._1)).toMap
    (in.keys ++ addConst.keys).map(i => (i, in.getOrElse(i, Set()) ++ addConst.getOrElse(i, Set()))).toMap
  }

}

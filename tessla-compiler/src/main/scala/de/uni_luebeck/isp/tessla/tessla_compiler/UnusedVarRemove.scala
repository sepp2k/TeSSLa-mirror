/*
 * Copyright 2020 The TeSSLa Community
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

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

/**
 * This class is used for removing unused variable declarations from the code. Therefore it first creates a map
 * indicating which variable is used where and afterwards removes all variables which are not used in any expression.
 * This phase is especially important since it also removes variables which are not used anymore since their usage was
 * inlined due to lazy usage. Not removing them would probably cause errors because the evaluation of the expression
 * is not possible.
 */

object UnusedVarRemove extends TranslationPhase[SourceListing, SourceListing] {

  /**
   * Function triggering the translation from a SourceListing to a SourceListing without unused variables
   * @param listing The input source listing
   * @return A source listing without unused variables
   */
  override def translate(listing: SourceListing): Result[SourceListing] = {
    val outerStmts =
      listing.stepSource ++ listing.tailSource ++ listing.tsGenSource ++ listing.inputProcessing ++ listing.staticSource

    val usages = getUsageMap(outerStmts, Map())
    var usedIn = usages.foldLeft[Map[String, Set[String]]](Map()) {
      case (map, (k, v)) => v.foldLeft(map) { case (map, e) => map + (e -> (map.getOrElse(e, Set()) + k)) }
    }

    var newDel = (usages.keys.toSet -- usedIn.keys) - "*"
    val deleteVars: collection.mutable.Set[String] = collection.mutable.Set()

    while (newDel.nonEmpty) {

      usedIn = usedIn.view.mapValues(_.removedAll(newDel)).filter { case (k, _) => !newDel.contains(k) }.toMap
      deleteVars ++= newDel

      newDel = usedIn.flatMap { case (k, v) => if (v.isEmpty) Some(k) else None }.toSet - "*"
    }

    Success(listing.mapAll(removeAssignments(_, deleteVars.toSet)), Seq())
  }

  /**
   * Removes all assignments to variables in del from stmts
   * @param stmts The stmts where the variables are removed from
   * @param del The variables to remove assignments to
   * @return stmts without the removed assignments
   */
  private def removeAssignments(stmts: Seq[ImpLanStmt], del: Set[String]): Seq[ImpLanStmt] = {

    val f = (stmt: ImpLanStmt) => {
      stmt match {
        case Assignment(lhs, _, _, _) if del.contains(lhs.name)      => None
        case FinalAssignment(lhs, _, _, _) if del.contains(lhs.name) => None
        case _                                                       => Some(stmt)
      }
    }

    IntermediateCodeUtils.mapAST(stmts, identity, f)
  }

  /**
   * Returns a usage map where every variable is mapped to a place where it is used
   * @param stmts The statements from which the map is created
   * @param currMap A base map on which the new map is built on
   * @param scopeVar Variable indicating the scope (i.e. the variable where the lambda is stored to which is directly
   *                 surrounding the statements, or star character if there is non
   * @return Usage map where every variable name points to other variables where it is used in their definitions
   */
  private def getUsageMap(
    stmts: Seq[ImpLanStmt],
    currMap: Map[String, Set[String]],
    scopeVar: String = "*"
  ): Map[String, Set[String]] = {
    stmts.foldLeft(currMap) { case (c, s) => getUsageMapforStmt(c, s, scopeVar) }
  }

  /**
   * Includes the usage information of a single statement to a base usage map
   * @param stmt The statement which is examined
   * @param currMap A base usage map on which the new map is built on
   * @param scopeVar Variable indicating the scope (i.e. the variable where the lambda is stored to which is directly
   *                 surrounding the statements, or star character if there is non
   * @return Usage map where every variable name points to other variables where it is used in their definitions
   */
  private def getUsageMapforStmt(
    currMap: Map[String, Set[String]],
    stmt: ImpLanStmt,
    scopeVar: String
  ): Map[String, Set[String]] = {
    val prevMap = stmt match {
      case If(guard, stmts, elseStmts) =>
        getUsageMap(stmts, getUsageMap(elseStmts, getUsageMap(guard.flatten, currMap)))
      case TryCatchBlock(tr, cat) => getUsageMap(tr, getUsageMap(cat, currMap))
      case Assignment(lhs, rexpr, defExpr, _) =>
        currMap + (lhs.name -> currMap
          .getOrElse(lhs.name, Set())
          .union(getUsagesInExpr(rexpr))
          .union(if (defExpr.isDefined) getUsagesInExpr(defExpr.get) else Set()))
      case FinalAssignment(lhs, defExp, _, _) =>
        currMap + (lhs.name -> currMap.getOrElse(lhs.name, Set()).union(getUsagesInExpr(defExp)))
      case e: ImpLanExpr      => currMap + (scopeVar -> currMap.getOrElse(scopeVar, Set()).union(getUsagesInExpr(e)))
      case ReturnStatement(e) => currMap + (scopeVar -> currMap.getOrElse(scopeVar, Set()).union(getUsagesInExpr(e)))
    }

    val subScopeVar = stmt match {
      case Assignment(lhs, _, _, _)      => lhs.name
      case FinalAssignment(lhs, _, _, _) => lhs.name
      case _                             => scopeVar
    }

    getUsageMap(getLambdaBodies(Seq(stmt)), prevMap, subScopeVar)
  }

  /**
   * Returns the names of variables used in an expression
   * @param exp The expression to examine
   * @return Set of names of the unused variables
   */
  private def getUsagesInExpr(exp: ImpLanExpr): Set[String] = {
    val f = (curr: Seq[String], exp: ImpLanExpr) => {
      exp match {
        case Variable(name)                  => curr ++ Seq(name)
        case LambdaExpression(_, _, _, body) => val defines = getDefines(body); curr.filter(s => !defines(s))
        case _                               => curr
      }
    }

    IntermediateCodeUtils.foldAST(exp, Seq(), f, (n: Seq[String], _) => n).toSet
  }

  /**
   * Returns a list of variables defined in a set of statements
   * @param stmts The statements to be examined
   * @return List of defined variable names in stmts
   */
  private def getDefines(stmts: Seq[ImpLanStmt]): Set[String] = {
    stmts
      .map[Set[String]] {
        case If(_, stmts, elseStmts)       => getDefines(stmts) ++ getDefines(elseStmts)
        case TryCatchBlock(tr, cat)        => getDefines(tr) ++ getDefines(cat)
        case Assignment(lhs, _, _, _)      => Set(lhs.name)
        case FinalAssignment(lhs, _, _, _) => Set(lhs.name)
        case _                             => Set()
      }
      .reduce(_ ++ _)
  }

  /**
   * Extracts the statements from all lambda bodies in stmts
   * @param stmts The statements to be examined
   * @return List of statements from lambda bodies in stmts
   */
  private def getLambdaBodies(stmts: Seq[ImpLanStmt]): Seq[ImpLanStmt] = {
    val f = (curr: Seq[ImpLanStmt], exp: ImpLanExpr) => {
      exp match {
        case LambdaExpression(_, _, _, body) => curr ++ body
        case _                               => curr
      }
    }

    IntermediateCodeUtils.foldAST(stmts, Seq(), f, (n: Seq[ImpLanStmt], _) => n)
  }
}

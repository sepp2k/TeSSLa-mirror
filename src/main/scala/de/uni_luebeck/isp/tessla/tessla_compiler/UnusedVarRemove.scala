package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

object UnusedVarRemove extends TranslationPhase[SourceListing, SourceListing] {

  override def translate(listing: SourceListing): Result[SourceListing] = {
    val outerStmts = listing.stepSource ++ listing.tailSource ++ listing.tsGenSource ++ listing.inputProcessing ++ listing.staticSource

    val usages = getUsageMap(outerStmts, Map())
    var usedIn = usages.foldLeft[Map[String, Set[String]]](Map()) { case (map, (k, v)) => v.foldLeft(map) { case (map, e) => map + (e -> (map.getOrElse(e, Set()) + k)) } }

    var newDel = (usages.keys.toSet -- usedIn.keys) - "*"
    val deleteVars : collection.mutable.Set[String] = collection.mutable.Set()

    while (newDel.nonEmpty) {

      usedIn = usedIn.view.mapValues(_.removedAll(newDel)).filter{case (k,_) => !newDel.contains(k)}.toMap
      deleteVars ++= newDel

      newDel = usedIn.flatMap{case (k, v) => if (v.isEmpty) scala.Some(k) else scala.None}.toSet - "*"
    }

    Success(listing.mapAll(removeAssignments(_, deleteVars.toSet)), Seq())
  }

  def removeAssignments(stmts: Seq[ImpLanStmt], del: Set[String]) : Seq[ImpLanStmt] = {

    val f = (stmt: ImpLanStmt) => {
      stmt match {
        case Assignment(lhs, _, _, _) if del.contains(lhs.name) => scala.None
        case FinalAssignment(lhs, _, _, _) if del.contains(lhs.name) => scala.None
        case _ => scala.Some(stmt)
      }
    }

    IntermediateCodeUtils.mapAST(stmts, identity, f)
  }

  def getUsageMap(stmts: Seq[ImpLanStmt], currMap: Map[String, Set[String]], scopeVar: String = "*"): Map[String, Set[String]] = {
    stmts.foldLeft(currMap){ case (c,s) => getUsageMapforStmt(c, s, scopeVar)}
  }

  def getUsageMapforStmt(currMap: Map[String, Set[String]], stmt: ImpLanStmt, scopeVar: String): Map[String, Set[String]] = {
    val prevMap = stmt match {
      case If(guard, stmts, elseStmts) => getUsageMap(stmts, getUsageMap(elseStmts, getUsageMap(guard.flatten, currMap)))
      case TryCatchBlock(tr, cat) => getUsageMap(tr, getUsageMap(cat, currMap))
      case Assignment(lhs, rexpr, defExpr, _) => currMap + (lhs.name -> currMap.getOrElse(lhs.name, Set()).union(getUsagesInExpr(rexpr)).union(if (defExpr.isDefined) getUsagesInExpr(defExpr.get) else Set()))
      case FinalAssignment(lhs, defExp, _, _) => currMap + (lhs.name -> currMap.getOrElse(lhs.name, Set()).union(getUsagesInExpr(defExp)))
      case e: ImpLanExpr => currMap + (scopeVar -> currMap.getOrElse(scopeVar, Set()).union(getUsagesInExpr(e)))
      case ReturnStatement(e) => currMap + (scopeVar -> currMap.getOrElse(scopeVar, Set()).union(getUsagesInExpr(e)))
    }

    val subScopeVar = stmt match {
      case Assignment(lhs, _, _, _) => lhs.name
      case FinalAssignment(lhs, _, _, _) => lhs.name
      case _ => scopeVar
    }

    getUsageMap(getLambdaBodies(Seq(stmt)), prevMap, subScopeVar)
  }

  def getUsagesInExpr(exp: ImpLanExpr): Set[String] = {
    val f = (curr: Seq[String], exp: ImpLanExpr) => {
      exp match {
        case Variable(name) => curr ++ Seq(name)
        case LambdaExpression(_, _, _, body) => val defines =  getDefines(body); curr.filter(s => !defines(s))
        case _ => curr
      }
    }

    IntermediateCodeUtils.foldAST(exp, Seq(), f, (n : Seq[String], _) => n).toSet
  }

  def getDefines(stmts: Seq[ImpLanStmt]): Set[String] = {
    stmts.map[Set[String]]{
        case If(_, stmts, elseStmts) => getDefines(stmts) ++ getDefines(elseStmts)
        case TryCatchBlock(tr, cat) => getDefines(tr) ++ getDefines(cat)
        case Assignment(lhs, _, _, _) => Set(lhs.name)
        case FinalAssignment(lhs, _, _, _) => Set(lhs.name)
        case _ => Set()
    }.reduce(_ ++ _)
  }

  def getLambdaBodies(stmts: Seq[ImpLanStmt]): Seq[ImpLanStmt] = {
    val f = (curr: Seq[ImpLanStmt], exp: ImpLanExpr) => {
      exp match {
        case LambdaExpression(_, _, _, body) => curr ++ body
        case _ => curr
      }
    }

    IntermediateCodeUtils.foldAST(stmts, Seq(), f, (n : Seq[ImpLanStmt], _) => n)
  }
}

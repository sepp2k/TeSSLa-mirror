package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

//TODO: Watch out, in theory param names may have the same name as internal stream vars

object UnusedVarRemove extends TranslationPhase[SourceListing, SourceListing] {

  override def translate(listing: SourceListing): Result[SourceListing] = {
    val outerStmts = listing.stepSource ++ listing.tsGenSource ++ listing.inputProcessing ++ listing.staticSource
    val innerStmts = outerStmts.foldLeft[Seq[ImpLanStmt]](Seq()){case (curr, stmt) => curr ++ getLambdaBodies(stmt)}

    val usages = getUsageMap(innerStmts ++ outerStmts, Map())
    var usedIn = usages.foldLeft[Map[String, Set[String]]](Map()) { case (map, (k, v)) => v.foldLeft(map) { case (map, e) => map + (e -> (map.getOrElse(e, Set()) + k)) } }

    var newDel = (usages.keys.toSet -- usedIn.keys) - "*"
    val deleteVars : collection.mutable.Set[String] = collection.mutable.Set()

    while (newDel.nonEmpty) {
      usedIn = usedIn.view.mapValues(_.removedAll(newDel)).filter{case (k,_) => !newDel.contains(k)}.toMap
      deleteVars ++= newDel

      newDel = usedIn.flatMap{case (k, v) => if (v.isEmpty) scala.Some(k) else scala.None}.toSet - "*"
    }

    Success(SourceListing(removeAssignments(listing.stepSource, deleteVars.toSet),
                          removeAssignments(listing.tsGenSource, deleteVars.toSet),
                          removeAssignments(listing.inputProcessing, deleteVars.toSet),
                          removeAssignments(listing.staticSource, deleteVars.toSet),
    ), Seq())
  }

  def removeAssignments(stmts: Seq[ImpLanStmt], del: Set[String]) : Seq[ImpLanStmt] = {
    stmts.flatMap(removeAssignments(_, del))
  }

  def removeAssignments(stmt: ImpLanStmt, del: Set[String]) : Option[ImpLanStmt] = {
    stmt match {
      case Assignment(lhs, _, _, _) if del.contains(lhs.name) => scala.None
      case FinalAssignment(lhs, _, _) if del.contains(lhs.name) => scala.None

      case If(guard, stmts, elseStmts) => scala.Some(If(guard.map(_.map(removeAssignments(_, del))), removeAssignments(stmts, del), removeAssignments(elseStmts, del)))
      case Assignment(lhs, rexpr, scala.None, typ) => scala.Some(Assignment(lhs, removeAssignments(rexpr, del), scala.None, typ))
      case Assignment(lhs, rexpr, scala.Some(defExp), typ) => scala.Some(Assignment(lhs, removeAssignments(rexpr, del), scala.Some(removeAssignments(defExp, del)), typ))
      case FinalAssignment(lhs, defExp, typ) => scala.Some(FinalAssignment(lhs, removeAssignments(defExp, del), typ))
      case e: ImpLanExpr => scala.Some(removeAssignments(e, del))
      case ReturnStatement(exp) => scala.Some(ReturnStatement(removeAssignments(exp, del)))
    }
  }

  def removeAssignments(exp: ImpLanExpr, del: Set[String]) : ImpLanExpr = {
    exp match {
      case CastingExpression(e, target) => CastingExpression(removeAssignments(e, del), target)
      case FunctionCall(name, params, typeHint) => FunctionCall(name, params.map(removeAssignments(_, del)), typeHint)
      case LambdaApplication(exp, params) => LambdaApplication(removeAssignments(exp, del), params.map(removeAssignments(_, del)))
      case Addition(op1, op2) => Addition(removeAssignments(op1, del), removeAssignments(op2, del))
      case Subtraction(op1, op2) => Subtraction(removeAssignments(op1, del), removeAssignments(op2, del))
      case BitwiseOr(op1, op2) => BitwiseOr(removeAssignments(op1, del), removeAssignments(op2, del))
      case TernaryExpression(guard, e1, e2) => TernaryExpression(guard.map(_.map(removeAssignments(_, del))), removeAssignments(e1, del), removeAssignments(e2, del))
      case Equal(a, b) => Equal(removeAssignments(a, del), removeAssignments(b, del))
      case NotEqual(a, b) => NotEqual(removeAssignments(a, del), removeAssignments(b, del))
      case Greater(a, b) => Greater(removeAssignments(a, del), removeAssignments(b, del))
      case GreaterEqual(a, b) => GreaterEqual(removeAssignments(a, del), removeAssignments(b, del))
      case Negation(a) => Negation(removeAssignments(a, del))
      case LambdaExpression(argNames, argsTypes, retType, body) => LambdaExpression(argNames, argsTypes, retType, removeAssignments(body, del))
      case _ => exp
    }
  }

  def getUsageMap(stmts: Seq[ImpLanStmt], currMap: Map[String, Set[String]]): Map[String, Set[String]] = {
    stmts.foldLeft(currMap)(getUsageMapforStmt)
  }

  @scala.annotation.tailrec
  def getUsageMapforStmt(currMap: Map[String, Set[String]], stmt: ImpLanStmt): Map[String, Set[String]] = {
    stmt match {
      case If(guard, stmts, elseStmts) => getUsageMap(stmts, getUsageMap(elseStmts, getUsageMap(guard.flatten, currMap)))
      case Assignment(lhs, rexpr, defExpr, _) => currMap + (lhs.name -> currMap.getOrElse(lhs.name, Set()).union(getUsagesInExpr(rexpr)).union(if (defExpr.isDefined) getUsagesInExpr(defExpr.get) else Set()))
      case FinalAssignment(lhs, defExp, _) => currMap + (lhs.name -> currMap.getOrElse(lhs.name, Set()).union(getUsagesInExpr(defExp)))
      case e: ImpLanExpr => currMap + ("*" -> currMap.getOrElse("*", Set()).union(getUsagesInExpr(e)))
      case ReturnStatement(exp) => getUsageMapforStmt(currMap, exp)
    }
  }

  def getUsagesInExpr(exp: ImpLanExpr): Set[String] = {
    val f = (curr: Seq[String], exp: ImpLanExpr) => {
      exp match {
        case Variable(name) => curr ++ Seq(name)
        case _ => curr
      }
    }

    IntermediateCodeUtils.expressionFold(exp, Seq(), f).toSet
  }

  def getLambdaBodies(stmt: ImpLanStmt): Seq[ImpLanStmt] = {
    val f = (curr: Seq[ImpLanStmt], exp: ImpLanExpr) => {
      exp match {
        case LambdaExpression(_, _, _, body) => curr ++ body
        case _ => curr
      }
    }

    //FIXME: Only top-level
    IntermediateCodeUtils.extractExpressions(stmt).foldLeft[Seq[ImpLanStmt]](Seq())(f)
  }
}

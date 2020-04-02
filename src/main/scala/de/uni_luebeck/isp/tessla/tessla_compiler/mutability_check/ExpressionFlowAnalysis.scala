package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.ExpressionFlowAnalysis.IdentifierDependencies
import de.uni_luebeck.isp.tessla.util.Lazy

object ExpressionFlowAnalysis {


  //TODO: --> STDLIB Annotations ?!
  //TODO: Set[Set] ...

  final case class IdentifierDependencies(reads: Set[Identifier],
                                          writes: Set[Identifier],
                                          reps: Set[Identifier],
                                          pass: Set[Identifier],
                                          deps: Set[Identifier],
                                          immut: Set[Identifier],
                                          calls: Set[(Identifier, Identifier)]) {

    def ++(o: IdentifierDependencies): IdentifierDependencies = {
      IdentifierDependencies(
        reads ++ o.reads,
        writes ++ o.writes,
        reps ++ o.reps,
        pass ++ o.pass,
        deps ++ o.deps,
        immut ++ o.immut,
        calls ++ o.calls
      )
    }

    def removeAll(ids: Set[Identifier]): IdentifierDependencies = {
      IdentifierDependencies(
        reads -- ids,
        writes -- ids,
        reps -- ids,
        pass -- ids,
        deps -- ids,
        immut -- ids,
        calls.filter(id => !ids.contains(id._1) && !ids.contains(id._2))
      )
    }

    def mapAll(f: Set[Identifier] => Set[Identifier]) : IdentifierDependencies = {
      IdentifierDependencies(
        f(reads),
        f(writes),
        f(reps),
        f(pass),
        f(deps),
        f(immut),
        calls
      )
    }

  }

  object IdentifierDependencies {
    def empty: IdentifierDependencies = {
      IdentifierDependencies(Set(), Set(), Set(), Set(), Set(), Set(), Set())
    }
  }

  def getExpArgID(e: ExpressionArg) : Identifier = {
    e match {
      case _: Expression => ??? //TODO: Error
      case ExpressionRef(id, _, _) => id
    }
  }

}

class ExpressionFlowAnalysis(val impCheck: ImplicationChecker) {

  //TODO: Maybe duplicate exists somewhere else
  def getExpsFlow(exps: Set[(Identifier, ExpressionArg)], scope: Map[Identifier, DefinitionExpression],
                  fixedPoints: Map[Identifier, IdentifierDependencies]): IdentifierDependencies = {
    if (exps.isEmpty) {
      IdentifierDependencies.empty
    } else {
      exps.map{case (i, e) => getExpFlow(i, e, scope, fixedPoints)}.reduce(_ ++ _)
    }
  }

  def getExpFlow(resID: Identifier, e: ExpressionArg, scope: Map[Identifier, DefinitionExpression],
                 fixedPoints: Map[Identifier, IdentifierDependencies], onlyParams: Boolean = false): IdentifierDependencies = {

    val idDeps = e match {
      case FunctionExpression(_, params, body, result, _) =>
        val exps = body.toSeq :+ (resID, result)
        val deps = getExpsFlow(exps.toSet, scope ++ body, fixedPoints)
          if (onlyParams) {
            deps.mapAll(_.intersect(params.map(_._1).toSet))
          } else {
            deps.removeAll(body.keys.toSet ++ params.map(_._1))
          }
      case ApplicationExpression(applicable, args, _) =>
        getLiftFlow(resID, applicable, args, scope, fixedPoints)
      case TypeApplicationExpression(applicable, _, _) => getExpFlow(resID, applicable, scope, fixedPoints)
      case RecordConstructorExpression(entries, _) =>
        val entryIDs = entries.map{ case (_, (exp, _)) => ExpressionFlowAnalysis.getExpArgID(exp)}.toSet
        IdentifierDependencies(Set(), Set(), Set(), entryIDs, entryIDs, entryIDs, Set())
      case RecordAccessorExpression(_, target, _, _) => getExpFlow(resID, target, scope, fixedPoints)
      case ExpressionRef(id, _, _) => IdentifierDependencies(Set(), Set(), Set(), Set(id), Set(id), Set(), Set())
      case _ => IdentifierDependencies.empty
    }

    idDeps
  }

  def getExpRefCallFixedPoint(resID: Identifier, liftID: Identifier, argExps: Seq[ExpressionArg],
                              scope: Map[Identifier, DefinitionExpression],
                              fixedPoints: Map[Identifier, IdentifierDependencies]) : IdentifierDependencies = {

    var oldFixedPoint = IdentifierDependencies.empty
    var newFixedPoint = IdentifierDependencies.empty

    do {
      oldFixedPoint = newFixedPoint
      newFixedPoint = oldFixedPoint ++ getExpFlow(resID, scope(liftID), scope, fixedPoints + (liftID -> oldFixedPoint), true)
    } while (newFixedPoint != oldFixedPoint)

    newFixedPoint
  }

  //TODO: Functions which are used outside of applications must be made immutable in all params
  def getLiftFlow(resID: Identifier, liftExpr: ExpressionArg, argExps: Seq[ExpressionArg], scope: Map[Identifier, DefinitionExpression],
                  fixedPoints: Map[Identifier, IdentifierDependencies]) : IdentifierDependencies = {

    val argsL = Lazy(argExps.map(a => ExpressionFlowAnalysis.getExpArgID(a)))
    def args: Seq[TesslaAST.Core.Identifier] = argsL.get

    liftExpr match {
      case r : ExpressionRef if scope.contains(r.id) && !scope(r.id).isInstanceOf[FunctionExpression] =>
        getLiftFlow(resID, scope(r.id), argExps, scope, fixedPoints)

      case r : ExpressionRef if scope.contains(r.id) =>
        val flow = if (fixedPoints.contains(r.id)) fixedPoints(r.id) else getExpRefCallFixedPoint(resID, r.id, argExps, scope, fixedPoints)
        val fe = scope(r.id).asInstanceOf[FunctionExpression]

        val depsPerParam = fe.params.map(_._1).zip(argExps.map(ExpressionFlowAnalysis.getExpArgID)).toMap
        def replaceParams(ids: Set[Identifier]): Set[Identifier] = {
          ids.map(id => depsPerParam.getOrElse(id, id))
        }

        val adjExpFlow = IdentifierDependencies(flow.reads -- flow.writes, flow.writes, flow.reps, flow.pass, flow.deps,
                                                flow.immut, flow.calls)
        val transDeps = adjExpFlow.mapAll(replaceParams)
        val feResID = ExpressionFlowAnalysis.getExpArgID(fe.result)

        IdentifierDependencies(transDeps.reads, transDeps.writes, transDeps.reps, transDeps.pass, transDeps.deps,
                               transDeps.immut, transDeps.calls ++ depsPerParam.toSet + (feResID -> resID))

      case _ : ExpressionRef /*Function is param*/ =>
        IdentifierDependencies(Set(), Set(), Set(), Set(), Set(), args.toSet + resID, Set())
      case TypeApplicationExpression(e, _, _) =>
        getLiftFlow(resID, e, argExps, scope, fixedPoints)
      case ExternExpression(_, _, _, "Set_empty", _) |
           ExternExpression(_, _, _, "Map_empty", _) |
           ExternExpression(_, _, _, "List_empty", _) =>
        IdentifierDependencies.empty
      case ExternExpression(_, _, _, "Map_add", _) =>
        IdentifierDependencies(Set(), Set(args(0)), Set(), Set(), args.toSet, Set(args(1), args(2)), Set())
      case ExternExpression(_, _, _, "Set_add", _) |
           ExternExpression(_, _, _, "Map_remove", _) |
           ExternExpression(_, _, _, "Set_remove", _) |
           ExternExpression(_, _, _, "List_append", _) =>
        IdentifierDependencies(Set(), Set(args(0)), Set(), Set(), args.toSet, Set(args(1)), Set())
      case ExternExpression(_, _, _, "List_set", _) =>
        IdentifierDependencies(Set(), Set(args(0)), Set(), Set(), args.toSet, Set(args(2)), Set())
      case ExternExpression(_, _, _, "List_prepend", _) =>
        IdentifierDependencies(Set(), Set(args(1)), Set(), Set(), args.toSet, Set(args(0)), Set())
      case ExternExpression(_, _, _, "Map_contains", _) |
           ExternExpression(_, _, _, "Map_keys", _) |
           ExternExpression(_, _, _, "Set_contains", _) |
           ExternExpression(_, _, _, "Set_fold", _) |
           ExternExpression(_, _, _, "Map_fold", _) |
           ExternExpression(_, _, _, "List_fold", _) |
           ExternExpression(_, _, _, "Map_size", _) |
           ExternExpression(_, _, _, "Set_size", _) |
           ExternExpression(_, _, _, "List_size", _) |
           ExternExpression(_, _, _, "List_tail", _) | //TODO: Pass???
           ExternExpression(_, _, _, "List_init", _) =>
        IdentifierDependencies(Set(args(0)), Set(), Set(), Set(), args.toSet, Set(), Set())
      case ExternExpression(_, _, _, "Map_get", _) |
           ExternExpression(_, _, _, "List_get", _) =>
        IdentifierDependencies(Set(args(0)), Set(), Set(), Set(), args.toSet, Set(resID), Set())
      case ExternExpression(_, _, _, "Set_minus", _) |
           ExternExpression(_, _, _, "Set_union", _) |
           ExternExpression(_, _, _, "Set_intersection", _) =>
        IdentifierDependencies(Set(args(1)), Set(args(0)), Set(), Set(), args.toSet, Set(), Set())

      case ExternExpression(_, _, _, "nil", _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(), Set(), Set(), Set())
      case ExternExpression(_, _, _, "default", _) |
           ExternExpression(_, _, _, "defaultFrom", _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(args(0), args(1)), Set(args(0), args(1)), Set(), Set())
      case ExternExpression(_, _, _, "last", _) =>
        if (MutabilityChecker.mutabilityCheckRelevantStreamType(argExps(0).tpe)) {
          IdentifierDependencies(Set(), Set(), Set(args(0)), Set(), Set(args(1)), Set(), Set((resID, args(0))))
        } else {
          IdentifierDependencies(Set(), Set(), Set(), Set(), Set(args(1)), Set(), Set())
        }
      case ExternExpression(_, _, _, "lift", _) =>
        getLiftFlow(resID, argExps.last, argExps.dropRight(1), scope, fixedPoints)
      case ExternExpression(_, _, _, "slift", _) =>
        val dep = getLiftFlow(resID, argExps.last, argExps.dropRight(1), scope, fixedPoints)
        val addReps = (dep.reads ++ dep.writes ++ dep.pass).filter(i => !impCheck.freqImplication(resID, i, true))
          IdentifierDependencies(dep.reads, dep.writes, dep.reps ++ addReps, dep.pass, dep.deps, dep.immut, dep.calls)
      case ExternExpression(_, _, _, "merge", _) =>
        IdentifierDependencies(Set(), Set(), Set(), args.toSet, args.toSet, Set(), Set())
      case ExternExpression(_, _, _, "time", _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(), Set(args(0)), Set(), Set())
      case ExternExpression(_,  _, _, "delay",  _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(), Set(args(0), args(1)), Set(), Set())
      case ExternExpression(_,  _, _, "getSome",  _) |
           ExternExpression(_,  _, _, "Some",  _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(args(0)), Set(args(0)), Set(), Set())
      case ExternExpression(_,  _, _, "ite",  _) |
           ExternExpression(_,  _, _, "staticite",  _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(args(1), args(2)), args.toSet, Set(), Set())
      case _: ExternExpression =>
        //Fallback
        IdentifierDependencies(Set(), Set(), Set(), Set(), args.toSet, Set(), Set())
      case _: ApplicationExpression |
           _: RecordAccessorExpression => ??? //TODO: In WC every identifier of the specification could be used
      case _ => ???
    }
  }

}

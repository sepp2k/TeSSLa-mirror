package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.ExpressionFlowAnalysis.IdentifierDependencies
import de.uni_luebeck.isp.tessla.util.Lazy

/**
  * Class for analysis which functions modify which parameters in which way (write/read/pass access...)
  */

object ExpressionFlowAnalysis {


  /**
    * Information which identifiers are used in which way
    * @param reads Set of identifiers with read access
    * @param writes Set of identifiers with write access
    * @param reps Set of identifiers whose last value is used
    * @param pass Set of identifiers which are passed unchanged
    * @param deps All identifiers the expression depends on
    * @param immut Set of identifiers which have to be immutable in every case, e.g. because they are added to a set etc.
    * @param calls Pair of identifiers which have to be both mutable or immutable though no pass edge between them exists
    */
  final case class IdentifierDependencies(reads: Set[Identifier] = Set(),
                                          writes: Set[Identifier] = Set(),
                                          reps: Set[Identifier] = Set(),
                                          pass: Set[Identifier] = Set(),
                                          deps: Set[Identifier] = Set(),
                                          immut: Set[Identifier] = Set(),
                                          calls: Set[(Identifier, Identifier)] = Set()) {

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

  def getExpsFlow(exps: Set[(Identifier, ExpressionArg)], scope: Map[Identifier, DefinitionExpression],
                  fixedPoints: Map[Identifier, IdentifierDependencies], resId: Identifier): IdentifierDependencies = {
    val expMap : Map[Identifier, ExpressionArg] = exps.toMap
    var expFlows : Map[Identifier, IdentifierDependencies] = Map()

    def getWithPass(stack: Set[Identifier])(s: Set[Identifier]) : Set[Identifier] = {
      s  ++ s.flatMap{i => recGetExpFlow(i, stack).pass}
    }

    def recGetExpFlow(id: Identifier, stack: Set[Identifier] = Set()) : IdentifierDependencies = {
      if (stack.contains(id)) {
        IdentifierDependencies.empty
      } else if (expFlows.contains(id)) {
        expFlows(id)
      } else if (expMap.contains(id)) {
        val flow = getExpFlow(id, expMap(id), scope, fixedPoints)
        val deps = flow.mapAll(getWithPass(stack + id))
        expFlows += (id -> deps)

        deps
      } else {
        IdentifierDependencies.empty
      }
    }

    expMap.keys.foreach(i => recGetExpFlow(i))
    expFlows.values.reduce(_ ++ _).copy(pass = expFlows(resId).pass)
  }

  def getExpFlow(resID: Identifier, e: ExpressionArg, scope: Map[Identifier, DefinitionExpression],
                 fixedPoints: Map[Identifier, IdentifierDependencies], onlyParams: Boolean = false): IdentifierDependencies = {

    val idDeps = e match {
      case FunctionExpression(_, params, body, result, _) =>
        val exps = body.toSeq :+ (resID, result)
        val deps = getExpsFlow(exps.toSet, scope ++ body, fixedPoints, result.asInstanceOf[ExpressionRef].id)
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
        IdentifierDependencies(immut = args.toSet + resID)
      case TypeApplicationExpression(e, _, _) =>
        getLiftFlow(resID, e, argExps, scope, fixedPoints)
      case ExternExpression(_, _, _, "Set_empty", _) |
           ExternExpression(_, _, _, "Map_empty", _) |
           ExternExpression(_, _, _, "List_empty", _) |
           ExternExpression(_, _, _, "Queue_empty", _)=>
        IdentifierDependencies.empty
      case ExternExpression(_, _, _, "Map_add", _) =>
        IdentifierDependencies(writes = Set(args(0)), deps = args.toSet, immut = Set(args(1), args(2)))
      case ExternExpression(_, _, _, "Set_add", _) |
           ExternExpression(_, _, _, "Queue_enq", _) |
           ExternExpression(_, _, _, "Map_remove", _) |
           ExternExpression(_, _, _, "Set_remove", _) |
           ExternExpression(_, _, _, "List_append", _) =>
        IdentifierDependencies(writes = Set(args(0)), deps = args.toSet, immut = Set(args(1)))
      case ExternExpression(_, _, _, "Queue_deq", _) =>
        IdentifierDependencies(writes = Set(args(0)), deps = args.toSet)
      case ExternExpression(_, _, _, "List_set", _) =>
        IdentifierDependencies(writes = Set(args(0)), deps = args.toSet, immut = Set(args(2)))
      case ExternExpression(_, _, _, "List_prepend", _) =>
        IdentifierDependencies(writes =  Set(args(1)), deps =  args.toSet, immut = Set(args(0)))
      case ExternExpression(_, _, _, "Map_contains", _) |
           ExternExpression(_, _, _, "Map_keys", _) |
           ExternExpression(_, _, _, "Set_contains", _) |
           ExternExpression(_, _, _, "Set_fold", _) |
           ExternExpression(_, _, _, "Map_fold", _) |
           ExternExpression(_, _, _, "List_fold", _) |
           ExternExpression(_, _, _, "Map_size", _) |
           ExternExpression(_, _, _, "Set_size", _) |
           ExternExpression(_, _, _, "Queue_size", _) |
           ExternExpression(_, _, _, "Queue_first", _) |
           ExternExpression(_, _, _, "List_size", _) |
           ExternExpression(_, _, _, "List_tail", _) |
           ExternExpression(_, _, _, "List_init", _) =>
        IdentifierDependencies(reads = Set(args(0)), deps = args.toSet)
      case ExternExpression(_, _, _, "Map_get", _) |
           ExternExpression(_, _, _, "List_get", _) =>
        IdentifierDependencies(reads = Set(args(0)), deps = args.toSet, immut = Set(resID))
      case ExternExpression(_, _, _, "Set_minus", _) |
           ExternExpression(_, _, _, "Set_union", _) |
           ExternExpression(_, _, _, "Set_intersection", _) =>
        IdentifierDependencies(reads = Set(args(1)), writes = Set(args(0)), deps = args.toSet)

      case ExternExpression(_, _, _, "nil", _) =>
        IdentifierDependencies.empty
      case ExternExpression(_, _, _, "default", _) |
           ExternExpression(_, _, _, "defaultFrom", _) =>
        IdentifierDependencies(pass = Set(args(0), args(1)), deps = Set(args(0), args(1)))
      case ExternExpression(_, _, _, "last", _) =>
        if (MutabilityChecker.mutabilityCheckRelevantStreamType(argExps(0).tpe)) {
          IdentifierDependencies(reps = Set(args(0)), deps = Set(args(1)), calls = Set((resID, args(0))))
        } else {
          IdentifierDependencies(deps = Set(args(1)))
        }
      case ExternExpression(_, _, _, "lift", _) =>
        getLiftFlow(resID, argExps.last, argExps.dropRight(1), scope, fixedPoints)
      case ExternExpression(_, _, _, "slift", _) =>
        val dep = getLiftFlow(resID, argExps.last, argExps.dropRight(1), scope, fixedPoints)
        val addReps = (dep.reads ++ dep.writes ++ dep.pass).filter(i => !impCheck.freqImplication(resID, i, true))
          IdentifierDependencies(dep.reads, dep.writes, dep.reps ++ addReps, dep.pass, dep.deps, dep.immut, dep.calls)
      case ExternExpression(_, _, _, "merge", _) =>
        IdentifierDependencies(pass = args.toSet, deps = args.toSet)
      case ExternExpression(_, _, _, "time", _) =>
        IdentifierDependencies(deps = Set(args(0)))
      case ExternExpression(_,  _, _, "delay",  _) =>
        IdentifierDependencies(deps = Set(args(1)))
      case ExternExpression(_,  _, _, "getSome",  _) |
           ExternExpression(_,  _, _, "Some",  _) =>
        IdentifierDependencies(pass = Set(args(0)), deps = Set(args(0)))
      case ExternExpression(_,  _, _, "ite",  _) |
           ExternExpression(_,  _, _, "staticite",  _) =>
        IdentifierDependencies(pass = Set(args(1), args(2)), deps = args.toSet)
      case _: ExternExpression =>
        //Fallback
        IdentifierDependencies(deps = args.toSet)
      case _: ApplicationExpression |
           _: RecordAccessorExpression => ???
      case _ => ???
    }
  }

}

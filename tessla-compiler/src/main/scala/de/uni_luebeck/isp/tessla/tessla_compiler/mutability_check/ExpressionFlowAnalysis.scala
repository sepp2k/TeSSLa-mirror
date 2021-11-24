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

package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.core.TesslaAST
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.ExpressionFlowAnalysis.IdentifierDependencies
import de.uni_luebeck.isp.tessla.core.util.Lazy

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
  final case class IdentifierDependencies(
    reads: Set[Identifier] = Set(),
    writes: Set[Identifier] = Set(),
    reps: Set[Identifier] = Set(),
    pass: Set[Identifier] = Set(),
    deps: Set[Identifier] = Set(),
    immut: Set[Identifier] = Set(),
    calls: Set[(Identifier, Identifier)] = Set()
  ) {

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

    def mapAll(f: Set[Identifier] => Set[Identifier]): IdentifierDependencies = {
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

  def getExpArgID(e: ExpressionArg): Identifier = {
    e match {
      case _: Expression           => ??? //TODO: Error
      case ExpressionRef(id, _, _) => id
    }
  }

}

class ExpressionFlowAnalysis(val impCheck: ImplicationChecker) {

  def getExpsFlow(
    exps: Set[(Identifier, ExpressionArg)],
    scope: Map[Identifier, DefinitionExpression],
    fixedPoints: Map[Identifier, IdentifierDependencies],
    resId: Identifier
  ): IdentifierDependencies = {
    val expMap: Map[Identifier, ExpressionArg] = exps.toMap
    var expFlows: Map[Identifier, IdentifierDependencies] = Map()

    def getWithPass(stack: Set[Identifier])(s: Set[Identifier]): Set[Identifier] = {
      s ++ s.flatMap { i => recGetExpFlow(i, stack).pass }
    }

    def recGetExpFlow(id: Identifier, stack: Set[Identifier] = Set()): IdentifierDependencies = {
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

  def getExpFlow(
    resID: Identifier,
    e: ExpressionArg,
    scope: Map[Identifier, DefinitionExpression],
    fixedPoints: Map[Identifier, IdentifierDependencies],
    onlyParams: Boolean = false
  ): IdentifierDependencies = {

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
        val entryIDs = entries.map { case (_, (exp, _)) => ExpressionFlowAnalysis.getExpArgID(exp) }.toSet
        IdentifierDependencies(Set(), Set(), Set(), entryIDs, entryIDs, entryIDs, Set())
      case RecordAccessorExpression(_, target, _, _) => getExpFlow(resID, target, scope, fixedPoints)
      case ExpressionRef(id, _, _)                   => IdentifierDependencies(Set(), Set(), Set(), Set(id), Set(id), Set(), Set())
      case _                                         => IdentifierDependencies.empty
    }

    idDeps
  }

  def getExpRefCallFixedPoint(
    resID: Identifier,
    liftID: Identifier,
    argExps: Seq[ExpressionArg],
    scope: Map[Identifier, DefinitionExpression],
    fixedPoints: Map[Identifier, IdentifierDependencies]
  ): IdentifierDependencies = {

    var oldFixedPoint = IdentifierDependencies.empty
    var newFixedPoint = IdentifierDependencies.empty

    do {
      oldFixedPoint = newFixedPoint
      newFixedPoint =
        oldFixedPoint ++ getExpFlow(resID, scope(liftID), scope, fixedPoints + (liftID -> oldFixedPoint), true)

    } while (newFixedPoint != oldFixedPoint)

    newFixedPoint
  }

  //TODO: Functions which are used outside of applications must be made immutable in all params
  def getLiftFlow(
    resID: Identifier,
    liftExpr: ExpressionArg,
    argExps: Seq[ExpressionArg],
    scope: Map[Identifier, DefinitionExpression],
    fixedPoints: Map[Identifier, IdentifierDependencies]
  ): IdentifierDependencies = {

    val argsL = Lazy(argExps.map(a => ExpressionFlowAnalysis.getExpArgID(a)))
    def args: Seq[TesslaAST.Core.Identifier] = argsL.get

    liftExpr match {
      case r: ExpressionRef if scope.contains(r.id) && !scope(r.id).isInstanceOf[FunctionExpression] =>
        getLiftFlow(resID, scope(r.id), argExps, scope, fixedPoints)

      case r: ExpressionRef if scope.contains(r.id) =>
        val flow =
          if (fixedPoints.contains(r.id)) fixedPoints(r.id)
          else getExpRefCallFixedPoint(resID, r.id, argExps, scope, fixedPoints)
        val fe = scope(r.id).asInstanceOf[FunctionExpression]

        val depsPerParam = fe.params.map(_._1).zip(argExps.map(ExpressionFlowAnalysis.getExpArgID)).toMap
        def replaceParams(ids: Set[Identifier]): Set[Identifier] = {
          ids.map(id => depsPerParam.getOrElse(id, id))
        }

        val adjExpFlow = IdentifierDependencies(
          flow.reads -- flow.writes,
          flow.writes,
          flow.reps,
          flow.pass,
          flow.deps,
          flow.immut,
          flow.calls
        )
        val transDeps = adjExpFlow.mapAll(replaceParams)
        val feResID = ExpressionFlowAnalysis.getExpArgID(fe.result)

        IdentifierDependencies(
          transDeps.reads,
          transDeps.writes,
          transDeps.reps,
          transDeps.pass,
          transDeps.deps,
          transDeps.immut,
          transDeps.calls ++ depsPerParam.toSet + (feResID -> resID)
        )

      case _: ExpressionRef /*Function is param*/ =>
        IdentifierDependencies(immut = args.toSet + resID)
      case TypeApplicationExpression(e, _, _) =>
        getLiftFlow(resID, e, argExps, scope, fixedPoints)
      case ExternExpression("Set_empty", _, _) | ExternExpression("Map_empty", _, _) |
          ExternExpression("List_empty", _, _) | ExternExpression("Queue_empty", _, _) =>
        IdentifierDependencies.empty
      case ExternExpression("Map_add", _, _) =>
        IdentifierDependencies(writes = Set(args(0)), deps = args.toSet, immut = Set(args(1), args(2)))
      case ExternExpression("Set_add", _, _) | ExternExpression("Queue_enq", _, _) |
          ExternExpression("Map_remove", _, _) | ExternExpression("Set_remove", _, _) |
          ExternExpression("List_append", _, _) =>
        IdentifierDependencies(writes = Set(args(0)), deps = args.toSet, immut = Set(args(1)))
      case ExternExpression("Queue_deq", _, _) =>
        IdentifierDependencies(writes = Set(args(0)), deps = args.toSet)
      case ExternExpression("List_set", _, _) =>
        IdentifierDependencies(writes = Set(args(0)), deps = args.toSet, immut = Set(args(2)))
      case ExternExpression("List_prepend", _, _) =>
        IdentifierDependencies(writes = Set(args(1)), deps = args.toSet, immut = Set(args(0)))
      case ExternExpression("Map_contains", _, _) | ExternExpression("Map_keys", _, _) |
          ExternExpression("Set_contains", _, _) | ExternExpression("Set_fold", _, _) |
          ExternExpression("Map_fold", _, _) | ExternExpression("List_fold", _, _) |
          ExternExpression("Map_size", _, _) | ExternExpression("Set_size", _, _) |
          ExternExpression("Queue_size", _, _) | ExternExpression("Queue_first", _, _) |
          ExternExpression("List_size", _, _) | ExternExpression("List_tail", _, _) |
          ExternExpression("List_init", _, _) =>
        IdentifierDependencies(reads = Set(args(0)), deps = args.toSet)
      case ExternExpression("Map_get", _, _) | ExternExpression("Map_sortedKeys", _, _) |
          ExternExpression("List_get", _, _) =>
        IdentifierDependencies(reads = Set(args(0)), deps = args.toSet, immut = Set(resID))
      case ExternExpression("Set_minus", _, _) | ExternExpression("Set_union", _, _) |
          ExternExpression("Set_intersection", _, _) =>
        IdentifierDependencies(reads = Set(args(1)), writes = Set(args(0)), deps = args.toSet)

      case ExternExpression("nil", _, _) =>
        IdentifierDependencies.empty
      case ExternExpression("default", _, _) | ExternExpression("defaultFrom", _, _) =>
        IdentifierDependencies(pass = Set(args(0), args(1)), deps = Set(args(0), args(1)))
      case ExternExpression("last", _, _) =>
        if (MutabilityChecker.mutabilityCheckRelevantStreamType(argExps(0).tpe)) {
          IdentifierDependencies(reps = Set(args(0)), deps = Set(args(1)), calls = Set((resID, args(0))))
        } else {
          IdentifierDependencies(deps = Set(args(1)))
        }
      case ExternExpression("lift", _, _) =>
        getLiftFlow(resID, argExps.last, argExps.dropRight(1), scope, fixedPoints)
      case ExternExpression("slift", _, _) =>
        val dep = getLiftFlow(resID, argExps.last, argExps.dropRight(1), scope, fixedPoints)
        val addReps = (dep.reads ++ dep.writes ++ dep.pass).filter(i => !impCheck.freqImplication(resID, i, true))
        IdentifierDependencies(dep.reads, dep.writes, dep.reps ++ addReps, dep.pass, dep.deps, dep.immut, dep.calls)
      case ExternExpression("merge", _, _) =>
        IdentifierDependencies(pass = args.toSet, deps = args.toSet)
      case ExternExpression("time", _, _) =>
        IdentifierDependencies(deps = Set(args(0)))
      case ExternExpression("delay", _, _) =>
        IdentifierDependencies(deps = Set(args(1)))
      case ExternExpression("getSome", _, _) | ExternExpression("Some", _, _) =>
        IdentifierDependencies(pass = Set(args(0)), deps = Set(args(0)))
      case ExternExpression("ite", _, _) | ExternExpression("staticite", _, _) =>
        IdentifierDependencies(pass = Set(args(1), args(2)), deps = args.toSet)
      case _: ExternExpression =>
        //Fallback
        IdentifierDependencies(deps = args.toSet)
      case _: ApplicationExpression | _: RecordAccessorExpression => ???
      case _                                                      => ???
    }
  }

}

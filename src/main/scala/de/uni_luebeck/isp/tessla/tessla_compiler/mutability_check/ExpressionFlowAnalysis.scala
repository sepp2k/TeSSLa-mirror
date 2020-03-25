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
                                          calls: Set[(Identifier, Identifier)]) {

    def ++(o: IdentifierDependencies): IdentifierDependencies = {
      IdentifierDependencies(
        reads ++ o.reads,
        writes ++ o.writes,
        reps ++ o.reps,
        pass ++ o.pass,
        deps ++ o.deps,
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
        calls
      )
    }

  }

  object IdentifierDependencies {
    def empty: IdentifierDependencies = {
      IdentifierDependencies(Set(), Set(), Set(), Set(), Set(), Set())
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
  def getExpsFlow(exps: Set[(Identifier, ExpressionArg)], scope: Map[Identifier, DefinitionExpression]): IdentifierDependencies = {
    if (exps.isEmpty) {
      IdentifierDependencies.empty
    } else {
      exps.map{case (i, e) => getExpFlow(i, e, scope)}.reduce(_ ++ _)
    }
  }

  def getExpFlow(resID: Identifier, e: ExpressionArg, scope: Map[Identifier, DefinitionExpression], onlyParams: Boolean = false): IdentifierDependencies = {

    val idDeps = e match {
      case FunctionExpression(_, params, body, result, _) =>
        val exps = body.toSeq :+ (resID, result)
        val deps = getExpsFlow(exps.toSet, scope ++ body)
          if (onlyParams) {
            deps.mapAll(_.intersect(params.map(_._1).toSet))
          } else {
            deps.removeAll(body.keys.toSet)
          }
      case ApplicationExpression(applicable, args, _) =>
        getLiftFlow(resID, applicable, args, scope)
      case TypeApplicationExpression(applicable, _, _) => getExpFlow(resID, applicable, scope)
      case RecordConstructorExpression(entries, _) =>
        val entryIDs = entries.map{ case (_, (exp, _)) => ExpressionFlowAnalysis.getExpArgID(exp)}.toSet
        IdentifierDependencies(Set(), Set(), Set(), entryIDs, entryIDs, Set())
      case RecordAccessorExpression(_, target, _, _) => getExpFlow(resID, target, scope)
      case ExpressionRef(id, _, _) => IdentifierDependencies(Set(), Set(), Set(), Set(id), Set(id), Set())
      case _ => IdentifierDependencies.empty
    }

    idDeps
  }

  def getLiftFlow(resID: Identifier, liftExpr: ExpressionArg, argExps: Seq[ExpressionArg], scope: Map[Identifier, DefinitionExpression]) : IdentifierDependencies = {

    val argsL = Lazy(argExps.map(a => ExpressionFlowAnalysis.getExpArgID(a)))
    def args: Seq[TesslaAST.Core.Identifier] = argsL.get

    liftExpr match {
      case fe: FunctionExpression =>
        val depsPerParam = fe.params.map(_._1).zip(args).toMap
        def replaceParams(ids: Set[Identifier]): Set[Identifier] = {
          ids.map(id => depsPerParam.getOrElse(id, id))
        }

        val transDeps = getExpFlow(resID, fe, scope, true).mapAll(replaceParams)
        val feResID = ExpressionFlowAnalysis.getExpArgID(fe.result)

        IdentifierDependencies(transDeps.reads, transDeps.writes, transDeps.reps, transDeps.pass, transDeps.deps, transDeps.calls ++ depsPerParam.toSet + (feResID -> resID))
      case r: ExpressionRef =>
        getLiftFlow(resID, scope(r.id), argExps, scope)
      case TypeApplicationExpression(e, _, _) =>
        getLiftFlow(resID, e, argExps, scope)
      case ExternExpression(_, _, _, "Set_empty", _) |
           ExternExpression(_, _, _, "Map_empty", _) |
           ExternExpression(_, _, _, "List_empty", _) =>
        IdentifierDependencies.empty
      case ExternExpression(_, _, _, "Map_add", _) |
           ExternExpression(_, _, _, "Set_add", _) |
           ExternExpression(_, _, _, "Map_remove", _) |
           ExternExpression(_, _, _, "Set_remove", _) |
           ExternExpression(_, _, _, "List_append", _) |
           ExternExpression(_, _, _, "List_set", _) =>
        IdentifierDependencies(Set(), Set(args(0)), Set(), Set(), args.toSet, Set())
      case ExternExpression(_, _, _, "List_prepend", _) =>
        IdentifierDependencies(Set(), Set(args(1)), Set(), Set(), args.toSet, Set())
      case ExternExpression(_, _, _, "Map_contains", _) |
           ExternExpression(_, _, _, "Map_get", _) |
           ExternExpression(_, _, _, "Map_keys", _) |
           ExternExpression(_, _, _, "Set_contains", _) |
           ExternExpression(_, _, _, "List_get", _) |
           ExternExpression(_, _, _, "Set_fold", _) |
           ExternExpression(_, _, _, "Map_fold", _) |
           ExternExpression(_, _, _, "List_fold", _) |
           ExternExpression(_, _, _, "Map_size", _) |
           ExternExpression(_, _, _, "Set_size", _) |
           ExternExpression(_, _, _, "List_size", _) |
           ExternExpression(_, _, _, "List_tail", _) |
           ExternExpression(_, _, _, "List_init", _) =>
        IdentifierDependencies(Set(args(0)), Set(), Set(), Set(), args.toSet, Set())
      case ExternExpression(_, _, _, "Set_minus", _) |
           ExternExpression(_, _, _, "Set_union", _) |
           ExternExpression(_, _, _, "Set_intersection", _) =>
        IdentifierDependencies(Set(args(1)), Set(args(0)), Set(), Set(), args.toSet, Set())//TODO: Really?

      case ExternExpression(_, _, _, "nil", _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(), Set(), Set())
      case ExternExpression(_, _, _, "default", _) |
           ExternExpression(_, _, _, "defaultFrom", _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(args(0), args(1)), Set(args(0), args(1)), Set())
      case ExternExpression(_, _, _, "last", _) =>
        if (MutabilityChecker.mutabilityCheckRelevantStreamType(argExps(0).tpe)) {
          IdentifierDependencies(Set(), Set(), Set(args(0)), Set(), Set(args(1)), Set())
        } else {
          IdentifierDependencies(Set(), Set(), Set(), Set(), Set(args(1)), Set())
        }
      case ExternExpression(_, _, _, "lift", _) =>
        getLiftFlow(resID, argExps.last, argExps.dropRight(1), scope)
      case ExternExpression(_, _, _, "slift", _) =>
        val dep = getLiftFlow(resID, argExps.last, argExps.dropRight(1), scope)
        val addReps = (dep.reads ++ dep.writes).filter(i => !impCheck.freqImplication(resID, i))
          IdentifierDependencies(dep.reads, dep.writes, dep.reps ++ addReps, dep.pass, dep.deps, dep.calls)
      case ExternExpression(_, _, _, "merge", _) =>
        IdentifierDependencies(Set(), Set(), Set(), args.toSet, args.toSet, Set())
      case ExternExpression(_, _, _, "time", _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(), Set(args(0)), Set())
      case ExternExpression(_,  _, _, "delay",  _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(), Set(args(0), args(1)), Set())
      case ExternExpression(_,  _, _, "getSome",  _) |
           ExternExpression(_,  _, _, "Some",  _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(args(0)), Set(args(0)), Set())
      case ExternExpression(_,  _, _, "ite",  _) |
           ExternExpression(_,  _, _, "staticite",  _) =>
        IdentifierDependencies(Set(), Set(), Set(), Set(args(1), args(2)), args.toSet, Set())
      case _: ExternExpression =>
        //Fallback
        IdentifierDependencies(Set(), Set(), Set(), Set(), args.toSet, Set())
      case _: ApplicationExpression |
           _: RecordAccessorExpression => ??? //TODO: In WC every identifier of the specification could be used
      case _ => ???
    }
  }

}

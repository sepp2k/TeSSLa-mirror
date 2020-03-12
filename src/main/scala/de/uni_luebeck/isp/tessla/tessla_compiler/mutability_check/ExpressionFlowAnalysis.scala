package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.ExpressionFlowAnalysis.IdentifierDependencies

object ExpressionFlowAnalysis {

  //TODO: --> STDLIB Annotations ?!
  //TODO: Set[Set] ...

  def getOutputTypeForExternExpression(id: Option[Identifier], e: ExternExpression, args: Seq[ExpressionArg],
                                       idTypes: Identifier => Type, impCheck: ImplicationChecker,
                                       scope: Map[Identifier, DefinitionExpression]) : Type = {
    val expFlowAnalysis = new ExpressionFlowAnalysis(impCheck)
    val flowDef = expFlowAnalysis.getLiftFlow(id, e, args, scope)
    val pass = flowDef.pass ++ flowDef.reps ++ flowDef.writes
    if (pass.nonEmpty) idTypes(pass.head) else e.tpe.asInstanceOf[FunctionType].resultType
  }

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

}

class ExpressionFlowAnalysis(val impCheck: ImplicationChecker) {

  final case class Dependencies(reads: Set[ExpressionArg],
                                writes: Set[ExpressionArg],
                                reps: Set[ExpressionArg],
                                pass: Set[ExpressionArg],
                                deps: Set[ExpressionArg],
                                calls: Set[(Identifier, Identifier)]) { //TODO: Replace by function

    def toIdentifierDependencies(scope: Map[Identifier, DefinitionExpression]): IdentifierDependencies = {
      val rf = getExpsFlow(reads, scope)
      val wf = getExpsFlow(writes, scope)
      val ef = getExpsFlow(reps, scope)
      val pf = getExpsFlow(pass, scope)
      val df = getExpsFlow(deps, scope)

      IdentifierDependencies(
        rf.reads ++ wf.reads ++ ef.reads ++ pf.reads ++ df.reads ++ rf.pass,
        rf.writes ++ wf.writes ++ ef.writes ++ pf.writes ++ df.writes ++ wf.pass,
        rf.reps ++ wf.reps ++ ef.reps ++ pf.reps ++ df.reps ++ ef.pass,
        pf.pass,
        df.deps,
        rf.calls ++ wf.calls ++ ef.calls ++ pf.calls ++ df.calls
      )
    }

  }

  //TODO: Maybe duplicate exists somewhere else
  def getExpsFlow(exps: Set[ExpressionArg], scope: Map[Identifier, DefinitionExpression]): IdentifierDependencies = {
    if (exps.isEmpty) {
      IdentifierDependencies.empty
    } else {
      exps.map(getExpFlow(_, scope)).reduce(_ ++ _)
    }
  }

  def getExpFlow(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression], withParams: Boolean = false): IdentifierDependencies = {
      e match {
      case FunctionExpression(_, params, body, result, _) =>
        val exps : Seq[ExpressionArg] = body.values.toSeq :+ result
        val deps = getExpsFlow(exps.toSet, scope ++ body)
          if (withParams) {
            deps
          } else {
            deps.removeAll(params.map(_._1).toSet)
          }
      case ApplicationExpression(applicable, args, _) =>
        getLiftFlow(None, applicable, args, scope)
      case TypeApplicationExpression(applicable, _, _) => getExpFlow(applicable, scope)
      case RecordConstructorExpression(entries, _) => getExpsFlow(entries.map(e => e._2._1).toSet, scope)
      case RecordAccesorExpression(_, target, _, _) => getExpFlow(target, scope)
      case ExpressionRef(id, _, _) => IdentifierDependencies(Set(), Set(), Set(), Set(id), Set(id), Set())
      case _ => IdentifierDependencies.empty
    }
  }

  def getLiftFlow(id: Option[Identifier], liftExpr: ExpressionArg, args: Seq[ExpressionArg], scope: Map[Identifier, DefinitionExpression]) : IdentifierDependencies = {

    liftExpr match {
      case fe: FunctionExpression =>
        val depsPerParam = fe.params.map(_._1).zip(args.map(getExpFlow(_, scope).pass)).toMap
        def replaceParams(ids: Set[Identifier]): Set[Identifier] = {
          ids.flatMap(id => depsPerParam.getOrElse(id, Set(id)))
        }

        val transDeps = getExpFlow(fe, scope, true).mapAll(replaceParams)
        val argsDeps = getExpsFlow(args.toSet, scope)

        //TODO: Tie arguments and params
        transDeps ++ argsDeps
      case r: ExpressionRef =>
        getLiftFlow(id, scope(r.id), args, scope)
      case TypeApplicationExpression(e, _, _) =>
        getLiftFlow(id, e, args, scope)
      case ExternExpression(_, _, _, "Set_empty", _) |
           ExternExpression(_, _, _, "Map_empty", _) |
           ExternExpression(_, _, _, "List_empty", _) =>
        IdentifierDependencies.empty
      case ExternExpression(_, _, _, "Map_add", _) |
           ExternExpression(_, _, _, "Set_add", _) |
           ExternExpression(_, _, _, "Map_remove", _) |
           ExternExpression(_, _, _, "Set_remove", _) |
           ExternExpression(_, _, _, "List_prepend", _) |
           ExternExpression(_, _, _, "List_append", _) |
           ExternExpression(_, _, _, "List_set", _) =>
        Dependencies(Set(), Set(args(0)), Set(), Set(), args.toSet, Set()).toIdentifierDependencies(scope)
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
        Dependencies(Set(args(0)), Set(), Set(), Set(), args.toSet, Set()).toIdentifierDependencies(scope)
      case ExternExpression(_, _, _, "Set_minus", _) |
           ExternExpression(_, _, _, "Set_union", _) |
           ExternExpression(_, _, _, "Set_intersection", _) =>
        Dependencies(args.toSet, Set(), Set(), Set(), args.toSet, Set()).toIdentifierDependencies(scope)//TODO: Really?

      case ExternExpression(_, _, _, "nil", _) =>
        Dependencies(Set(), Set(), Set(), Set(), Set(), Set()).toIdentifierDependencies(scope)
      case ExternExpression(_, _, _, "default", _) =>
        Dependencies(Set(), Set(), Set(), Set(args(0)), Set(args(0)), Set()).toIdentifierDependencies(scope)
      case ExternExpression(_, _, _, "defaultFrom", _) =>
        Dependencies(Set(), Set(), Set(), Set(args(0), args(1)),
                     Set(args(0), args(1)), Set()).toIdentifierDependencies(scope)
      case ExternExpression(_, _, _, "last", _) =>
        if (MutabilityChecker.mutabilityCheckRelevantStreamType(args(0).tpe)) {
          Dependencies(Set(), Set(), Set(args(0)), Set(), Set(args(1)), Set()).toIdentifierDependencies(scope)
        } else {
          Dependencies(Set(), Set(), Set(), Set(), Set(args(1)), Set()).toIdentifierDependencies(scope)
        }
      case ExternExpression(_, _, _, "lift", _) =>
        getLiftFlow(id, args.last, args.dropRight(1), scope)
      case ExternExpression(_, _, _, "slift", _) =>
        val dep = getLiftFlow(id, args.last, args.dropRight(1), scope)
        val addReps = (dep.reads ++ dep.writes).filter(i => id.isEmpty || !impCheck.freqImplication(id.get, i))
          IdentifierDependencies(dep.reads, dep.writes, dep.reps ++ addReps, dep.pass, dep.deps, Set())
      case ExternExpression(_, _, _, "merge", _) =>
          Dependencies(Set(), Set(), Set(), args.toSet, args.toSet, Set()).toIdentifierDependencies(scope)
      case ExternExpression(_, _, _, "time", _) =>
          Dependencies(Set(), Set(), Set(), Set(), Set(args(0)), Set()).toIdentifierDependencies(scope)
      case ExternExpression(_,  _, _, "delay",  _) =>
          Dependencies(Set(), Set(), Set(), Set(), Set(args(0), args(1)), Set()).toIdentifierDependencies(scope)
      case _: ApplicationExpression => ??? //TODO: In WC every identifier of the specification could be used
      case _ => ???
    }
  }

}

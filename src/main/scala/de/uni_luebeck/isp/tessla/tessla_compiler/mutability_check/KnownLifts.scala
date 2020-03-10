package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.FunctionType
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.MutabilityChecker.mutabilityCheckRelevantType

object KnownLifts {

  //TODO: --> STDLIB Annotations ?!
  //TODO: Set[Set] ...
  def getDependenciesFromLift(liftExpr: ExpressionArg, args: Seq[ExpressionArg]) : MutabilityChecker.Dependencies = { //reads, writes, reps, pass, deps
    liftExpr match {
      case TypeApplicationExpression(e, _, _) => getDependenciesFromLift(e, args)
      case ExternExpression(_, _, _, "Map_add", _) |
           ExternExpression(_, _, _, "Set_add", _) |
           ExternExpression(_, _, _, "Map_remove", _) |
           ExternExpression(_, _, _, "Set_remove", _) |
           ExternExpression(_, _, _, "List_prepend", _) |
           ExternExpression(_, _, _, "List_append", _) |
           ExternExpression(_, _, _, "List_set", _) => MutabilityChecker.Dependencies(Set(), Set(MutabilityChecker.getUsedStream(args(0))), Set(), Set(), args.map(MutabilityChecker.getUsedStream).toSet)
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
           ExternExpression(_, _, _, "List_init", _) => MutabilityChecker.Dependencies(Set(MutabilityChecker.getUsedStream(args(0))), Set(), Set(), Set(), args.map(MutabilityChecker.getUsedStream).toSet)
      case ExternExpression(_, _, _, "Set_minus", _) |
           ExternExpression(_, _, _, "Set_union", _) |
           ExternExpression(_, _, _, "Set_intersection", _) => MutabilityChecker.Dependencies(args.map(MutabilityChecker.getUsedStream).toSet, Set(), Set(), Set(), args.map(MutabilityChecker.getUsedStream).toSet) //TODO: Really?
      case _ => getFallbackDependencies(liftExpr, args)
    }
  }

  def getFallbackDependencies(liftExpr: ExpressionArg, args: Seq[ExpressionArg]) : MutabilityChecker.Dependencies = {
    val objArgs = args.filter{arg => mutabilityCheckRelevantType(arg.tpe)}.map(MutabilityChecker.getUsedStream).toSet
    MutabilityChecker.Dependencies(Set(), objArgs, Set(), Set(), args.map(MutabilityChecker.getUsedStream).toSet)
  }

  def mutabilityCheckRelevantType(tpe: Type) : Boolean = {
    tpe match {
      case InstatiatedType("Events", ta, _) => MutabilityChecker.mutabilityCheckRelevantType(ta.head)
      case _ => false
    }
  }

}

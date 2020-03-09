package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.tessla_compiler.{DefinitionOrdering, Errors}
import de.uni_luebeck.isp.tessla.tessla_compiler.Errors.CoreASTError
import de.uni_luebeck.isp.tessla.{TesslaAST, TranslationPhase}

class TesslaCoreWithMutabilityInfo(val spec: TesslaAST.Core.Specification, val mutableStreams: Set[Identifier]) {
  override def toString = s"${spec}\nMutable streams:${mutableStreams.mkString(", ")}"

}

object MutabilityChecker extends
  TranslationPhase[TesslaAST.Core.Specification, TesslaCoreWithMutabilityInfo] {

  override def translate(spec: TesslaAST.Core.Specification): TranslationPhase.Result[TesslaCoreWithMutabilityInfo] = {

    val in = spec.in
    val definitions = spec.definitions
    val out = spec.out
    val mutableStreams : Set[Identifier] = Set()

    val simplifiedGraph = generateSimplifiedGraph(spec)

    Success(new TesslaCoreWithMutabilityInfo(TesslaAST.Core.Specification(in, definitions, out, spec.maxIdentifier), mutableStreams), Seq())

  }

  def generateSimplifiedGraph(spec: TesslaAST.Core.Specification): (Set[Identifier], Set[(Identifier, Identifier)]) = {
    val nodes: collection.mutable.ArrayBuffer[Identifier] = collection.mutable.ArrayBuffer()
    val edges: collection.mutable.ArrayBuffer[(Identifier, Identifier)] = collection.mutable.ArrayBuffer()
    val immutVars: collection.mutable.ArrayBuffer[Identifier] = collection.mutable.ArrayBuffer()

    val readMap: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val writeMap: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val invRepsMap: collection.mutable.ArrayBuffer[(Identifier, Identifier)] = collection.mutable.ArrayBuffer()

    val cfDependencies: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val passDependencies: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()

    val variableFamilies : UnionFind[Identifier] = new UnionFind()

    val coloring = new ImplicationChecker(spec)

    def processStreamDef(id: Identifier, exp: ExternExpression, args: Seq[ExpressionArg]) : Unit = {
      val dep = getAllDependencies(id, exp, args, coloring)

      if (dep.reads != Set() || dep.writes != Set() || dep.reps != Set()) {
        nodes += id

        def getDeps(add: Set[Identifier]): Set[Identifier] = add.flatMap{i => passDependencies.getOrElse(i, Set(i))}

        getDeps(dep.reads).foreach{i => readMap += (i -> (readMap.getOrElse(i, Set()) + id))}

        getDeps(dep.writes).foreach{i =>
          variableFamilies.union(i, id)
          writeMap += (i -> (writeMap.getOrElse(i, Set()) + id))
        }

        dep.reps.foreach{i =>
          invRepsMap += (i -> id)
          variableFamilies.union(i, id)
        }

        dep.deps.foreach {
          i => if (nodes.contains(i)) {
            edges += ((i,id))
          } else {
            cfDependencies.getOrElse(i, Set()).foreach{i => edges += ((i,id))}
          }
        }

      } else {
        cfDependencies += (id -> dep.deps.map{d => if (cfDependencies.contains(d)) cfDependencies(d) else Set(d)}.foldLeft[Set[Identifier]](Set()){case (c,s) => c ++ s})
        if (dep.pass.nonEmpty) {
          passDependencies += (id -> dep.pass.map { d => if (passDependencies.contains(d)) passDependencies(d) else Set(d) }.foldLeft[Set[Identifier]](Set()) { case (c, s) => c ++ s })
        }
        cfDependencies(id).foreach(i => if (!nodes.contains(i)) nodes += i)

        dep.pass.foreach{i => variableFamilies.union(id, i)}
      }
    }

    DefinitionOrdering.order(spec.definitions).foreach { case (id, defExpr) => {
      defExpr.tpe match {
        case InstantiatedType("Events", _, _) => defExpr match {
          case ApplicationExpression(TypeApplicationExpression(e: ExternExpression, _, _), args, _) => processStreamDef(id, e, args)
          case ApplicationExpression(e: ExternExpression, args, _) => processStreamDef(id, e, args)
          case e => throw Errors.CoreASTError("Non valid stream defining expression cannot be translated", e.location)
        }
        case _ =>
      }
      }
    }

    val repsMap : Map[Identifier, Set[(Identifier, Identifier)]] = invRepsMap.flatMap{case (k,v) =>
      passDependencies.getOrElse(k, Set(k)).map((_,(v,k)))
    }.groupBy(_._1).view.mapValues{vs => vs.map(_._2).toSet}.toMap


    (nodes.toSet, edges.toSet)
  }

  final case class Dependencies(reads: Set[Identifier],
                          writes: Set[Identifier],
                          reps: Set[Identifier],
                          pass: Set[Identifier],
                          deps: Set[Identifier])

  def getAllDependencies(id: Identifier, defExp: ExternExpression, args: Seq[ExpressionArg], coloring: ImplicationChecker) : Dependencies = {

    defExp.name match {
      case "nil" => Dependencies(Set(), Set(), Set(), Set(), Set())
      case "default" => Dependencies(Set(), Set(), Set(), Set(getUsedStream(args(0))), Set(getUsedStream(args(0))))
      case "defaultFrom" => Dependencies(Set(), Set(), Set(), Set(getUsedStream(args(0)), getUsedStream(args(1))), Set(getUsedStream(args(0)), getUsedStream(args(1))))
      case "time" => Dependencies(Set(), Set(), Set(), Set(), Set(getUsedStream(args(0))))
      case "last" => {
        if (KnownLifts.mutabilityCheckRelevantType(args(0).tpe)) {
          val valStream  = getUsedStream(args(0))
          Dependencies(Set(), Set(), Set(valStream), Set(), Set(getUsedStream(args(1))))
        } else {
          Dependencies(Set(), Set(), Set(), Set(), Set(getUsedStream(args(1))))
        }

      }
      case "delay" => Dependencies(Set(), Set(), Set(), Set(), Set(getUsedStream(args(0)), getUsedStream(args(1))))
      case "lift" => KnownLifts.getDependenciesFromLift(args.last, args.dropRight(1))
      case "slift" => {
        val dep = KnownLifts.getDependenciesFromLift(args.last, args.dropRight(1))
        val addReps = (dep.reads ++ dep.writes).filter(i => !coloring.freqImplication(id, i))
        Dependencies(dep.reads, dep.writes, dep.reps ++ addReps, dep.pass, dep.deps)
      }
      case "merge" => Dependencies(Set(), Set(), Set(), args.map(getUsedStream).toSet, args.map(getUsedStream).toSet)
      case _ => throw Errors.CommandNotSupportedError(defExp.toString)
    }
  }

  def getUsedStream(e: ExpressionArg) : Identifier = {
    e match {
      case ExpressionRef(id, _, _) => id
      case e: Expression => throw CoreASTError(s"Required ExpressionRef, but Expression found: $e", e.location)
    }
  }

  def mutabilityCheckRelevantType(tpe: Type) : Boolean = { //TODO: TypeArg
    tpe match {
      case InstantiatedType("Map", _, _) |
           InstantiatedType("Set", _, _) |
           InstantiatedType("List", _, _) => true
      case RecordType(entries, _) => entries.values.map{case (t,_) => mutabilityCheckRelevantType(t)}.reduce(_ || _)
      case _ => false
    }
  }

}

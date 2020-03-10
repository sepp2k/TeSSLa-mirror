package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.tessla_compiler.{DefinitionOrdering, Errors}
import de.uni_luebeck.isp.tessla.tessla_compiler.Errors.CoreASTError
import de.uni_luebeck.isp.tessla.{TesslaAST, TranslationPhase}

import scala.collection.mutable

class TesslaCoreWithMutabilityInfo(val spec: TesslaAST.Core.Specification, val mutableStreams: Set[Identifier], val addDeps: Map[Identifier, Set[Identifier]]) {
  override def toString = s"${spec}\nMutable streams:${mutableStreams.mkString(", ")}"
}

object MutabilityChecker extends
  TranslationPhase[TesslaAST.Core.Specification, TesslaCoreWithMutabilityInfo] {

  override def translate(spec: TesslaAST.Core.Specification): TranslationPhase.Result[TesslaCoreWithMutabilityInfo] = {

    val in = spec.in
    val definitions = spec.definitions
    val out = spec.out

    val nodes: collection.mutable.ArrayBuffer[Identifier] = collection.mutable.ArrayBuffer()
    val edges: collection.mutable.ArrayBuffer[(Identifier, Identifier)] = collection.mutable.ArrayBuffer()
    val immutVars: collection.mutable.ArrayBuffer[Identifier] = collection.mutable.ArrayBuffer()

    val readMap: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val writeMap: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val invRepsMap: collection.mutable.ArrayBuffer[(Identifier, Identifier)] = collection.mutable.ArrayBuffer()

    val cfDependencies: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val passDependencies: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()

    val variableFamilies : UnionFind[Identifier] = new UnionFind()
    val allMutableRelevantVars: collection.mutable.ArrayBuffer[Identifier] = collection.mutable.ArrayBuffer()

    val implicationChecker = new ImplicationChecker(spec)

    def processStreamDef(id: Identifier, exp: ExternExpression, args: Seq[ExpressionArg]) : Unit = {
      allMutableRelevantVars += id
      val dep = getAllDependencies(id, exp, args, implicationChecker)

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

    DefinitionOrdering.order(spec.definitions, Map()).foreach { case (id, defExpr) => {
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

    //No Double Write
    immutVars ++= writeMap.filter(_._2.size > 1).keys.flatMap(variableFamilies.equivalenceClass)

    //No Replicating Lasts
    def cleanParent(node: Identifier, beat: Seq[Identifier], caller: Identifier) : (Set[Identifier], Boolean) = {
      if (writeMap.getOrElse(node, Set()).size > (if (beat.isEmpty) 1 else 0) || (beat.nonEmpty && !repsMap.getOrElse(node, Set()).filter(_._1 == beat.head).exists(c => implicationChecker.freqImplication(beat.head, c._2)))) {
        (Set(), false)
      } else {
        val (childReads, childClean) = dealChilds(node, beat, caller)

        if (childClean) {
          val repParents = repsMap.flatMap{case (k, v) => if (v.exists(_._1 == node)) Some(k) else None}
          repParents.map(cleanParent(_, node +: beat, node)).foldLeft[(Set[Identifier], Boolean)]((childReads, childClean)){case ((s1, b1), (s2, b2)) => (s1.union(s2), b1 && b2)}
        } else {
          (Set(), false)
        }

      }
    }

    def cleanChild(node: Identifier, beat: Seq[Identifier], caller: Identifier) : (Set[Identifier], Boolean) = {
      val cr : (Set[Identifier], Boolean) = if (beat.nonEmpty) cleanChild(node, beat.drop(1), caller) else (Set(), false)
      if (cr._2) {
        cr
      } else if (beat.isEmpty || writeMap.getOrElse(node, Set()).nonEmpty || repsMap.getOrElse(caller, Set()).filter(_._1 == node).exists(c => !implicationChecker.freqImplication(beat.head, c._2))) {
        (Set(), false)
      } else {
        dealChilds(node, beat.drop(1), caller)
      }
    }

    def dealChilds(node: Identifier, beat: Seq[Identifier], caller: Identifier) : (Set[Identifier], Boolean) = {
      val childs : Set[Identifier] = repsMap.getOrElse(node, Set()).map(_._1) - caller
      if (childs.nonEmpty) {
        childs.map(c => cleanChild(c, beat, caller)).reduce[(Set[Identifier], Boolean)]{case ((s1, b1), (s2, b2)) => (s1.union(s2), b1 && b2)}
      } else {
        (readMap.getOrElse(node, Set()), true)
      }
    }

    val readBeforeWrites : collection.mutable.ArrayBuffer[(Identifier, Identifier, Identifier)] = collection.mutable.ArrayBuffer()
    writeMap.filter(_._2.size == 1).foreach{case (writtenNode, writers) =>
        writers.toSeq match {
          case Seq(writeNode) =>
            if (!immutVars.contains(writeNode)) {
              val (readDeps, clean) = cleanParent(writtenNode, Seq(), writeNode)
              if (!repsMap.getOrElse(writtenNode, Set()).map(_._1).contains(writeNode) && clean) {
                  readDeps.foreach{r =>
                    readBeforeWrites += ((r, writeNode, writtenNode))
                  }
              } else {
                immutVars ++= variableFamilies.equivalenceClass(writtenNode)
              }
            }
          case _ =>
        }
    }

    //No Read before Write
    val z3H = new Z3Handler(edges.toSet, readBeforeWrites.toSet, variableFamilies)
    immutVars ++= z3H.getImmutableVars

    val addDeps = readBeforeWrites.flatMap{case (from, to, mut) =>
      if (!immutVars.contains(mut)) Some(from -> to) else None
    }.groupBy(_._1).view.mapValues(e => e.map(x => x._2).toSet).toMap


//    println("========================")
//    println(nodes)
//    println("-READS-")
//    println(readMap)
//    println("-WRITES-")
//    println(writeMap)
//    println("-REPS-")
//    println(repsMap)
//    println("-EDGES-")
//    println(edges)
//    println("-FAMILIES-")
//    println(variableFamilies.toMap)
//    println("-IMMUT_VARS-")
//    println(immutVars)
//    println("-FREQ_IMP-")
//    println(implicationChecker.knownImplications)
//    println("-Read_Before_Write-")
//    println(readBeforeWrites)
//    println("-Z3-IMMUT-")
//    println(z3H.getImmutableVars)

    Success(new TesslaCoreWithMutabilityInfo(TesslaAST.Core.Specification(in, definitions, out, spec.maxIdentifier), allMutableRelevantVars.toSet -- immutVars, addDeps), Seq())
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

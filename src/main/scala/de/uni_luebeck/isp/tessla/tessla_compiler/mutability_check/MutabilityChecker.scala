package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.tessla_compiler.DefinitionOrdering
import de.uni_luebeck.isp.tessla.{TesslaAST, TranslationPhase}

class TesslaCoreWithMutabilityInfo(val spec: TesslaAST.Core.Specification, val idTypes: Identifier => Type,
                                   val addDeps: Map[Identifier, Set[Identifier]], val impCheck: ImplicationChecker) {
  override def toString = s"${spec}"
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

    val impCheck = new ImplicationChecker(spec)
    val expFlowAnalysis = new ExpressionFlowAnalysis(impCheck)

    def processDependencies(id: Identifier, dep: ExpressionFlowAnalysis.IdentifierDependencies) : Unit = {
      allMutableRelevantVars += id

      dep.calls.foreach(c => variableFamilies.union(c._1, c._2))

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

    def processDefinitions(defs: Seq[(Identifier, DefinitionExpression)], scope: Map[Identifier, DefinitionExpression]) : Unit = {

      def execOnFuncExpr(e : ExpressionArg) : Unit = {
        e match {
          case FunctionExpression(_, _, body, _, _) => processDefinitions(DefinitionOrdering.order(body, Map()), scope ++ body) //TODO: Make Result only Ref
          case ApplicationExpression(applicable, args, _) => (args :+ applicable).foreach(execOnFuncExpr)
          case TypeApplicationExpression(applicable, _, _) => execOnFuncExpr(applicable)
          case RecordConstructorExpression(entries, _) => entries.foreach(e => execOnFuncExpr(e._2._1))
          case RecordAccesorExpression(_, target, _, _) => execOnFuncExpr(target)
          case _ =>
        }
      }

      defs.foreach{case (id, d) =>
        execOnFuncExpr(d)
        processDependencies(id, expFlowAnalysis.getExpFlow(d, scope))
      }
    }

    processDefinitions(DefinitionOrdering.order(spec.definitions, Map()), spec.definitions)

    val repsMap : Map[Identifier, Set[(Identifier, Identifier)]] = invRepsMap.flatMap{case (k,v) =>
      passDependencies.getOrElse(k, Set(k)).map((_,(v,k)))
    }.groupBy(_._1).view.mapValues{vs => vs.map(_._2).toSet}.toMap

    //No Double Write
    immutVars ++= writeMap.filter(_._2.size > 1).keys.flatMap(variableFamilies.equivalenceClass)

    //No Replicating Lasts
    def cleanParent(node: Identifier, beat: Seq[Identifier], caller: Identifier) : (Set[Identifier], Boolean) = {
      if (writeMap.getOrElse(node, Set()).size > (if (beat.isEmpty) 1 else 0) || (beat.nonEmpty && !repsMap.getOrElse(node, Set()).filter(_._1 == beat.head).exists(c => impCheck.freqImplication(beat.head, c._2)))) {
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
      } else if (beat.isEmpty || writeMap.getOrElse(node, Set()).nonEmpty || repsMap.getOrElse(caller, Set()).filter(_._1 == node).exists(c => !impCheck.freqImplication(beat.head, c._2))) {
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
    if (readBeforeWrites.nonEmpty) {
      val z3H = new Z3Handler(edges.toSet, readBeforeWrites.toSet, variableFamilies)
      immutVars ++= z3H.getImmutableVars
    }

    val addDeps = readBeforeWrites.flatMap{case (from, to, mut) =>
      if (!immutVars.contains(mut)) Some(from -> to) else None
    }.groupBy(_._1).view.mapValues(e => e.map(x => x._2).toSet).toMap


    println("========================")
    println(nodes)
    println("-READS-")
    println(readMap)
    println("-WRITES-")
    println(writeMap)
    println("-REPS-")
    println(repsMap)
    println("-EDGES-")
    println(edges)
    println("-FAMILIES-")
    println(variableFamilies.toMap)
    println("-IMMUT_VARS-")
    println(immutVars)
    println("-FREQ_IMP-")
    println(impCheck.knownImplications)
    println("-Read_Before_Write-")
    println(readBeforeWrites)
    println("-Z3-IMMUT-")
    println(immutVars)

    def targetVarType(id: Identifier) : Type = {
      val origType = if (spec.in.contains(id)) {
        spec.in(id)._1
      } else {
        spec.definitions(id).tpe
      }
      if (origType.isInstanceOf[FunctionType]) {
        definitions(id) match {
          case FunctionExpression(typeParams, params, _, result, _) =>
            FunctionType(typeParams, params.map{case (id, ev, _) => (ev, targetVarType(id))}, targetVarType(result.asInstanceOf[ExpressionRef].id))
          case ExternExpression(typeParams, params, resultType, name, location) => ???
          case ApplicationExpression(applicable, args, location) => ???
          case RecordAccesorExpression(name, target, nameLocation, location) => ???
        }
      }
      else if ((mutabilityCheckRelevantType(origType) || mutabilityCheckRelevantStreamType(origType)) && !immutVars.contains(id)) {
        mkTypeMutable(origType)
      } else {
        origType
      }
    }


    Success(new TesslaCoreWithMutabilityInfo(TesslaAST.Core.Specification(in, definitions, out, spec.maxIdentifier),
            targetVarType, addDeps, impCheck), Seq())
  }

  def mutabilityCheckRelevantStreamType(tpe: Type) : Boolean = {
    tpe match {
      case InstatiatedType("Events", Seq(t), _) => mutabilityCheckRelevantType(t)
    }
  }

  def mutabilityCheckRelevantType(tpe: Type) : Boolean = {
    //TODO: TypeArg
    //TODO: Option[Set] ...
    tpe match {
      case InstantiatedType("Map", _, _) |
           InstantiatedType("Set", _, _) |
           InstantiatedType("List", _, _) => true
      case RecordType(entries, _) => entries.values.map{case (t,_) => mutabilityCheckRelevantType(t)}.reduce(_ || _)
      case _ => false
    }
  }

  def mkTypeMutable(t: Type): Type = {
    t match {
      case InstatiatedType("Events", List(t), l) => InstatiatedType("Events", List(mkTypeMutable(t)), l)
      case InstatiatedType("Option", List(t), l) => InstatiatedType("Option", List(mkTypeMutable(t)), l)
      case InstatiatedType("Set", t, l) => InstatiatedType("MutSet", t, l)
      case InstatiatedType("Map", t, l) => InstatiatedType("MutMap", t, l)
      case InstatiatedType("List", t, l) => InstatiatedType("MutList", t, l)
      case RecordType(entries, _) => RecordType(entries.map{case (n,(t,l)) => (n, (mkTypeMutable(t), l))})
      case _ : TypeParam => t
      //TODO: Add Error
    }
  }

}

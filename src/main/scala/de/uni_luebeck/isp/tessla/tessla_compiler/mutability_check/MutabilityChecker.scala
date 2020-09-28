package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.tessla_compiler.DefinitionOrdering
import de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing.ControlFlowAnalysis
import de.uni_luebeck.isp.tessla.{TesslaAST, TranslationPhase}

class TesslaCoreWithMutabilityInfo(val spec: TesslaAST.Core.Specification,
                                   val idTypes: (Identifier, Map[Identifier, DefinitionExpression]) => Type,
                                   val addDeps: Map[Identifier, Set[Identifier]], val impCheck: ImplicationChecker) {
  override def toString = s"${spec}"
}

object MutabilityChecker extends
  TranslationPhase[TesslaAST.Core.Specification, TesslaCoreWithMutabilityInfo] {

  override def translate(spec: TesslaAST.Core.Specification): TranslationPhase.Result[TesslaCoreWithMutabilityInfo] = {

    val in = spec.in
    val definitions = spec.definitions
    val out = spec.out

    //TODO: Only create once
    val cfAnalysis = new ControlFlowAnalysis(spec)

    val edges: collection.mutable.HashSet[(Identifier, Identifier)] = collection.mutable.HashSet()
    val transEdges: collection.mutable.HashMap[Identifier, Set[Identifier]] = collection.mutable.HashMap()
    def addEdge(i: Identifier, j: Identifier): Unit = {
      edges += (i -> j)
      transEdges += (i -> (transEdges.getOrElse(i, Set()) + j))
      transEdges.mapValuesInPlace{case (_, s) => if (s.contains(i)) s + j else s}
    }

    //TODO: Get rid of some maps
    val nodes: collection.mutable.HashSet[Identifier] = collection.mutable.HashSet()
    val immutVars: collection.mutable.HashSet[Identifier] = collection.mutable.HashSet()

    val readMap: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val writeMap: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val invRepsMap: collection.mutable.HashSet[(Identifier, Identifier)] = collection.mutable.HashSet()
    val inlinings: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()

    val cfDependencies: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()
    val passDependencies: collection.mutable.Map[Identifier, Set[Identifier]] = collection.mutable.Map()

    val variableFamilies : UnionFind[Identifier] = new UnionFind()
    val allMutableRelevantVars: collection.mutable.HashSet[Identifier] = collection.mutable.HashSet()

    //TODO: Find better solution
    val paramTypes: collection.mutable.Map[Identifier, Type] = collection.mutable.Map()

    val impCheck = new ImplicationChecker(spec)
    val expFlowAnalysis = new ExpressionFlowAnalysis(impCheck)

    def processDependencies(id: Identifier, dep: ExpressionFlowAnalysis.IdentifierDependencies) : Unit = {

      allMutableRelevantVars += id

      dep.calls.foreach(c => variableFamilies.union(c._1, c._2))

      dep.immut.foreach(id => immutVars += id) //TODO: Filter for mutablecheck relevant types

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
        }

        dep.deps.foreach {
          i => if (nodes.contains(i)) {
            addEdge(i, id)
          } else {
            cfDependencies.getOrElse(i, Set()).foreach{i => addEdge(i, id)}
          }
        }

      } else {
        cfDependencies += (id -> dep.deps.map{d => if (cfDependencies.contains(d)) cfDependencies(d) else Set(d)}.foldLeft[Set[Identifier]](Set()){case (c,s) => c ++ s})
        if (dep.pass.nonEmpty) {
          passDependencies += (id -> dep.pass.map { d => if (passDependencies.contains(d)) passDependencies(d) else Set(d) }.foldLeft[Set[Identifier]](Set()) { case (c, s) => c ++ s })
        }
        cfDependencies(id).foreach(i => if (!nodes.contains(i)) nodes += i)
      }

      dep.pass.foreach{i => variableFamilies.union(id, i)}
    }

    //TODO: Only a dirty hack, find better holistic way to deal with if's lazyness
    //TODO: Care that function types are not inlined and ordered correctly
    //TODO: Is name really a good way to recognize that a variable already existed?
    def getInlinedIdentifiers(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): Set[Identifier] = {
       val args : Set[ExpressionArg]= e match {
            case RecordAccessorExpression(_, target, _, _) => Set(target)
            case ApplicationExpression(applicable, args, _) => args.toSet + applicable
            case TypeApplicationExpression(applicable, _, _) => Set(applicable)
            case RecordConstructorExpression(entries, _) => entries.map(_._2._1).toSet
            case ExpressionRef(id, tpe, _) if id.idOrName.left.isEmpty && !tpe.isInstanceOf[FunctionType] =>
              if (scope.contains(id)) Set(scope(id)) else Set()
            case _ => Set()
       }
       val inl : Set[Identifier] = e match {
         case ExpressionRef(id, tpe, _) if cfAnalysis.varSuitableForInlining(id) && !tpe.isInstanceOf[FunctionType] =>
           if (scope.contains(id)) Set(id) else Set()
         case _ => Set()
       }

      inl ++ args.foldLeft[Set[Identifier]](Set()){case (c, e) => c ++ getInlinedIdentifiers(e, scope)}
    }

    def getIDsIfInlineExpression(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): Set[Identifier] = {
      e match {
        case ApplicationExpression(TypeApplicationExpression(e, _, _), args, _) => {
          e match {
            case ExternExpression(_, _, _, op, _) if Set("ite", "staticite", "and", "or").contains(op) =>
              args.drop(1).map(getInlinedIdentifiers(_, scope)).reduce(_ ++ _)
            case _ =>
              Set()
          }
        }
        case _ => Set()
      }
    }

    def processDefinitions(defs: Seq[(Identifier, DefinitionExpression)], scope: Map[Identifier, DefinitionExpression]) : Unit = {

      def execOnFuncExpr(e : ExpressionArg) : Unit = {
        e match {
          case FunctionExpression(_, pars, body, _, _) =>
            pars.foreach{case (id, _, typ) => paramTypes += (id -> typ)}
            processDefinitions(DefinitionOrdering.order(body, Map()), scope ++ body)
          case ApplicationExpression(applicable, args, _) => (args :+ applicable).foreach(execOnFuncExpr)
          case TypeApplicationExpression(applicable, _, _) => execOnFuncExpr(applicable)
          case RecordConstructorExpression(entries, _) => entries.foreach(e => execOnFuncExpr(e._2._1))
          case RecordAccessorExpression(_, target, _, _) => execOnFuncExpr(target)
          case _ =>
        }
      }

      defs.foreach{case (id, d) =>
        execOnFuncExpr(d)
        processDependencies(id, expFlowAnalysis.getExpFlow(id, d, scope, Map()))
        getIDsIfInlineExpression(d, scope).foreach(i => inlinings += (i -> inlinings.getOrElse(i, Set() + id)))
      }
    }

    processDefinitions(DefinitionOrdering.order(spec.definitions, Map()), spec.definitions)

    val repsMap : Map[Identifier, Set[(Identifier, Identifier)]] = invRepsMap.flatMap{case (k,v) =>
      passDependencies.getOrElse(k, Set(k)).map((_,(v,k)))
    }.groupBy(_._1).view.mapValues{vs => vs.map(_._2).toSet}.toMap

    //No IDs belonging to already immutable vars
    immutVars ++= immutVars.flatMap(variableFamilies.equivalenceClass)


    def cleanParent(node: Identifier, beat: Seq[Identifier], caller: Identifier, origWrite: Identifier) : (Set[Identifier], Boolean) = {

      //println(s"cleanParent: $node")
      val writes = if (beat.isEmpty) writeMap.getOrElse(node, Set()).filter(!incompatibleVars(_, caller)) else writeMap.getOrElse(node, Set())

      if (writes.size > (if (beat.isEmpty) 1 else 0) ||
          (beat.nonEmpty && !repsMap.getOrElse(node, Set()).filter(_._1 == beat.head).exists(c => impCheck.freqImplication(beat.head, c._2, false)))) {
        //println(s"cleanParent: $node : false")
        (Set(), false)
      } else {
        val (childReads, childClean) = dealChilds(node, beat, caller, origWrite)

        //println(s"cleanParent: $node --> childR/C $childReads $childClean")

        if (childClean) {
          val repParents = repsMap.flatMap{case (k, v) => if (v.exists(_._1 == node)) Some(k) else None}
          val ret = repParents.map(cleanParent(_, node +: beat, node, origWrite)).foldLeft[(Set[Identifier], Boolean)]((childReads, childClean)) { case ((s1, b1), (s2, b2)) => (s1.union(s2), b1 && b2) }
          //println(s"cleanParent: $node : $ret")
          ret
        } else {
          //println(s"cleanParent: $node : false2")
          (Set(), false)
        }

      }
    }

    def cleanChild(node: Identifier, beat: Seq[Identifier], caller: Identifier, origWrite: Identifier) : (Set[Identifier], Boolean) = {

      //println(s"cleanChild: $node $origWrite")

      val cr : (Set[Identifier], Boolean) = if (beat.nonEmpty) cleanChild(node, beat.drop(1), caller, origWrite) else (Set(), false)
      val pr : (Set[Identifier], Boolean) = if (beat.isEmpty || writeMap.getOrElse(node, Set()).nonEmpty ||
                 repsMap.getOrElse(caller, Set()).filter(_._1 == node).exists(c => !impCheck.freqImplication(beat.head, c._2, false))) {
        (Set(), false)
      } else {
        dealChilds(node, beat.drop(1), caller, origWrite)
      }


      if (pr._2) {
        //println(s"cleanChild: $node : $pr")
        pr
      } else {
        //println(s"cleanChild: $node : $cr")
        cr
      }
    }

    def dealChilds(node: Identifier, beat: Seq[Identifier], caller: Identifier, origWrite: Identifier) : (Set[Identifier], Boolean) = {
      val childs : Set[Identifier] = if (beat.isEmpty) {
        repsMap.getOrElse(node, Set()).map(_._1).filter(!incompatibleVars(_, origWrite))
      } else {
        repsMap.getOrElse(node, Set()).map(_._1)
      } - caller

      if (childs.nonEmpty) {
        childs.map(c => cleanChild(c, beat, caller, origWrite)).reduce[(Set[Identifier], Boolean)]{case ((s1, b1), (s2, b2)) => (s1.union(s2), b1 && b2)}
      } else if (beat.isEmpty) {
        (readMap.getOrElse(node, Set()).filter(!incompatibleVars(_, origWrite)), true)
      } else {
        (Set(), true)
      }
    }

    def incompatibleVars(id1: Identifier, id2: Identifier) : Boolean = {
      val c1 = cfAnalysis.getAddConditions(id1)
      val c2 = cfAnalysis.getAddConditions(id2)

      c1._1.intersect(c2._2).nonEmpty || c1._2.intersect(c2._1).nonEmpty
    }

    //No double writes, replicating lasts  and collect Read-Before-Writes
    val readBeforeWrites : collection.mutable.ArrayBuffer[(Identifier, Identifier, Identifier)] = collection.mutable.ArrayBuffer()
    writeMap.foreach { case (writtenNode, writeNodes) =>
      val writeNode = writeNodes.toSeq.head
      if (!immutVars.contains(writeNode)) {
        val (readDeps, clean) = cleanParent(writtenNode, Seq(), writeNode, writeNode)
        if (!repsMap.getOrElse(writtenNode, Set()).map(_._1).contains(writeNode) && clean) {
          readDeps.foreach { r =>
            readBeforeWrites += ((r, writeNode, writtenNode))
          }
        } else {
          immutVars ++= variableFamilies.equivalenceClass(writtenNode)
        }
      }
    }

    //No Read before Write
      readBeforeWrites.flatMapInPlace{case (i, j, m) =>
      if (transEdges.getOrElse(i, Set()).contains(j)) {
        None
      } else if (transEdges.getOrElse(j, Set()).contains(i)) {
        immutVars ++= variableFamilies.equivalenceClass(m)
        None
      } else {
        Some((i, j, m))
      }
    }

    if (readBeforeWrites.nonEmpty) {
      val z3H = new Z3Handler(edges.toSet ++ cfAnalysis.getAddOrderingConstraints, readBeforeWrites.toSet, inlinings.toMap, variableFamilies)
      immutVars ++= z3H.getImmutableVars
    }

    val addDeps = readBeforeWrites.flatMap{case (from, to, mut) =>
      if (!immutVars.contains(mut)) inlinings.getOrElse(from, Set()).map(to -> _) + (to -> from) else None
    }.groupBy(_._1).view.mapValues(e => e.map(x => x._2).toSet).toMap


    /*
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
    println("-TRANS_EDGES-")
    println(transEdges)
    println("-FAMILIES-")
    println(variableFamilies.toMap)
    println("-INLININGS-")
    println(inlinings)
    println("-FREQ_IMP-")
    println(impCheck.knownImplications)
    println("-Read_Before_Write-")
    println(readBeforeWrites)
    println("-Z3-IMMUT-")
    println(immutVars)
    */


    def targetVarType(id: Identifier, scope: Map[Identifier, DefinitionExpression]) : Type = {
      val origType = if (spec.in.contains(id)) {
        spec.in(id)._1
      } else if (scope.contains(id)){
        scope(id).tpe
      } else {
        paramTypes(id)
      }
      if (origType.isInstanceOf[FunctionType]) {
        if (scope.contains(id)) {
          scope(id) match {
            case FunctionExpression(typeParams, params, body, result, _) =>
              FunctionType(typeParams, params.map { case (id, ev, _) => (ev, targetVarType(id, scope)) }, targetVarType(result.asInstanceOf[ExpressionRef].id, scope ++ body))

            //TODO: Extern Expression: Preprocess, wrap in lambda -> Error
            case _: ExternExpression => ???
            case _ => definitions(id).tpe
          }
        } else {
          //Id is param ---> immutable
          origType
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
      case InstantiatedType("Events", Seq(t), _) => mutabilityCheckRelevantType(t)
      case _ => false
    }
  }

  @scala.annotation.tailrec
  def mutabilityCheckRelevantType(tpe: Type) : Boolean = {
    //TODO: TypeArg
    //TODO: Option[Set] ...
    //TODO: Make Record types also mutable under circumstances --> Problem: Inlining
    tpe match {
      case InstantiatedType("Map", _, _) |
           InstantiatedType("Set", _, _) |
           InstantiatedType("List", _, _) |
           InstantiatedType("Queue", _, _) => true
      case InstantiatedType("Option", Seq(t), _) => mutabilityCheckRelevantType(t)
      case _ => false
    }
  }

  def mkTypeMutable(t: Type): Type = {
    t match {
      case InstantiatedType("Events", List(t), l) => InstantiatedType("Events", List(mkTypeMutable(t)), l)
      case InstantiatedType("Option", List(t), l) => InstantiatedType("Option", List(mkTypeMutable(t)), l)
      case InstantiatedType("Set", t, l) => InstantiatedType("MutSet", t, l)
      case InstantiatedType("Map", t, l) => InstantiatedType("MutMap", t, l)
      case InstantiatedType("Queue", t, l) => InstantiatedType("MutQueue", t, l)
      case InstantiatedType("List", t, l) => InstantiatedType("MutList", t, l)
      case _ : TypeParam => t
      //TODO: Add Error
    }
  }

}

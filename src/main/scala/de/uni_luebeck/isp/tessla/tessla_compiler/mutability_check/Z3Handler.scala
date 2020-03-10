package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import com.microsoft.z3._
import de.uni_luebeck.isp.tessla.tessla_compiler.Errors

class Z3Handler(edges: Set[(Identifier, Identifier)], readBeforeWriteConstraints: Set[(Identifier, Identifier, Identifier)], families: UnionFind[Identifier]) {

  val mutStringIDMap : collection.mutable.Map[Identifier, String] = collection.mutable.Map()
  val posStringIDMap : collection.mutable.Map[Identifier, String] = collection.mutable.Map()

  def getOrCreateMutVar(id: Identifier): String = {
    val idFam = families.find(id)
    getOrCreate(idFam, mutStringIDMap, "Mut")
  }

  def getOrCreatePosVar(id: Identifier): String = getOrCreate(id, posStringIDMap, "")

  def getOrCreate(id: Identifier, map: collection.mutable.Map[Identifier, String], add: String): String = {
    if (!map.contains(id)) {
      map += (id -> s"${id.fullName}$add")
    }
    map(id)
  }

  val ctx: Context = new Context()
  val optimizer: Optimize = ctx.mkOptimize()

 edges.foreach{case (f,t) =>
     optimizer.Assert(ctx.mkLt(ctx.mkIntConst(f.fullName), ctx.mkIntConst(t.fullName)))
 }

 readBeforeWriteConstraints.foreach{case (from, to, mutVar) =>
      optimizer.Assert(ctx.mkImplies(ctx.mkBoolConst(getOrCreateMutVar(mutVar)), ctx.mkLt(ctx.mkIntConst(from.fullName), ctx.mkIntConst(to.fullName))))
 }

  mutStringIDMap.values.foreach{mutVar =>
    optimizer.AssertSoft(ctx.mkBoolConst(mutVar), 1, "defGroup") //TODO: Use weights
  }

  val status: Status = optimizer.Check()
  if (status != Status.SATISFIABLE) {
    throw Errors.OptimizationError("Z3 formula is unsatisfiable or satisfiability is unknown") //TODO: If unclear make everything immutable
  }

  def getImmutableVars: Set[Identifier] = {
    val mdl = optimizer.getModel
    mutStringIDMap.flatMap{case (mutVarID, mutVar) =>
      if (mdl.getConstInterp(ctx.mkBoolConst(mutVar)).isFalse) families.equivalenceClass(mutVarID) else Set()
    }.toSet
  }

  def getOrder: Seq[Identifier] = { //FIXME: Dead code
    val mdl = optimizer.getModel
    posStringIDMap.toSeq.sortBy{case (_, varName) => mdl.getConstInterp(ctx.mkIntConst(varName)).getNumArgs}.map(_._1)
  }

}

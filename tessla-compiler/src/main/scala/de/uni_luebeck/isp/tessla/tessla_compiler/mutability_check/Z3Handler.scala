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

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import com.microsoft.z3._
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics

class Z3Handler(
  edges: Set[(Identifier, Identifier)],
  readBeforeWriteConstraints: Set[(Identifier, Identifier, Identifier)],
  inlinings: Map[Identifier, Set[Identifier]],
  families: UnionFind[Identifier]
) {

  val mutStringIDMap: collection.mutable.Map[Identifier, String] = collection.mutable.Map()
  val posStringIDMap: collection.mutable.Map[Identifier, String] = collection.mutable.Map()

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

  edges.foreach {
    case (f, t) =>
      optimizer.Assert(ctx.mkLe(ctx.mkIntConst(f.fullName), ctx.mkIntConst(t.fullName)))
  }

  readBeforeWriteConstraints.foreach {
    case (from, to, mutVar) =>
      optimizer.Assert(
        ctx.mkImplies(
          ctx.mkBoolConst(getOrCreateMutVar(mutVar)),
          ctx.mkLt(ctx.mkIntConst(from.fullName), ctx.mkIntConst(to.fullName))
        )
      )
  }

  inlinings.foreach {
    case (a, bs) => bs.foreach(b => optimizer.Assert(ctx.mkEq(ctx.mkIntConst(a.fullName), ctx.mkIntConst(b.fullName))))
  }

  mutStringIDMap.values.foreach { mutVar =>
    optimizer.AssertSoft(ctx.mkBoolConst(mutVar), 1, "defGroup") //TODO: Use weights
  }

  val status: Status = optimizer.Check()
  if (status != Status.SATISFIABLE) {
    throw Diagnostics.OptimizationError(
      s"Z3 formula is unsatisfiable or satisfiability is unknown; Status: $status"
    ) //TODO: If unclear make everything immutable
  }

  def getImmutableVars: Set[Identifier] = {
    val mdl = optimizer.getModel
    mutStringIDMap.flatMap {
      case (mutVarID, mutVar) =>
        if (mdl.getConstInterp(ctx.mkBoolConst(mutVar)).isFalse) families.equivalenceClass(mutVarID) else Set()
    }.toSet
  }

  /*
  def getOrder: Seq[Identifier] = {
    val mdl = optimizer.getModel
    posStringIDMap.toSeq.sortBy{case (_, varName) => mdl.getConstInterp(ctx.mkIntConst(varName)).getNumArgs}.map(_._1)
  }
   */

}

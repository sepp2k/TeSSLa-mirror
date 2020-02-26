package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.Tessla.Identifier
import de.uni_luebeck.isp.tessla.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.{TesslaAST, TranslationPhase}

class TesslaCoreWithMutabilityInfo(val spec: TesslaAST.Core.Specification, val mutableStreams: Set[Identifier]) {
  override def toString = s"${spec}\nMutable streams:${mutableStreams.mkString(", ")}"

}

//trait Edge;
//final class PassingEdge(val from: StreamRef, val writeBehavior: Boolean) extends Edge
//final class ConsumingEdge(val from: StreamRef, val readBehavior: Boolean) extends Edge
//final class ReplicatingEdge(val from: StreamRef, val trigger: Seq[StreamRef]) extends Edge

object MutabilityChecker extends
  TranslationPhase[TesslaAST.Core.Specification, TesslaCoreWithMutabilityInfo] {

  override def translate(spec: TesslaAST.Core.Specification): TranslationPhase.Result[TesslaCoreWithMutabilityInfo] = {

    val in = spec.in
    val definitions = spec.definitions
    val out = spec.out
    val mutableStreams : Set[Identifier] = Set()


//    def buildMapsDFS(sr: StreamRef) : (Map[Identifier, Identifier], Map[Identifier, Identifier], Map[Identifier, Identifier]) = {
//
//    }
//
//
//    val (writes, reads, replicates) = buildMapsDFS()

    Success(new TesslaCoreWithMutabilityInfo(TesslaAST.Core.Specification(in, definitions, out, spec.maxIdentifier), mutableStreams), Seq())

  }
}

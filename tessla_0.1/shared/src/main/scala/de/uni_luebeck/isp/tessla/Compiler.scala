package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST.Spec
import de.uni_luebeck.isp.tessla.modules.Module
import de.uni_luebeck.isp.tessla.modules.ModuleMapping
import scala.util.{Failure, Success, Try}
import Compiler._

object Compiler {
  abstract class State(val obj: Object)
  final case class Source(source: String) extends State(source)
  final case class Tree(tree: Spec) extends State(tree)
  final case class Graph(graph: ASTGraph) extends State(graph)
  final case class ModuleList(list: List[Module]) extends State(list)

  abstract class Provider

  case object UnexpectedCompilerState extends Exception
  
  trait Pass {
    def applyPass(compiler: Compiler, state: State): Try[State]
    def passDescription: String = this.getClass.getSimpleName.dropRight(1)
  }
  
  val defaultPasses = Seq(
    Parser,
    ResolveTypeNames,
    MacroResolution,
    ASTGraph,
    ResolveLocalRefs,
    TypeChecker,
    ModuleMapping)
  val defaultProviders: Set[Provider] = Functions.resolvers(32)
}

class Compiler(
  val passes: Seq[Pass] = defaultPasses,
  var providers: Set[Provider] = defaultProviders,
  val debug: Boolean = false) {
  
  def compile(state: State) = {
    Try {
      var currentState = state
      for (pass <- passes) {
        if (debug) {
          println(currentState.obj)
          println("=== " + pass.passDescription + " ===")
        }
        currentState = pass.applyPass(this, currentState) match {
          case Success(x) => x
          case Failure(x) =>
            if (debug) {
              println("ERROR:" )
              x.printStackTrace(System.out)
            }
            throw x
        }
      }
      if (debug) {
        println(currentState.obj)
      }
      currentState
    }
  }
  
}
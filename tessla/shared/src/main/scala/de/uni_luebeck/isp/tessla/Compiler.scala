package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST.Spec
import scala.util.{Failure, Success, Try}
import Compiler._

object Compiler {
  abstract class State;
  final case class Source(source: String) extends State;
  final case class Tree(tree: Spec) extends State;
  final case class Graph(graph: ASTGraph) extends State
  
  case object UnexpectedCompilerState extends Exception
  
  trait Pass {
    def applyPass(compiler: Compiler, state: State): Try[State];
    def passDescription: String = this.getClass.getSimpleName.dropRight(1)
  }
  
  val defaultPasses = Seq(Parser, MacroResolution, ASTGraph, ResolveLocalRefs)
}

class Compiler(val passes: Seq[Pass] = defaultPasses, val debug: Boolean = false) {
  
  def compile(state: State) = {
    Try {
      var currentState = state
      for (pass <- passes) {
        if (debug) {
          println(currentState)
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
        println(currentState)
      }
      currentState
    }
  }
  
}
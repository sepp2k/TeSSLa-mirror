package de.uni_luebeck.isp.tessla

class Compiler(val debug: Boolean = false, val silent: Boolean = false) {
  case class StateWrapper[T](state: T) {
    def apply[U](pass: CompilerPass[T, U]): StateWrapper[U] = {
      if (debug) {
        println(s"=== ${pass.passDescription} ===")
      }
      val result = pass(Compiler.this, state)
      if (debug) {
        result match {
          case t: WithDebugOutput => println(t.debugOutput)
          case _ => println(result)
        }
      }
      StateWrapper(result)
    }
  }

  def applyPasses(src: TesslaSource): Option[TesslaCore.Specification] = {
    try {
      val result =
        (
          StateWrapper(src)
          (Parser)
          (AstToCore)
        ).state

      Some(result)
    } catch {
      case error: CompilationError =>
        // TODO: More flexible error handling that allows for display methods other than printing to stdout
        println(s"Error: $error")
        None
    }
  }
}

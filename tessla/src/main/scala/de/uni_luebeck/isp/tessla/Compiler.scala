package de.uni_luebeck.isp.tessla

class Compiler(val debug: Boolean = false, val silent: Boolean = false) {
  var diagnostics: Seq[Diagnostic] = Seq()

  var encounteredFatal: Boolean = false

  def diagnostic(diagnostic: Diagnostic) {
    diagnostic match {
      case _: GiveUp =>
        encounteredFatal = true
        return
      case _: Fatal =>
        encounteredFatal = true
      case _ =>
    }
    if (debug) {
      diagnostic.printStackTrace()
    } else if (!silent) {
      // TODO generate nice diagnostic messages
      println(diagnostic)
    }
    diagnostics :+= diagnostic
  }

  case class StateWrapper[T](state: T) {
    def apply[U](pass: CompilerPass[T, U]): StateWrapper[U] = {
      if (debug) {
        println(s"=== ${pass.passDescription} ===")
      }
      val result = pass(Compiler.this, state).get
      if (debug) {
        result match {
          case t:WithDebugOutput => println(t.debugOutput)
          case _ => println(result)
        }
      }
      if (encounteredFatal) {
        throw GiveUp()
      }
      StateWrapper(result)
    }
  }

  // TODO change return type
  def applyPasses(src: TesslaSource): Option[AnyRef] = {
    try {
      val result = (StateWrapper(src)
        (Parser)
        (DefExtractor)
        (MacroResolver)
        (TypeChecker)
        (AscriptionRemover)
        (SaltConverter)
        (ConstantFolder)
        (ModuleMapper)).state

      Some(result)
    } catch {
      case x: Diagnostic =>
        Compiler.this.diagnostic(x)
        None
    }
  }

  def lookupFunction(name: String): Set[Function] = {
    Function.defaultFunctions.filter(fn => fn.name == name).toSet
  }

}

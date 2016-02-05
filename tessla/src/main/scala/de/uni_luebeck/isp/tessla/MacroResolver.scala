package de.uni_luebeck.isp.tessla

import scala.util.{Try, Success}

object MacroResolver extends CompilerPass[Definitions, Definitions] {
  override def apply(compiler: Compiler, defs: Definitions) = Try {
    assert(defs.macroDefs.isEmpty, "macro substitution not implement yet")
    defs
  }
}

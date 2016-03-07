package de.uni_luebeck.isp.tessla

object TypeChecker extends CompilerPass[Definitions, FunctionGraph] {
  override def apply(compiler: Compiler, defs: Definitions) = {
    assert(defs.macroDefs.isEmpty, "macros still present during typecheck")

    val checker = new TypeChecker(compiler, defs)

    ???
  }


}

class TypeChecker(compiler: Compiler, defs: Definitions) {
  /*
  Plan:

  implement type checking without overloading in persistent.
  Use 1-Saturation to resolve overloading.

  implement coercion by adding overloaded coerce functions "everywhere"

  whenever no progress is made, force innermost coerce functions to be id.
  (for a suitable definition of innermost that makes this deterministic)

  if no progress is made and no coerce function is left error out
  (type annotation needed)

  */
}
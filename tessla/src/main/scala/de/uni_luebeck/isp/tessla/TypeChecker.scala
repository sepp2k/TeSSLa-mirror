package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object TypeChecker extends CompilerPass[Definitions, FunctionGraph] {
  override def apply(compiler: Compiler, defs: Definitions) = {
    assert(defs.macroDefs.isEmpty, "macros still present during typecheck")

    val checker = new TypeChecker(compiler, defs)

    ???
  }

  case class Node(result: TypeVar, fn: ExprTreeFn, args: Map[ArgName, TypeVar], subtree: ExprTree)

}

class TypeChecker(compiler: Compiler, defs: Definitions) {
  import TypeChecker._
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

  val sdefs = defs.streamDefs

  val defTypes: mutable.Map[String, Option[TypeVar]] = mutable.Map()
  val nodes: mutable.Map[TypeVar, Node] = mutable.Map()


  def handleName(fn: NamedFn, expr: ExprTree): TypeVar = {
    if (sdefs contains fn.name) {
      handleStreamDef(sdefs(fn.name))
    } else {
      val v = new TypeVar
      nodes(v) = Node(v, fn, Map(), expr)
      v
    }
  }

  def handleStreamDef(sdef: StreamDef): TypeVar = {
    if (defTypes contains sdef.name) {
      // TODO This fails on cycles, generate diagnostics
      defTypes(sdef.name).get
    } else {
      defTypes(sdef.name) = None
      val v = handleSubtree(sdef.expr)
      defTypes(sdef.name) = Some(v)
      v
    }
  }

  def handleSubtree(expr: ExprTree): TypeVar = {
    if (expr.args.isEmpty && expr.fn.isInstanceOf[NamedFn]) {
      handleName(expr.fn.asInstanceOf[NamedFn], expr)
    } else {
      val v = new TypeVar
      nodes(v) = Node(v, expr.fn, expr.args.mapValues(handleSubtree), expr)
      v
    }
  }

  for (sdef <- sdefs.values) {
    handleStreamDef(sdef)
  }
}
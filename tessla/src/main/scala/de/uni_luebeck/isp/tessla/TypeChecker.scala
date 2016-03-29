package de.uni_luebeck.isp.tessla

import scala.collection.mutable

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

  case class Node(result: TypeVar, fn: ExprTreeFn, args: Map[ArgName, TypeVar], subtree: ExprTree)

  case class State(env: Env, overloads: Map[TypeVar, Set[Function]]) {
    // TODO also return why an overload was discarded
    def applyOverload(key: TypeVar, overload: Function): Option[State] = {
      val sig = overload.signature
      val node = nodes(key)

      val nameToIndex = mutable.Map[ArgName, Int]()

      for (((name, _), i) <- sig.args.zipWithIndex) {
        nameToIndex(Pos(i)) = i
        name match {
          case Some(n) => nameToIndex(Named(n)) = i
          case _ => ()
        }
      }

      val nodeArgs = mutable.Map[Int, TypeVar]()

      for ((key, value) <- node.args) {
        if (!nameToIndex.contains(key)) {
          return None
        }
        val index = nameToIndex(key)
        if (nodeArgs.contains(index)) {
          return None
        }
        nodeArgs(index) = value
      }

      val sigTypes = Seq(sig.ret) ++ sig.args.map(_._2)

      val nodeTypes = Seq(node.result) ++ sig.args.indices.map(i => {
        if (!nodeArgs.contains(i)) {
          return None
        }
        nodeArgs(i)
      })

      // The deep copy below is needed, as multiple invocations of the same
      // function do not necessarily share the type variables

      val sigType = GenericType("Function", sigTypes).deepCopy
      val nodeType = GenericType("Function", nodeTypes)

      val unifiedEnv = env.unify(sigType, nodeType) match {
        case None => return None
        case Some(e) => e
      }
      Some(State(env, overloads.updated(key, Set(overload))))
    }
  }


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

  var state: State = State(Env(Map()),
    nodes.values.map(node => node.result -> (node.fn match {
      case TypeAscrFn(t, _) => Set(TypeAscription(t): Function)
      case NamedFn(name, _) => compiler.lookupFunction(name)
    })).toMap)
}

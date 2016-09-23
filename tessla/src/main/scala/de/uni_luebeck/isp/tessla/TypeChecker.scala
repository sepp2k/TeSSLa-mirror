package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TypeChecker.{TypeMatchError, UnknownFunctionError}

import scala.collection.{Set, mutable}
import scala.util.{Try, Success}

object TypeChecker extends CompilerPass[Definitions, FunctionGraph] {

  case class UnknownFunctionError(name: String, atLocation: NestedLoc) extends Fatal
  //Preliminary TypeMatchError:
  case class TypeMatchError(conflicts: Set[Function], atLocation: NestedLoc) extends Fatal

  override def apply(compiler: Compiler, defs: Definitions) = Try {
    assert(defs.macroDefs.isEmpty, "macros still present during type check")

    val checker = new TypeChecker(compiler, defs)

    checker.typecheck

    checker.functionGraph
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

  case class Node(result: TypeVar, fn: ExprTreeFn, args: Map[ArgName, TypeVar], subtree: ExprTree) {
    def applySignature(sig: FunctionSig): Option[(Seq[Type], Seq[TypeVar])] = {
      val nameToIndex = mutable.Map[ArgName, Int]()

      for (((name, _), i) <- sig.args.zipWithIndex) {
        nameToIndex(Pos(i)) = i
        name match {
          case Some(n) => nameToIndex(Named(n)) = i
          case _ => ()
        }
      }
      val nodeArgs = mutable.Map[Int, TypeVar]()
      for ((key, value) <- args) {
        if (!nameToIndex.contains(key)) {
          // argument name does not exist in signature
          return None
        }
        val index = nameToIndex(key)
        if (nodeArgs.contains(index)) {
          // multiple arguments provided for same formal parameter in signature
          return None
        }
        nodeArgs(index) = value
      }

      val sigTypes = Seq(sig.ret) ++ sig.args.map(_._2)

      val argTypes = sig.args.indices.map(i => {
        if (!nodeArgs.contains(i)) {
          return None
        }
        nodeArgs(i)
      })

      val nodeTypes = Seq(result) ++ argTypes

      Some((sigTypes, nodeTypes))
    }
  }

  case class State(env: Env, overloads: Map[TypeVar, Set[Function]]) {
    // TODO also return why an overload was discarded
    def applyOverload(key: TypeVar, overload: Function): Option[State] = {
      val sig = overload.signature
      val node = nodes(key)

      val (sigTypes, nodeTypes) = node.applySignature(sig) match {
        case None => return None
        case Some(v) => v
      }

      // The deep copy below is needed, as multiple invocations of the same
      // function do not necessarily share the type variables

      val sigType = GenericType("Function", sigTypes).deepCopy
      val nodeType = GenericType("Function", nodeTypes)

      val unifiedEnv = env.unify(sigType, nodeType) match {
        case None => return None
        case Some(e) => e
      }
      Some(State(unifiedEnv, overloads.updated(key, Set(overload))))
    }

    def resolveOverload(key: TypeVar): Option[State] = {

      val states = overloads(key).flatMap(applyOverload(key, _)).toSeq

      states match {
        case Seq() => {
          //Todo: Make failed unification attempt available.
          // The following code works in debug mode but not after normal compilation - fix this.
          /*val conflicts = overloads(key).flatMap{ol =>
            nodes(key).applySignature(ol.signature).map {
              case (sigT,nodeT) =>
                println(nodeT)
                (sigT,nodeT).map(env(_)))
            }
          }
         compiler.diagnostic(TypeMatchError(conflicts, nodes(key).subtree.loc))
         */
          //Preliminary diagnostic:
          compiler.diagnostic(TypeMatchError(overloads(key), nodes(key).subtree.loc))
          None
        }
        case Seq(first, rest @ _*) => Some(rest.fold(first)(_ join _))
      }
    }

    def join(other: State): State = {
      State(
        env = this.env.join(other.env),
        overloads = overloads.keys.map(k =>
          k -> (this.overloads(k) ++ other.overloads(k))).toMap
      )
    }

    // TODO Error reporting
    def resolve: Option[State] = {
      var state = this
      var activeKeys = overloads.keys.toSet
      var progress = 2

      while (progress > 0 && activeKeys.nonEmpty) {
        // TODO check for coercions when progress is 0
        progress -= 1
        for (key <- activeKeys) {
          state.resolveOverload(key) match {
            case Some(s) =>
              val oldOverloadCount = state.overloads(key).size
              val newOverloadCount = s.overloads(key).size
              if (newOverloadCount < oldOverloadCount) {
                progress = 2
              }
              if (newOverloadCount == 1) {
                activeKeys -= key
              }
              state = s
            case None =>
              return None
          }
        }
      }
      if (activeKeys.isEmpty) Some(state) else None
    }

    def debugPrint: Unit = {
      println(">>>> state")
      for (key <- overloads.keys) {
        println("  " + key + " = " + key.substitute(env))
        for (overload <- overloads(key)) {
          println("    " + overload)
        }
      }
      println("<<<<")
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
      nodes(v) = Node(v, expr.fn, expr.args.mapValues(handleSubtree).view.force, expr)
      v
    }
  }

  for (sdef <- sdefs.values) {
    handleStreamDef(sdef)
  }

  for(outStream <- defs.outStreams.values) {
    handleSubtree(outStream.toExprTree())
  }


  var state: State = State(Env(Map()),
    nodes.map({case (_, node) => node.result -> (node.fn match {
      case TypeAscrFn(t, _) => Set(TypeAscription(t): Function)
      case NamedFn(name, _) => {
        val functions = compiler.lookupFunction(name,node.args.size)
        if (functions.isEmpty) {
          compiler.diagnostic (UnknownFunctionError (name, node.subtree.loc) )
        }
        functions
      }
      case LiteralFn(IntLiteral(v), _) =>
        Set(ConstantValue(SimpleType("Int"), v): Function)
      case LiteralFn(StringLiteral(v), _) =>
        Set(ConstantValue(SimpleType("String"), v): Function)
      case LiteralFn(BoolLiteral(v), _) =>
        Set(ConstantValue(SimpleType("Boolean"), v): Function)
      case LiteralFn(FloatLiteral(v), _) =>
        Set(ConstantValue(SimpleType("Float"), v): Function)
    })}).toMap)

  def typecheck: Unit = {
    state = state.resolve.get
  }

  def functionGraph: FunctionGraph = {
    val graph = new FunctionGraph
    val graphNodes = mutable.Map[TypeVar, graph.NodeId]()

    def process(typeVar: TypeVar): graph.NodeId = {
      graphNodes.get(typeVar) match {
        case Some(node) => node
        case None =>
          val Seq(overload) = state.overloads(typeVar).toSeq
          val Some((_, args)) = nodes(typeVar).applySignature(overload.signature)
          val node = graph.addNode(overload, args.tail map process)
          graphNodes(typeVar) = node
          node
      }
    }

    for (typeVar <- state.overloads.keys) {
      process(typeVar)
    }

    graph
  }
}

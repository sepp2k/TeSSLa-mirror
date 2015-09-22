package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.ASTGraph.NodeId
import de.uni_luebeck.isp.tessla.Compiler.{Graph, State, UnexpectedCompilerState}
import de.uni_luebeck.isp.tessla.TypeChecker._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}


object TypeChecker extends Compiler.Pass {

  // TODO make different error classes with metadata
  case class TypeError(message: String) extends Exception(message)

  case class FunctionSignature(
    argTypes: Seq[Set[AST.Type]],
    namedTypes: Map[String, Set[AST.Type]],
    returnType: Set[AST.Type]) {

    def isUnique: Boolean = {
      returnType.size == 1 && (argTypes forall (_.size == 1)) && (namedTypes forall (_._2.size == 1))
    }

    def isNone: Boolean = {
      returnType.isEmpty || (argTypes forall (_.isEmpty)) || (namedTypes forall (_._2.isEmpty))
    }
  }

  object FunctionSignature {
    val none = FunctionSignature(Seq(), Map(), Set())

    // TODO implement them in a more general way
    def simplify(a: Set[AST.Type]): Set[AST.Type] = {
      if (a contains ToBeInferred) {
        Set(ToBeInferred)
      } else {
        val (streamVals, otherVals) = a.partition({
          case StreamType(_) => true
          case _ => false
        })
        (simplify(streamVals map { case StreamType(x) => x }) map StreamType) ++ otherVals
      }
    }

    def intersect(a: Set[AST.Type], b: Set[AST.Type]): Set[AST.Type] = {
      if (a.isEmpty || b.isEmpty) {
        return Set()
      } else if (a contains ToBeInferred) {
        return b
      } else if (b contains ToBeInferred) {
        return a
      } else {
        val (streamValsA, otherValsA) = a.partition({
          case StreamType(_) => true case _ => false})
        val (streamValsB, otherValsB) = b.partition({
          case StreamType(_) => true case _ => false})
        val intersectedStreams = intersect(
          streamValsA map {case StreamType(x) => x},
          streamValsB map {case StreamType(x) => x}) map StreamType
        return intersectedStreams ++ (otherValsA & otherValsB)
      }
    }

    def union(a: Set[AST.Type], b: Set[AST.Type]): Set[AST.Type] = {
      simplify(a ++ b)
    }

    def lift(f: (Set[AST.Type], Set[AST.Type]) => Set[AST.Type], a: FunctionSignature, b: FunctionSignature): FunctionSignature = {
      if (a.argTypes.length != b.argTypes.length) {
        throw new IllegalArgumentException("different FunctionSignature argTypes lengths")
      }
      if (a.namedTypes.keySet != b.namedTypes.keySet) {
        throw new IllegalArgumentException("different FunctionSignature namedTypes names")
      }
      val returnType = f(a.returnType, b.returnType)
      val argTypes = (a.argTypes, b.argTypes).zipped map f
      val namedTypes = a.namedTypes.keySet.map(name => name -> f(a.namedTypes(name), b.namedTypes(name)))(collection.breakOut): Map[String, Set[AST.Type]]
      FunctionSignature(argTypes toSeq, namedTypes, returnType)
    }

    def union(a: FunctionSignature, b: FunctionSignature): FunctionSignature = lift(union, a, b)
    def intersect(a: FunctionSignature, b: FunctionSignature): FunctionSignature = lift(intersect, a, b)
  }

  abstract class FunctionResolver extends Compiler.Provider {
    def names: Set[String]

    // TODO allow error reporting from inferTypes
    def inferTypes(sig: FunctionSignature): FunctionSignature

    def provideFunction(sig: FunctionSignature): Option[AST.Function]
  }

  override def applyPass(compiler: Compiler, state: State): Try[State] = {
    val graph = state match {
      case Graph(graph) => graph
      case _ => return Failure[Compiler.State](UnexpectedCompilerState)
    }

    var fnResolvers: mutable.Map[String, mutable.Set[FunctionResolver]] =
      mutable.HashMap()

    for (provider <- compiler.providers) {
      provider match {
        case res: FunctionResolver =>
          for (name <- res.names) {
            fnResolvers.getOrElseUpdate(name, {mutable.HashSet()}) += res
          }
      }
    }

    val checker = new TypeChecker(
      fnResolvers = fnResolvers,
      graph = graph)
    Try {
      checker.typecheck()

      Graph(checker.graph)
    }
  }
}

class TypeChecker(
  val fnResolvers: mutable.Map[String, mutable.Set[FunctionResolver]],
  var graph: ASTGraph) {

  val constraints: mutable.Map[NodeId, Set[AST.Type]] = mutable.HashMap()
  val dirty: mutable.Set[NodeId] = mutable.HashSet()

  def constraint(id: NodeId): Set[AST.Type] = constraints.getOrElse(id, Set(ToBeInferred))

  def typecheck() {
    prepare()
    inference()
    processResults()
  }

  def prepare() {
    for (id <- graph.nodeIds) {
      graph.node(id) match {
        case TypeAscr(_, typ) =>
          constraints(id) = Set(typ)
        case App(IntegralConstant(_), Seq(), Seq(), _) =>
          // TODO move this into a separate pass that handles literals with type ascriptions
          constraints(id) = Set(IntType(32, true))
        case _ => {}
      }
      dirty += id
    }
  }

  def inference() {
    while (dirty.nonEmpty) {
      val id = dirty.toIterator.next()
      dirty -= id
      infer(id)
    }
  }

  def infer(id: NodeId) {
    graph.node(id) match {
      case TypeAscr(id2, _) =>
        val after = FunctionSignature.intersect(constraint(id), constraint(id2))
        updateConstraints(id, after)
        updateConstraints(id2, after)
      case App(UnresolvedFunction(name), args, namedArgs, _) =>
        // TODO cache resolvers
        // TODO error when no resolvers are found
        val resolvers = fnResolvers(name)

        var returnSig = constraint(id)
        var argSig = args map {id => constraint(id)}
        var namedSig = namedArgs map {case NamedArg(name, id) => name -> constraint(id)} toMap

        var before = FunctionSignature(argSig, namedSig, returnSig)
        val it = resolvers.toIterator
        var after = FunctionSignature.intersect(it.next().inferTypes(before), before)
        for (res <- it) {
          after = FunctionSignature.union(after, FunctionSignature.intersect(res.inferTypes(before), before))
        }
        updateConstraints(id, after.returnType)
        for ((argId, cs) <- (args, after.argTypes).zipped) {
          updateConstraints(argId, cs)
        }
        for (NamedArg(name, id) <- namedArgs) {
          updateConstraints(id, after.namedTypes(name))
        }
      case _ => {}
    }
  }

  def updateConstraints(id: NodeId, cs: Set[AST.Type]) {
    val before = constraint(id)
    val after = FunctionSignature.intersect(before, cs)
    if (before != after) {
      constraints(id) = after
      mark(id)
    }
  }

  def mark(id: NodeId) {
    dirty += id
    for (id2 <- graph.useSet(id)) {
      dirty += id2
    }
  }

  def processResults() {
    for (id <- graph.nodeIds) {
      graph.node(id) match {
        case x@(TypeAscr(_, _) | App(UnresolvedFunction(_), _, _, _)) =>
          val cs = constraint(id)
          if (cs.isEmpty) {
            throw new TypeError("no valid type for " + id)
          } else if (cs.size > 1) {
            throw new TypeError("no unique type for " + id)
          }
          // TODO actually replace wiht resolved function!
          graph.updateNode(id, x.changeType(cs.iterator.next()))
        case _ => {}
      }
    }

  }
}


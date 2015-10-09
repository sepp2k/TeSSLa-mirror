package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.ASTGraph.NodeId
import de.uni_luebeck.isp.tessla.Compiler.{Graph, State, UnexpectedCompilerState}
import de.uni_luebeck.isp.tessla.TypeChecker._

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Try}


object TypeChecker extends Compiler.Pass {
  type Type = AST.Type
  type TypeSet = Set[AST.Type]

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

    def invalid = FunctionSignature(
      argTypes map (_ => Set() : TypeSet),
      namedTypes mapValues (_ => Set() : TypeSet),
      Set())

    def sameShapeAs(sig: FunctionSignature): Boolean =
      argTypes.length == sig.argTypes.length && namedTypes.keySet == sig.namedTypes.keySet
  }

  def simpleSignature(x: TypeSet*): FunctionSignature =
      FunctionSignature(x.init, Map(), x.last)

  implicit def typeToTypeSet(typ: Type): TypeSet = Set(typ)

  def typeJoin(a: Type, b: Type): Type = {
    (a, b) match {
      case (ToBeInferred, _) | (_, ToBeInferred) => ToBeInferred
      case (StreamType(x), StreamType(y)) => StreamType(typeJoin(x, y))
      case (x, y) if x == y => x
      case _ => ToBeInferred
    }
  }

  def typeMeet(a: Type, b: Type): Option[Type] = {
    (a, b) match {
      case (ToBeInferred, x) => Some(x)
      case (x, ToBeInferred) => Some(x)
      case (StreamType(x), StreamType(y)) => typeMeet(x, y) map StreamType
      case (x, y) if x == y => Some(x)
      case _ => None
    }
  }

  def typeLessThan(a: Type, b: Type) = a != b && typeJoin(a, b) == b

  def typeJoin(a: TypeSet, b: TypeSet): TypeSet = {
    typeSetSimplify(a ++ b)
  }

  def typeMeet(a: TypeSet, b: TypeSet): TypeSet = {
    typeSetSimplify(a flatMap (ta => b flatMap (tb => typeMeet(ta, tb))))
  }

  def typeSetSimplify(a: TypeSet): TypeSet = {
    a filterNot (ta => a exists (typeLessThan(ta, _)))
  }

  def typeRemoveVars(a: Type): Type = a match {
    case TypeVar(nr) => ToBeInferred
    case StreamType(elType) => StreamType(typeRemoveVars(elType))
    case x => x
  }

  def typeRemoveVars(a: TypeSet): TypeSet =
    typeSetSimplify(a map typeRemoveVars)

  // TODO check semantics for type vars with non singleton TypeSets

  def typeCollectBindings(a: Type, b: Type): Map[Int, Type] = (a, b) match {
    case (TypeVar(nr), typ) => Map(nr -> typ)
    case (StreamType(ta), StreamType(tb)) => typeCollectBindings(ta, tb)
    case _ => Map()
  }

  def typeCollectBindings(a: TypeSet, b: TypeSet): Map[Int, TypeSet] = {
    var result: Map[Int, TypeSet] = Map()
    for (ta <- a) {
      for (tb <- b) {
        for ((k, v) <- typeCollectBindings(ta, tb)) {
          result += (k -> typeJoin(Set(v), result.getOrElse(k, Set())))
        }
      }
    }
    result
  }

  def typeReplaceBindings(map: Map[Int, TypeSet], a: Type): TypeSet = a match {
    case TypeVar(nr) => map.getOrElse(nr, Set(ToBeInferred))
    case StreamType(elType) => typeReplaceBindings(map, elType) map StreamType
    case x => Set(x)
  }

  def typeReplaceBindings(map: Map[Int, TypeSet], a: TypeSet): TypeSet =
    typeSetSimplify(a flatMap (typeReplaceBindings(map, _)))



  def liftSig(f: (Set[AST.Type], Set[AST.Type]) => Set[AST.Type], a: FunctionSignature, b: FunctionSignature): FunctionSignature = {
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


  def liftSig(f: Set[AST.Type] => Set[AST.Type], a: FunctionSignature) =
    FunctionSignature(a.argTypes map f, a.namedTypes mapValues f, f(a.returnType))

  def typeJoin(a: FunctionSignature, b: FunctionSignature): FunctionSignature = liftSig(typeJoin, a, b)
  def typeMeet(a: FunctionSignature, b: FunctionSignature): FunctionSignature = liftSig(typeMeet, a, b)
  def typeRemoveVars(a: FunctionSignature): FunctionSignature = liftSig(typeRemoveVars, a)
  def typeReplaceBindings(map: Map[Int, TypeSet], a: FunctionSignature): FunctionSignature = liftSig(typeReplaceBindings(map, _), a)
  def typeCollectBindings(a: FunctionSignature, b: FunctionSignature): Map[Int, TypeSet] = {
    // TODO avoid code duplication
    if (a.argTypes.length != b.argTypes.length) {
      throw new IllegalArgumentException("different FunctionSignature argTypes lengths")
    }
    if (a.namedTypes.keySet != b.namedTypes.keySet) {
      throw new IllegalArgumentException("different FunctionSignature namedTypes names")
    }

    val returnCollected = typeCollectBindings(a.returnType, b.returnType)
    val argCollected = (a.argTypes, b.argTypes).zipped map typeCollectBindings
    val namedTypes = a.namedTypes.keySet.map(name => typeCollectBindings(a.namedTypes(name), b.namedTypes(name)))

    var result: Map[Int, TypeSet] = Map()

    for (mapping <- Seq(returnCollected) ++ argCollected ++ namedTypes) {
      for ((k, v) <- mapping) {
        result += (k -> typeMeet(v, result.getOrElse(k, Set(ToBeInferred))))
      }
    }
    result
  }

  abstract class FunctionResolver extends Compiler.Provider {
    def names: Set[String]

    // TODO allow error reporting from inferTypes
    def inferTypes(sig: FunctionSignature): FunctionSignature

    def provideFunction(sig: FunctionSignature): Option[AST.Function]
  }

  class SimpleFunctionResolver(name: String, signature: FunctionSignature) extends FunctionResolver {
    def names = Set(name)
    def provideFunction(fn: FunctionSignature) = Some(UnresolvedFunction(name))

    val sigWithoutVals = typeRemoveVars(signature)

    def inferTypes(sig: FunctionSignature): FunctionSignature = {
      if (!sig.sameShapeAs(signature)) {
        return sig.invalid
      }
      val updated = typeMeet(sig, sigWithoutVals)
      println(updated)

      val collected = typeCollectBindings(signature, sig)
      println(collected)

      val updatedSignature = typeReplaceBindings(collected, signature)

      typeMeet(updated, updatedSignature)
    }
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
          constraints(id) = Set(IntType(32, false))
        case App(StringConstant(_), Seq(), Seq(), _) =>
          // TODO move this into a separate pass that handles literals with type ascriptions
          constraints(id) = Set(StringType)
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
        val after = typeMeet(constraint(id), constraint(id2))
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
        var after = typeMeet(it.next().inferTypes(before), before)
        for (res <- it) {
          after = typeJoin(after, typeMeet(res.inferTypes(before), before))
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
    val after = typeMeet(before, cs)
    if (after.isEmpty) {
      throw new TypeError("no valid type for " + id)
    }
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
        case x@(TypeAscr(_, _) | App(UnresolvedFunction(_), _, _, _)
             | App(IntegralConstant(_), _, _, _)) => // TODO change when fixing integer literals
          val cs = constraint(id)
          if (cs.isEmpty) {
            throw new TypeError("no valid type for " + id)
          } else if (cs.size > 1 || (cs contains ToBeInferred)) {
            throw new TypeError("no unique type for " + id)
          }
          // TODO actually replace wiht resolved function!
          graph.updateNode(id, x.changeType(cs.iterator.next()))
        case _ => {}
      }
    }

  }
}


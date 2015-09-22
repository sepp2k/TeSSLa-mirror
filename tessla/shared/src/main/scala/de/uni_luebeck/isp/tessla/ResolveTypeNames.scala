package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.Compiler.{UnexpectedCompilerState, Tree, State}

import scala.util.{Success, Failure, Try}

object ResolveTypeNames extends Compiler.Pass {
  // TODO do not hardcode this

  case class UnknownTypeNameError(val typeName: String) extends Exception

  override def applyPass(compiler: Compiler, state: State): Try[State] = {
    val ast = state match {
      case Tree(ast) => ast
      case _ => return Failure[Compiler.State](UnexpectedCompilerState)
    }

    Try {
      Tree(recurse(ast))
    }
  }

  def recurse(spec: Spec): Spec = {
    spec.copy(statements = spec.statements map recurse)
  }

  def recurse(stmt: Statement): Statement = {
    stmt match {
      case s @ Def(name, definition) =>
        s.copy(definition = recurse(definition))
      case s @ MacroDef(name, args, definition) =>
        s.copy(definition = recurse(definition))
      case s @ Out(name) => s
    }
  }

  def recurse(term: TreeTerm): TreeTerm = {
    val outTerm = term.term map recurse
    TreeTerm(outTerm changeType recurse(outTerm.typ))
  }

  def recurse(typ: Type): Type = {
    typ match {
      // TODO before not hardcoding this, hardcode this some more
      case UnresolvedPrimitiveType(name) => name match {
        case "int" => IntType(32, true)
        case "uint" => IntType(32, false)
        case "bool" => BoolType
        case "event" => UnitType
        case _ => throw UnknownTypeNameError(name)
      }
      case StreamType(elType) => StreamType(recurse(elType))
      case x => x
    }
  }
}

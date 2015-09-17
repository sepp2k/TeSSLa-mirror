package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.Location

object AST {
  abstract class Locatable {
    var loc: Option[Location] = None
    def updateLoc(loc: Location) { this.loc = Some(loc) }
  }
  
  abstract class Function extends Locatable
  case class UnresolvedFunction(name: String) extends Function
  
  abstract class Constant extends Locatable
  case class UnresolvedConstant(name: String) extends Constant
  case class IntegralConstant(value: Integer) extends Constant
  
  abstract class Type extends Locatable
  case object ToBeInferred extends Type
  case class UnresolvedPrimitiveType(name: String) extends Type
  case class StreamType(elType: Type) extends Type
  
  case class Spec(statements: List[Statement]) extends Locatable
  
  sealed abstract class Statement extends Locatable
  case class Def(name: String, definition: TreeTerm) extends Statement
  case class MacroDef(name: String, args: List[String], definition: TreeTerm) extends Statement
  case class Out(name: String) extends Statement
  
  abstract sealed class Term[+SubTerm](val typ: Type) extends Locatable {
    def foreach[U](f: SubTerm => U) {}
    def map[U](f: SubTerm => U): Term[U]
  }

  abstract class LeafTerm(override val typ: Type) extends Term[Nothing](typ) {
    def map[U](f: Nothing => U): Term[U] = this
  }

  sealed case class TreeTerm(term: Term[TreeTerm])
  
  sealed case class NamedArg[+SubTerm](name: String, arg: SubTerm) extends Locatable
  
  case class UnresolvedTerm(name: String, override val typ: Type = ToBeInferred) extends LeafTerm(typ)
  case class Const(const: Constant, override val typ: Type = ToBeInferred) extends LeafTerm(typ)
  case class Ref(name: String, override val typ: Type = ToBeInferred) extends LeafTerm(typ)
  case class App[SubTerm](
      fn: Function,
      arguments: List[SubTerm] = List(),
      namedArguments: List[NamedArg[SubTerm]] = List(),
      override val typ: Type = ToBeInferred
  ) extends Term[SubTerm](typ) {
    override def foreach[U](f: SubTerm => U): Unit = {
      for (term <- arguments) {f(term)}
      for (NamedArg(_, term) <- namedArguments) {f(term)}
    }
    override def map[U](f: SubTerm => U): App[U] =  {
      App(fn, arguments map f, namedArguments map {case NamedArg(name, x) => NamedArg(name, f(x))}, typ)
    }
  }
  
  case class TypeAscr[SubTerm](term: SubTerm, override val typ: Type) extends Term[SubTerm](typ)  {
    override def foreach[U](f: SubTerm => U): Unit = {
      f(term)
    }
    override def map[U](f: SubTerm => U): TypeAscr[U] = {
      TypeAscr(f(term), typ)
    }
  }
}
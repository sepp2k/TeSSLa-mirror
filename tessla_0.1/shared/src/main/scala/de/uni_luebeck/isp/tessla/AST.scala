package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.Location
import scala.language.higherKinds

object AST {
  abstract class Locatable {
    var loc: Option[Location] = None
    def updateLoc(loc: Location) { this.loc = Some(loc) }

    def from(l: Locatable): this.type = { this.loc = l.loc; this }
  }

  abstract class Function extends Locatable
  case class UnresolvedFunction(name: String) extends Function
  case class ArithmeticFunction(op: ArithmeticOp) extends Function
  case class ArithmeticStreamFunction(op: ArithmeticOp) extends Function
  case object ConstantStreamFunction extends Function

  abstract class ArithmeticOp
  case object Add extends ArithmeticOp
  case object Sub extends ArithmeticOp
  case object Mul extends ArithmeticOp
  
  abstract class Constant extends Function
  case class IntegralConstant(value: Integer) extends Constant
  case class StringConstant(value: String) extends Constant
  abstract class InputConstant extends Constant

  abstract class Type extends Locatable {
    def map(f: Type => Type): Type = this
  }
  case object ToBeInferred extends Type
  case class TypeVar(nr: Int) extends Type
  case class UnresolvedPrimitiveType(name: String) extends Type
  case class StreamType(elType: Type) extends Type
  case class IntType(bits: Int, signed: Boolean) extends Type
  case object UnitType extends Type
  case object BoolType extends Type
  case object StringType extends Type

  case class Spec(statements: List[Statement]) extends Locatable

  sealed abstract class Statement extends Locatable
  case class Def(name: String, definition: TreeTerm) extends Statement
  case class MacroDef(name: String, args: List[String], definition: TreeTerm) extends Statement
  case class Out(name: String) extends Statement

  abstract sealed class Term[+SubTerm](val typ: Type) extends Locatable {
    def foreach[U](f: SubTerm => U) {}
    def map[U](f: SubTerm => U): Term[U]
    def changeType(typ: Type): Term[SubTerm]
  }

  abstract class LeafTerm(override val typ: Type) extends Term[Nothing](typ) {
    def map[U](f: Nothing => U): Term[U] = this
  }

  sealed case class TreeTerm(term: Term[TreeTerm])

  sealed case class NamedArg[+SubTerm](name: String, arg: SubTerm) extends Locatable

  case class UnresolvedTerm(name: String, override val typ: Type = ToBeInferred) extends LeafTerm(typ) {
    def changeType(typ: Type) = this.copy(typ = typ) from this
  }
  case class Ref(name: String, override val typ: Type = ToBeInferred) extends LeafTerm(typ) {
    def changeType(typ: Type) = this.copy(typ = typ) from this
  }

  abstract class ArgumentTaker[SubTerm, Self[U] <: Term[U]](
      arguments: List[SubTerm] = List(),
      namedArguments: List[NamedArg[SubTerm]] = List(),
      override val typ: Type) extends Term[SubTerm](typ) {
    override def foreach[U](f: SubTerm => U): Unit = {
      for (term <- arguments) { f(term) }
      for (NamedArg(_, term) <- namedArguments) { f(term) }
    }
    def cpy[U](arguments: List[U], namedArguments: List[NamedArg[U]], typ: Type): Self[U]
    override def map[U](f: SubTerm => U): Self[U] = {
      cpy(arguments map f, namedArguments map { case NamedArg(name, x) => NamedArg(name, f(x)) }, typ)
    }
  }

  //TODO: Should take a StringConstant rather than a String (salt). 
  // It might be specified by a variable or even composed by others. -- Normann 
  case class Monitor[SubTerm](
      salt: String,
      arguments: List[SubTerm] = List(),
      namedArguments: List[NamedArg[SubTerm]] = List(),
      override val typ: Type = BoolType) extends ArgumentTaker[SubTerm, Monitor](arguments, namedArguments, typ) {

    override def changeType(typ: Type): Term[SubTerm] = this.copy(typ = typ) from this

    override def cpy[U](arguments: List[U], namedArguments: List[NamedArg[U]], typ: Type): Monitor[U] =
      Monitor(salt, arguments, namedArguments, typ)
  }

  case class App[SubTerm](
      fn: Function,
      arguments: List[SubTerm] = List(),
      namedArguments: List[NamedArg[SubTerm]] = List(),
      override val typ: Type = ToBeInferred) extends ArgumentTaker[SubTerm, App](arguments, namedArguments, typ) {

    override def cpy[U](arguments: List[U], namedArguments: List[NamedArg[U]], typ: Type): App[U] =
      App(fn, arguments, namedArguments, typ)

    def changeType(typ: Type) = this.copy(typ = typ) from this
  }

  case class TypeAscr[SubTerm](term: SubTerm, override val typ: Type) extends Term[SubTerm](typ) {
    override def foreach[U](f: SubTerm => U): Unit = {
      f(term)
    }
    override def map[U](f: SubTerm => U): TypeAscr[U] = {
      TypeAscr(f(term), typ)
    }
    def changeType(typ: Type) = this.copy(typ = typ) from this
  }
}
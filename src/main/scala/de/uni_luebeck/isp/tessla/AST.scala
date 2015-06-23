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
  
  case class Spec(statements: List[Statement]) extends Locatable
  
  sealed abstract class Statement extends Locatable
  case class Def(name: String, definition: Term) extends Statement
  case class MacroDef(name: String, args: List[String], definition: Term) extends Statement
  case class Out(name: String) extends Statement
  
  abstract sealed class Term(val typ: Type)  extends Locatable
  
  case class UnresolvedTerm(name: String, override val typ: Type = ToBeInferred) extends Term(typ)
  case class Const(const: Constant, override val typ: Type = ToBeInferred) extends Term(typ)
  case class Ref(name: String, override val typ: Type = ToBeInferred) extends Term(typ)
  case class App(
      fn: Function,
      arguments: List[Term] = List(),
      namedArguments: List[Def] = List(),
      override val typ: Type = ToBeInferred
  ) extends Term(typ)
  
  case class TypeAscr(term: Term, override val typ: Type) extends Term(typ) 
}
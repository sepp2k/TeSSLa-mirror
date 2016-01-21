package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.Location

object Ast {
  case class Spec(statements: Seq[Statement])

  case class Identifier(name: String, loc: Location)
  case class IntLit(value: BigInt, loc: Location)
  case class StringLit(value: String, loc: Location)

  abstract class Statement
  case class Def(
    name: Identifier,
    typeAscr: Option[Type],
    definition: Expr) extends Statement
  case class MacroDef(
    name: Identifier,
    args: Seq[MacroArg],
    typeAscr: Option[Type],
    definition: Expr) extends Statement

  case class MacroArg(name: Identifier, typeAscr: Option[Type])

  abstract class Expr
  case class ExprName(name: Identifier) extends Expr
  case class ExprApp(name: Identifier, args: Seq[AppArg]) extends Expr
  case class ExprGrouped(expr: Expr) extends Expr
  case class ExprTypeAscr(expr: Expr, `type`: Type) extends Expr
  case class ExprIntLit(value: IntLit) extends Expr
  case class ExprStringLit(value: StringLit) extends Expr

  abstract class AppArg
  case class PosArg(expr: Expr) extends AppArg
  case class NamedArg(name: Identifier, expr: Expr) extends AppArg

  abstract class Type
  case class TypeName(name: Identifier) extends Type
  case class TypeApp(name: Identifier, args: Seq[Type]) extends Type
}


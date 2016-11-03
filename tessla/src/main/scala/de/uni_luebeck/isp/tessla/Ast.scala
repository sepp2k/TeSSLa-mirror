package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.Location

object Ast {
  case class Spec(statements: Seq[Statement])

  case class Identifier(name: String, loc: SourceLoc)
  case class IntLit(value: BigInt, loc: SourceLoc)
  case class StringLit(value: String, loc: SourceLoc)
  case class BoolLit(value: Boolean, loc: SourceLoc)
  case class FloatLit(value: BigDecimal, loc: SourceLoc)


  abstract sealed class Statement
  case class Def(
    name: Identifier,
    macroArgs: Seq[MacroArg],
    typeAscr: Option[Type],
    definition: Expr,
    loc: NestedLoc) extends Statement

  case class In(name: Identifier, typeAscr: Type, loc: NestedLoc) extends Statement
  case class Out(name: Identifier, loc: NestedLoc) extends Statement

  case class MacroArg(name: Identifier, typeAscr: Option[Type])

  sealed abstract class Expr
  case class ExprName(name: Identifier) extends Expr
  case class ExprApp(name: Identifier, args: Seq[AppArg], loc: SourceLoc) extends Expr
  case class ExprGrouped(expr: Expr, loc: SourceLoc) extends Expr
  case class ExprTypeAscr(expr: Expr, `type`: Type) extends Expr
  case class ExprIntLit(value: IntLit) extends Expr
  case class ExprStringLit(value: StringLit) extends Expr
  case class ExprBoolLit(value: BoolLit) extends Expr
  case class ExprFloatLit(value: FloatLit) extends Expr

  abstract class AppArg
  case class PosArg(expr: Expr) extends AppArg
  case class NamedArg(name: Identifier, expr: Expr) extends AppArg

  abstract class Type
  case class TypeName(name: Identifier) extends Type
  case class TypeApp(name: Identifier, args: Seq[Type], loc: SourceLoc) extends Type
}


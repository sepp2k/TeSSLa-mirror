package de.uni_luebeck.isp.tessla

object Ast {
  case class Spec(statements: Seq[Statement]) {
    override def toString = statements.mkString("\n")
  }

  case class Identifier(name: String, loc: Location) {
    override def toString = name
  }

  abstract sealed class Statement
  case class Def(
    name: Identifier,
    macroArgs: Seq[MacroArg],
    typeAscr: Option[Type],
    definition: Expr,
    loc: Location) extends Statement {
    override def toString = {
      val argList =
        if (macroArgs.isEmpty) ""
        else macroArgs.mkString("(", ", ", ")")
      s"def $name$argList := $definition"
    }
  }

  case class Out(name: Identifier, loc: Location) extends Statement
  case class In(name: Identifier, typeAscr: Type, loc: Location) extends Statement {
    override def toString = s"in $name: $typeAscr"
  }

  case class MacroArg(name: Identifier, typeAscr: Option[Type]) {
    override def toString = typeAscr match {
      case Some(t) => s"${name.name}: $t"
      case None => name.name
    }
  }

  sealed abstract class Expr {
    def loc: Location
    def toString(inner: Boolean): String
    override def toString = toString(false)
  }
  case class ExprName(name: Identifier) extends Expr {
    def loc = name.loc
    override def toString(inner: Boolean) = name.name
  }
  private val ID_PATTERN = "^[a-zA-Z0-9_]+$".r
  case class ExprApp(id: Identifier, args: Seq[AppArg], loc: SourceLoc) extends Expr {
    override def toString(inner: Boolean) = {
      val str = (id.name, args) match {
        case (ID_PATTERN(), _) | (_, Seq()) => s"${id.name}(${args.mkString(", ")})"
        case ("if then", Seq(cond, thenCase)) =>
          s"if $cond then $thenCase"
        case ("if then else", Seq(cond, thenCase, elseCase)) =>
          s"if $cond then $thenCase else $elseCase"
        case (name, Seq(PosArg(arg))) =>
          if (inner) s"($name${arg.toString(inner = true)})"
          else s"$name${arg.toString(inner = true)}"
        case (name, Seq(PosArg(lhs), PosArg(rhs))) =>
          if (inner) s"(${lhs.toString(inner = true)} $name ${rhs.toString(inner = true)})"
          else s"${lhs.toString(inner = true)} $name ${rhs.toString(inner = true)}"
        case (name, _) => s"$name(${args.mkString(", ")})"
      }
      if (inner) s"($str)" else str
    }
  }
  case class ExprTypeAscr(expr: Expr, `type`: Type) extends Expr {
    def loc = expr.loc.merge(`type`.loc)
    override def toString(inner: Boolean) = {
      if (inner) s"(${expr.toString(inner = true)})"
      else s"${expr.toString(inner = true)}"
    }
  }
  case class ExprIntLit(value: BigInt, loc: Location) extends Expr {
    override def toString(inner: Boolean) = value.toString
  }
  case class ExprStringLit(value: String, loc: Location) extends Expr {
    override def toString(inner: Boolean) = value.toString
  }
  case class ExprBoolLit(value: Boolean, loc: Location) extends Expr {
    override def toString(inner: Boolean) = value.toString
  }
  case class ExprUnit(loc: SourceLoc) extends Expr {
    override def toString(inner: Boolean) = "()"
  }
  case class ExprBlock(definitions: Seq[Def], expression: Expr, loc: SourceLoc) extends Expr {
    override def toString(inner: Boolean) = s"{\n${definitions.mkString("\n")}\n$expression\n}"
  }

  abstract class AppArg
  case class PosArg(expr: Expr) extends AppArg {
    override def toString = expr.toString
  }
  case class NamedArg(name: Identifier, expr: Expr) extends AppArg {
    override def toString = s"$name = $expr"
  }

  abstract class Type {
    def loc: Location
  }
  case class TypeName(name: Identifier) extends Type {
    def loc = name.loc
    override def toString = name.name
  }
  case class TypeApp(name: Identifier, args: Seq[Type], loc: SourceLoc) extends Type {
    override def toString = s"$name<${args.mkString(", ")}>"
  }
}


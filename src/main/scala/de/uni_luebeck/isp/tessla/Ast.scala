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
                  body: Expr,
                  loc: Location) extends Statement {
    override def toString = {
      val argList =
        if (macroArgs.isEmpty) ""
        else macroArgs.mkString("(", ", ", ")")
      s"def $name$argList := $body"
    }
  }

  case class In(name: Identifier, typeAscr: Type, loc: Location) extends Statement {
    override def toString = s"in $name: $typeAscr"
  }
  case class Out(expr: Expr, nameOpt: Option[Identifier], loc: Location) extends Statement {
    def name = nameOpt.map(_.name).getOrElse(expr.toString)
    override def toString = s"out $expr as $name"
  }

  case class OutAll(loc: Location) extends Statement {
    def name = "out *"
    override def toString = name
  }

  case class MacroArg(name: Identifier, typeAscr: Option[Type]) {
    override def toString = typeAscr match {
      case Some(t) => s"${name.name}: $t"
      case None => name.name
    }

    def loc: Location = typeAscr match {
      case Some(t) => name.loc.merge(t.loc)
      case None => name.loc
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
  case class ExprTimeLit(value: BigInt, unit: TimeUnit.TimeUnit, loc: Location) extends Expr {
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

  abstract class AppArg {
    def loc: Location
  }
  case class PosArg(expr: Expr) extends AppArg {
    override def toString = expr.toString
    def loc = expr.loc
  }
  case class NamedArg(name: Identifier, expr: Expr) extends AppArg {
    override def toString = s"$name = $expr"
    def loc = name.loc.merge(expr.loc)
  }

  abstract class Type {
    def loc: Location
    def withLoc(loc: Location): Type
  }
  case class TypeName(name: Identifier) extends Type {
    def loc = name.loc
    override def toString = name.name
    def withLoc(loc: Location): TypeName = TypeName(name.copy(loc = loc))
  }
  case class TypeApp(name: Identifier, args: Seq[Type], loc: Location) extends Type {
    override def toString = s"$name<${args.mkString(", ")}>"
    def withLoc(loc: Location): TypeApp = copy(loc = loc)
  }
}


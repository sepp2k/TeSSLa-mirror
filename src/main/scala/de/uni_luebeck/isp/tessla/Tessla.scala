package de.uni_luebeck.isp.tessla

object Tessla {
  case class Specification(statements: Seq[Statement]) {
    override def toString = statements.mkString("\n")
  }

  case class Identifier(name: String, loc: Location) {
    override def toString = name
  }

  abstract sealed class Statement {
    def loc: Location
  }

  case class Definition(
                         id: Identifier,
                         typeParameters: Seq[Identifier],
                         parameters: Seq[Parameter],
                         returnType: Option[Type],
                         body: Expression,
                         loc: Location) extends Statement {
    override def toString = {
      val typeParameterList =
        if (typeParameters.isEmpty) ""
        else typeParameters.mkString("[", ", ", "]")
      val parameterList =
        if (parameters.isEmpty) ""
        else parameters.mkString("(", ", ", ")")
      s"def $id$typeParameterList$parameterList := $body"
    }
  }

  case class In(id: Identifier, streamType: Type, loc: Location) extends Statement {
    override def toString = s"in $id: $streamType"
  }

  case class Out(expr: Expression, idOpt: Option[Identifier], loc: Location) extends Statement {
    def name = idOpt.map(_.name).getOrElse(expr.toString)
    override def toString = s"out $expr as $name"
  }

  case class OutAll(loc: Location) extends Statement {
    def name = "out *"
    override def toString = name
  }

  case class Parameter(id: Identifier, parameterType: Option[Type]) {
    override def toString = parameterType match {
      case Some(t) => s"${id.name}: $t"
      case None => id.name
    }

    def loc: Location = parameterType match {
      case Some(t) => id.loc.merge(t.loc)
      case None => id.loc
    }
  }

  sealed abstract class Expression {
    def loc: Location
    def toString(inner: Boolean): String
    override def toString = toString(false)
  }

  case class Variable(id: Identifier) extends Expression {
    def loc = id.loc
    override def toString(inner: Boolean) = id.name
  }

  private val ID_PATTERN = "^[a-zA-Z0-9_]+$".r

  case class MacroCall(macroID: Identifier, typeArgs: Seq[Type], args: Seq[Argument], loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      val typeArgList =
        if (typeArgs.isEmpty) ""
        else typeArgs.mkString("[", ", ", "]")

      val str = (macroID.name, args) match {
        case (ID_PATTERN(), _) | (_, Seq()) => s"$macroID$typeArgList(${args.mkString(", ")})"
        case ("if then", Seq(cond, thenCase)) =>
          s"if $cond then $thenCase"
        case ("if then else", Seq(cond, thenCase, elseCase)) =>
          s"if $cond then $thenCase else $elseCase"
        case (name, Seq(PositionalArgument(arg))) =>
          if (inner) s"($name${arg.toString(inner = true)})"
          else s"$name${arg.toString(inner = true)}"
        case (name, Seq(PositionalArgument(lhs), PositionalArgument(rhs))) =>
          if (inner) s"(${lhs.toString(inner = true)} $name ${rhs.toString(inner = true)})"
          else s"${lhs.toString(inner = true)} $name ${rhs.toString(inner = true)}"
        case (name, _) => s"$name$typeArgList(${args.mkString(", ")})"
      }
      if (inner) s"($str)" else str
    }
  }

  case class Block(definitions: Seq[Definition], expression: Expression, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"{\n${definitions.mkString("\n")}\n$expression\n}"
  }

  case class Literal(value: LiteralValue, loc: Location) extends Expression {
    override def toString(inner: Boolean) = value.toString
  }

  sealed abstract class LiteralValue {
    def value: Any
    override def toString = value.toString
  }

  case class IntLiteral(value: BigInt) extends LiteralValue

  case class TimeSpanLiteral(value: BigInt, unit: TimeUnit) extends LiteralValue {
    override def toString = s"$value $unit"
  }

  case class StringLiteral(value: String) extends LiteralValue {
    override def toString = s""""$value""""
  }

  case class BoolLiteral(value: Boolean) extends LiteralValue

  case object Unit extends LiteralValue {
    override def value = ()
  }

  abstract class Argument {
    def loc: Location
  }

  case class PositionalArgument(expr: Expression) extends Argument {
    override def toString = expr.toString
    def loc = expr.loc
  }

  case class NamedArgument(id: Identifier, expr: Expression) extends Argument {
    override def toString = s"$id = $expr"
    def loc = id.loc.merge(expr.loc)
  }

  sealed abstract class Type {
    def loc: Location
    def withLoc(loc: Location): Type
  }

  case class SimpleType(id: Identifier) extends Type {
    def loc = id.loc
    override def toString = id.name
    def withLoc(loc: Location): SimpleType = SimpleType(id.copy(loc = loc))
  }

  case class TypeApplication(id: Identifier, args: Seq[Type], loc: Location) extends Type {
    override def toString = s"$id[${args.mkString(", ")}]"
    def withLoc(loc: Location): TypeApplication = copy(loc = loc)
  }
}
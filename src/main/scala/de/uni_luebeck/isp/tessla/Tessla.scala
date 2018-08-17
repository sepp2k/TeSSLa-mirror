package de.uni_luebeck.isp.tessla

import java.beans.Expression

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

  case class Definition( annotations: Seq[Identifier],
                         id: Identifier,
                         typeParameters: Seq[Identifier],
                         parameters: Seq[Parameter],
                         returnType: Option[Type],
                         headerLoc: Location,
                         body: Expression,
                         loc: Location) extends Statement {
    override def toString = toString(objectNotation = false)

    def toString(objectNotation: Boolean) = {
      val annotationList = annotations.map("@" + _ + "\n").mkString
      val typeParameterList =
        if (typeParameters.isEmpty) ""
        else typeParameters.mkString("[", ", ", "]")
      val parameterList =
        if (parameters.isEmpty) ""
        else parameters.mkString("(", ", ", ")")
      val defString = if (objectNotation) "" else "def "
      val assign = if (objectNotation) "=" else ":="
      s"$annotationList$defString$id$typeParameterList$parameterList $assign $body"
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

  case class MacroCall(mac: Expression, typeArgs: Seq[Type], args: Seq[Argument], loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      val typeArgList =
        if (typeArgs.isEmpty) ""
        else typeArgs.mkString("[", ", ", "]")

      var atomic = true
      val str = mac match {
        case Variable(id) =>
          (id.name, args) match {
            case (ID_PATTERN(), _) | (_, Seq()) =>
              s"$mac$typeArgList(${args.mkString(", ")})"
            case ("if then", Seq(cond, thenCase)) =>
              atomic = false
              s"if $cond then $thenCase"
            case (name, Seq(PositionalArgument(arg))) =>
              s"$name${arg.toString(inner = true)}"
            case (name, Seq(PositionalArgument(lhs), PositionalArgument(rhs))) =>
              atomic = false
              s"${lhs.toString(inner = true)} $name ${rhs.toString(inner = true)}"
            case (name, _) =>
              s"$name$typeArgList(${args.mkString(", ")})"
          }
        case _ =>
          s"${mac.toString(inner = true)}$typeArgList(${args.mkString(", ")})"
      }
      if (inner && !atomic) s"($str)" else str
    }
  }

  case class StaticIfThenElse(condition: Expression, thenCase: Expression, elseCase: Expression, loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      val str = s"if $condition then $thenCase else $elseCase"
      if (inner) s"($str)" else str
    }
  }

  case class Block(definitions: Seq[Definition], expression: Expression, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"{\n${definitions.mkString("\n")}\n$expression\n}"
  }

  case class ObjectLiteral(members: Seq[Definition], loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      val memberStrings = members.map(_.toString(objectNotation = true))
      memberStrings.mkString("${", ", ", "}")
    }
  }

  case class Tuple(elements: Seq[Expression], loc: Location) extends Expression {
    override def toString(inner: Boolean) = elements.mkString("(", ", ", ")")
  }

  case class MemberAccess(receiver: Expression, member: Identifier, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"${receiver.toString(inner = true)}.$member"
  }

  case class Literal(value: LiteralValue, loc: Location) extends Expression {
    override def toString(inner: Boolean) = value.toString
  }

  sealed abstract class LiteralValue {
    def value: Any
    override def toString = value.toString
  }

  case class IntLiteral(value: BigInt) extends LiteralValue

  case class TimeLiteral(value: BigInt, unit: TimeUnit) extends LiteralValue {
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

  case class FunctionType(parameterTypes: Seq[Type], returnType: Type, loc: Location) extends Type {
    override def toString = s"(${parameterTypes.mkString(", ")}) => $returnType]"
    def withLoc(loc: Location): FunctionType = copy(loc = loc)
  }
}
package de.uni_luebeck.isp.tessla

import Tessla.Identifier

object ScopedTessla {
  type Scope = Map[String, Expression]

  def scopeToString(scope: Scope) = ""

  case class Specification(globalScope: Scope, outStreams: Seq[OutStream], outAllLocation: Option[Location]) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      scopeToString(globalScope) + "\n" + outStreams.mkString("\n") + outAllString
    }

    def outAll = outAllLocation.isDefined
  }

  case class OutStream(expr: Expression, idOpt: Option[Identifier], loc: Location) {
    def name = idOpt.map(_.name).getOrElse(expr.toString)
    override def toString = s"out $expr as $name"
  }

  sealed abstract class Expression {
    def loc: Location
  }

  case class Macro(parameters: Seq[Parameter],
                   returnType: Option[Type],
                   body: Expression,
                   loc: Location) extends Expression {
    override def toString = {
      val parameterList = parameters.mkString("(", ", ", ")")
      s"$parameterList => $body"
    }
  }

  case class InStream(id: Identifier, streamType: Type, loc: Location) extends Expression {
    override def toString = s"in $id: $streamType"
  }

  case class Parameter(id: Identifier, parameterType: Option[Type], loc: Location) extends Expression {
    override def toString = parameterType match {
      case Some(t) => s"${id.name}: $t"
      case None => id.name
    }
  }

  case class Variable(id: Identifier, loc: Location) extends Expression {
    override def toString = id.toString
  }

  // TODO: Document Overload and Overloads
  case class Overload(parameters: Seq[(Identifier, Option[Type])], id: Identifier)

  case class Overloads(overloads: Seq[Overload], loc: Location) extends Expression

  case class MacroCall(macroID: Identifier, args: Map[Identifier, Expression], loc: Location) extends Expression {
    override def toString = args.mkString(s"$macroID(", ", ", ")")
  }

  case class TypeAssertion(expr: Expression, `type`: Type) extends Expression {
    def loc = expr.loc.merge(`type`.loc)
    override def toString = s"(${expr.toString} : ${`type`})"
  }

  case class IntLiteral(value: BigInt, loc: Location) extends Expression {
    override def toString = value.toString
  }

  case class TimeLiteral(value: BigInt, unit: TimeUnit.TimeUnit, loc: Location) extends Expression {
    override def toString = value.toString
  }

  case class StringLiteral(value: String, loc: Location) extends Expression {
    override def toString = value.toString
  }

  case class BoolLiteral(value: Boolean, loc: Location) extends Expression {
    override def toString = value.toString
  }

  case class Unit(loc: Location) extends Expression {
    override def toString = "()"
  }

  case class Block(scope: Scope, expression: Expression, loc: Location) extends Expression {
    override def toString = s"{\n${scopeToString(scope)}\n$expression\n}"
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
  }

  case class SimpleType(id: Identifier, loc: Location) extends Type {
    override def toString = id.name
  }

  case class GenericType(id: Identifier, args: Seq[Type], loc: Location) extends Type {
    override def toString = s"$id<${args.mkString(", ")}>"
  }
}
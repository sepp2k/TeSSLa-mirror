package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import Tessla.Identifier

object ScopedTessla {
  class Scope(val parent: Option[Scope] = None) {
    private val entries = mutable.Map[String, Overloads]()

    def addOverload(name: String, overload: Overload): Unit = {
      val existingOverloads = entries.getOrElse(name, ScopedTessla.Overloads())
      entries += name -> (existingOverloads + overload)
    }

    override def toString = {
      entries.map {
        case (name, overloads) =>
          s"def $name = {\n$overloads\n}"
      }.mkString("\n")
    }
  }

  case class Overloads(byArity: Map[Int, Seq[Overload]] = Map()) {
    def +(overload: Overload) = {
      val existing = byArity.getOrElse(overload.arity, Seq())
      // TODO: error checking
      Overloads(byArity + (overload.arity -> (overload +: existing)))
    }

    override def toString = byArity.flatMap(_._2).mkString("\n")
  }

  case class Overload(parameters: Seq[(Identifier, Option[Type])], body: Expression, loc: Location) {
    lazy val arity = parameters.size

    override def toString = parameters.mkString("(", ", ", s") => $body")
  }

  case class Specification(globalScope: Scope, outStreams: Seq[OutStream], outAllLocation: Option[Location]) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"$globalScope\n${outStreams.mkString("\n")}$outAllString"
    }

    def addGlobalOverload(name: String, overload: Overload): Unit = {
      globalScope.addOverload(name, overload)
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

  case class Variable(id: Identifier) extends Expression {
    override def toString = id.toString
    override def loc = id.loc
  }

  case class MacroCall(macroID: Identifier, args: Seq[Argument], loc: Location) extends Expression {
    override def toString = args.mkString(s"$macroID(", ", ", ")")
  }

  case class Block(scope: Scope, expression: Expression, loc: Location) extends Expression {
    override def toString = s"{\n$scope\n$expression\n}"
  }

  case class Literal(value: LiteralValue, loc: Location) extends Expression {
    override def toString = value.toString
  }

  type LiteralValue = Tessla.LiteralValue

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

  type Type = Tessla.Type
}
package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.MultipleDefinitionsError

import scala.collection.mutable

object ScopedTessla {
  case class Specification(globalScope: Scope, outStreams: Seq[OutStream], outAllLocation: Option[Location]) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"$globalScope\n${outStreams.mkString("\n")}$outAllString"
    }

    def addGlobalVariable(id: Identifier, expression: Expression): Unit = {
      globalScope.addVariable(id, expression)
    }

    def outAll = outAllLocation.isDefined
  }

  type Identifier = Tessla.Identifier

  class Scope(val parent: Option[Scope] = None) {
    case class Var(id: Identifier, expression: Expression)

    val variables = mutable.Map[String, Var]()

    val macros = mutable.Map[String, Overloads]()

    def addOverload(name: String, overload: Overload): Unit = {
      val existingOverloads = macros.getOrElse(name, ScopedTessla.Overloads())
      macros += name -> (existingOverloads + overload)
    }

    def addVariable(id: Identifier, expression: Expression): Unit = {
      variables.get(id.name).foreach { existing =>
        throw MultipleDefinitionsError(id, existing.id.loc)
      }
      variables(id.name) = Var(id, expression)
    }

    def resolveVariable(name: String): Option[Expression] = {
      variables.get(name).map(_.expression).orElse(parent.flatMap(_.resolveVariable(name)))
    }

    def getMacroOverloads(name: String): Overloads = {
      val parentOverloads = parent.map(_.getMacroOverloads(name)).getOrElse(Overloads())
      parentOverloads ++ macros.getOrElse(name, Overloads())
    }

    override def toString = {
      s"-- Scope $hashCode\n" + variables.map {
        case (name, Var(_, exp)) =>
          s"def $name = $exp"
      }.mkString("\n") + "\n" + macros.map {
        case (name, overloads) =>
          s"def $name = {\n$overloads\n}"
      }.mkString("\n") + parent.map(p => s"\n-- Parent = Scope ${p.hashCode}").getOrElse("")
    }
  }

  case class Overloads(byArity: Map[Int, Seq[Overload]] = Map()) {
    def +(overload: Overload) = {
      val existing = byArity.getOrElse(overload.arity, Seq())
      Overloads(byArity + (overload.arity -> (overload +: existing)))
    }

    def ++(overloads: Overloads) = {
      Overloads(overloads.byArity.foldLeft(byArity) {
        case (acc, (arity, entries)) =>
          acc + (arity -> (byArity.getOrElse(arity, Seq()) ++ entries))
      })
    }

    def getByArity(arity: Int) = byArity.getOrElse(arity, Seq())

    override def toString = byArity.flatMap(_._2).mkString("\n")
  }

  case class Overload(typeParameters: Seq[Identifier], parameters: Seq[Parameter], definition: Definition) {
    lazy val arity = parameters.size

    override def toString = parameters.mkString("(", ", ", s") => $definition")
  }

  case class OutStream(expr: Expression, idOpt: Option[Identifier], loc: Location) {
    def name = idOpt.map(_.name).getOrElse(expr.toString)
    override def toString = s"out $expr as $name"
  }

  sealed abstract class Expression {
    def loc: Location
  }

  sealed abstract class Definition

  case class Macro(typeParameters: Seq[Identifier],
                   parameters: Seq[Parameter],
                   scope: Scope,
                   returnType: Option[Type],
                   body: Expression,
                   loc: Location) extends Definition {
    override def toString = {
      s"$scope\n$body"
    }
  }

  sealed abstract class BuiltInOperator extends Definition {
    def loc = Location.builtIn
  }

  case object Default extends BuiltInOperator
  case object DefaultFrom extends BuiltInOperator
  case object Last extends BuiltInOperator
  case object Time extends BuiltInOperator
  case object DelayedLast extends BuiltInOperator
  case class PrimitiveOperator(op: PrimitiveOperators.PrimitiveOperator) extends BuiltInOperator {
    override def toString = op.toString
  }


  case class InStream(id: Identifier, streamType: Type, loc: Location) extends Expression {
    override def toString = s"in $id: $streamType"
  }

  case class Parameter(param: Tessla.Parameter) extends Expression {
    def parameterType = param.parameterType

    def id = param.id

    override def loc = param.loc

    override def toString = parameterType match {
      case Some(t) => s"param $id: $t"
      case None => s"param $id"
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
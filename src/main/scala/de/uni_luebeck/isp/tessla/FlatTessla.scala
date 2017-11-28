package de.uni_luebeck.isp.tessla

import scala.collection.mutable

object FlatTessla extends HasUniqueIdentifiers {
  case class Specification(globalScope: Scope, outStreams: Seq[OutStream], outAllLocation: Option[Location]) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"$globalScope\n${outStreams.mkString("\n")}$outAllString"
    }

    def outAll = outAllLocation.isDefined
  }

  case class VariableEntry(expression: Expression, typeOpt: Option[Type])

  class Scope(parent: Option[Scope]) {
    val variables = mutable.Map[Identifier, VariableEntry]()

    def addVariable(id: Identifier, entry: VariableEntry): Unit = {
      require(!variables.contains(id), "addVariable should only ever be called with a fresh identifier!")
      variables(id) = entry
    }

    def resolveVariable(id: Identifier): Option[VariableEntry] = {
      variables.get(id).orElse(parent.flatMap(_.resolveVariable(id)))
    }

    override def toString = {
      s"-- Scope $hashCode\n" + variables.map {
        case (name, entry) =>
          val typeAnnotation = entry.typeOpt.map(" : " + _).getOrElse("")
          s"def $name$typeAnnotation = ${entry.expression}"
      }.mkString("\n") + parent.map(p => s"\n-- Parent = Scope ${p.hashCode}").getOrElse("")
    }
  }

  case class OutStream(expr: Expression, name: String, loc: Location) {
    override def toString = s"out $expr as $name"
  }

  sealed abstract class Expression {
    def loc: Location
  }

  case class Macro(typeParameters: Seq[Identifier],
                   parameters: Seq[Parameter],
                   scope: Scope,
                   returnType: Option[Type],
                   body: Expression,
                   loc: Location) extends Expression {
    override def toString = {
      s"[${typeParameters.mkString(", ")}](${parameters.mkString(", ")}) => {\n$scope\n$body\n}"
    }
  }

  sealed abstract class BuiltInOperator extends Expression {
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

  case class InStream(name: String, streamType: Type, loc: Location) extends Expression {
    override def toString = s"in $name: $streamType"
  }

  case class Parameter(param: Tessla.Parameter) extends Expression {
    def parameterType = param.parameterType

    def name = param.id.name

    def nameWithLoc = param.id

    override def loc = param.loc

    override def toString = parameterType match {
      case Some(t) => s"param $name: $t"
      case None => s"param $name"
    }
  }

  case class Variable(id: Identifier, loc: Location) extends Expression {
    override def toString = id.toString
  }

  case class MacroCall(macroID: Identifier, args: Seq[Argument], loc: Location) extends Expression {
    override def toString = args.mkString(s"$macroID(", ", ", ")")
  }

  case class Literal(value: LiteralValue, loc: Location) extends Expression {
    override def toString = value.toString
  }

  type LiteralValue = Tessla.LiteralValue

  abstract class Argument {
    def loc: Location
  }

  case class PositionalArgument(id: Identifier, loc: Location) extends Argument {
    override def toString = id.toString
  }

  case class NamedArgument(name: String, id: Identifier, loc: Location) extends Argument {
    override def toString = s"$name = $id"
  }

  type Type = Tessla.Type
}
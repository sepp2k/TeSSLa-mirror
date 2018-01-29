package de.uni_luebeck.isp.tessla

import scala.collection.mutable

abstract class FlatTessla {
  import FlatTessla.{OutStream, Argument, BuiltIn}

  case class Specification(globalScope: Scope, outStreams: Seq[OutStream], outAllLocation: Option[Location]) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"$globalScope\n${outStreams.mkString("\n")}$outAllString"
    }

    def outAll = outAllLocation.isDefined
  }

  type Type
  type TypeAnnotation
  def typeAnnotationToString(typeAnnotation: TypeAnnotation): String

  type Identifier <: HasUniqueIdentifiers#Identifier

  case class VariableEntry(expression: Expression, typeInfo: TypeAnnotation)

  class Scope(val parent: Option[Scope]) {
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
          val typeAnnotation = typeAnnotationToString(entry.typeInfo)
          s"def $name$typeAnnotation = ${entry.expression}"
      }.mkString("\n") + parent.map(p => s"\n-- Parent = Scope ${p.hashCode}\n").getOrElse("") ++ "-- /Scope"
    }
  }

  sealed abstract class Expression {
    def loc: Location
  }

  case class Macro(typeParameters: Seq[Identifier],
                   parameters: Seq[Parameter],
                   scope: Scope,
                   returnType: TypeAnnotation,
                   body: Expression,
                   loc: Location) extends Expression {
    override def toString = {
      s"[${typeParameters.mkString(", ")}](${parameters.mkString(", ")}) => {\n$scope\n$body\n}"
    }
  }

  case class BuiltInOperator(builtIn: BuiltIn) extends Expression {
    override def loc = Location.builtIn
  }

  case object Nil extends Expression {
    override def toString = "nil"
    override def loc = Location.builtIn
  }

  case class InputStream(name: String, streamType: Type, loc: Location) extends Expression {
    override def toString = s"in $name: $streamType"
  }

  case class Parameter(param: Tessla.Parameter, id: Identifier) extends Expression {
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
}

object FlatTessla extends FlatTessla with HasUniqueIdentifiers {
  type Type = Tessla.Type
  type TypeAnnotation = Option[Type]

  override def typeAnnotationToString(typeAnnotation: TypeAnnotation) = typeAnnotation match {
    case None => ""
    case Some(t) => s" : $t"
  }

  case class OutStream(id: Identifier, name: String, loc: Location) {
    override def toString = s"out $id as $name"
  }

  sealed abstract class BuiltIn
  case object Default extends BuiltIn
  case object DefaultFrom extends BuiltIn
  case object Last extends BuiltIn
  case object Time extends BuiltIn
  case object DelayedLast extends BuiltIn
  case object Const extends BuiltIn
  case object Merge extends BuiltIn
  case class PrimitiveOperator(op: PrimitiveOperators.PrimitiveOperator) extends BuiltIn {
    override def toString = op.toString
  }

  sealed abstract class Argument {
    def loc: Location
  }

  case class PositionalArgument(id: Identifier, loc: Location) extends Argument {
    override def toString = id.toString
  }

  case class NamedArgument(name: String, id: Identifier, loc: Location) extends Argument {
    override def toString = s"$name = $id"
  }
}
package de.uni_luebeck.isp.tessla

import scala.collection.mutable

abstract class FlatTessla extends HasUniqueIdentifiers {
  case class Specification(globalScope: Scope, outStreams: Seq[OutStream], outAllLocation: Option[Location],
                           stdlibNames: Map[String, Identifier]) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"$globalScope\n${outStreams.mkString("\n")}$outAllString"
    }

    def outAll = outAllLocation.isDefined
  }

  type TypeAnnotation
  def typeAnnotationToString(typeAnnotation: TypeAnnotation): String

  case class VariableEntry(id: Identifier, expression: Expression, typeInfo: TypeAnnotation, loc: Location)
  case class TypeEntry(id: Identifier, arity: Int, typeConstructor: Seq[Type] => Type, loc: Location)

  class Scope(val parent: Option[Scope]) {
    val variables = mutable.Map[Identifier, VariableEntry]()
    val types = mutable.Map[Identifier, TypeEntry]()

    def addVariable(entry: VariableEntry): Unit = {
      variables(entry.id) = entry
    }

    def addType(entry: TypeEntry): Unit = {
      types(entry.id) = entry
    }

    def resolveVariable(id: Identifier): Option[VariableEntry] = {
      variables.get(id).orElse(parent.flatMap(_.resolveVariable(id)))
    }

    def resolveType(id: Identifier): Option[TypeEntry] = {
      types.get(id).orElse(parent.flatMap(_.resolveType(id)))
    }

    override def toString = {
      s"-- Scope $hashCode\n" + variables.map {
        case (id, entry) =>
          val typeAnnotation = typeAnnotationToString(entry.typeInfo)
          s"def $id$typeAnnotation = ${entry.expression}"
      }.mkString("\n") + types.map {
        case (id, entry) =>
          s"type $id[${entry.arity}] = ${entry.id}"
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
                   headerLoc: Location,
                   body: Expression,
                   loc: Location,
                   isLiftable: Boolean) extends Expression {
    override def toString = {
      val annotationString = if (isLiftable) "@liftable " else ""
      s"$annotationString[${typeParameters.mkString(", ")}](${parameters.mkString(", ")}) => {\n$scope\n$body\n}"
    }
  }

  case class BuiltInOperator(builtIn: BuiltIn) extends Expression {
    override def loc = Location.builtIn
  }

  case class InputStream(name: String, streamType: Type, typeLoc: Location, loc: Location) extends Expression {
    override def toString = s"in $name: $streamType"
  }

  case class Parameter(param: Tessla.Parameter, parameterType: Type, id: Identifier) extends Expression {
    def name = param.id.name

    def nameWithLoc = param.id

    override def loc = param.loc

    override def toString = s"param $name: $parameterType"
  }

  case class Variable(id: Identifier, loc: Location) extends Expression {
    override def toString = id.toString
  }

  case class MacroCall(macroID: Identifier, macroLoc: Location, typeArgs: Seq[Type], args: Seq[Argument], loc: Location) extends Expression {
    override def toString = args.mkString(s"$macroID(", ", ", ")")
  }

  case class IdLoc(id: Identifier, loc: Location)

  case class StaticIfThenElse(condition: IdLoc, thenCase: IdLoc, elseCase: IdLoc, loc: Location) extends Expression {
    override def toString = s"if $condition then $thenCase else $elseCase"
  }

  case class ObjectLiteral(members: Map[String, IdLoc], loc: Location) extends Expression

  case class MemberAccess(receiver: IdLoc, member: String, memberLoc: Location, loc: Location) extends Expression

  case class Literal(value: LiteralValue, loc: Location) extends Expression {
    override def toString = value.toString
  }

  type LiteralValue = Tessla.LiteralValue

  sealed abstract class Type {
    def isValueType: Boolean
  }

  case object IntType extends Type {
    override def isValueType = true

    override def toString = "Int"
  }

  case object FloatType extends Type {
    override def isValueType = true

    override def toString = "Float"
  }

  case object StringType extends Type {
    override def isValueType = true

    override def toString = "String"
  }

  case object BoolType extends Type {
    override def isValueType = true

    override def toString = "Bool"
  }

  case class OptionType(elementType: Type) extends Type {
    override def isValueType = true

    override def toString = s"Option[$elementType]"
  }

  case object CtfType extends Type {
    override def isValueType = true

    override def toString = "CTF"
  }

  case class MapType(keyType: Type, valueType: Type) extends Type {
    override def isValueType = true

    override def toString = s"Map[$keyType, $valueType]"
  }

  case class SetType(elementType: Type) extends Type {
    override def isValueType = true

    override def toString = s"Set[$elementType]"
  }

  case class ListType(elementType: Type) extends Type {
    override def isValueType = true

    override def toString = s"List[$elementType]"
  }

  case class StreamType(elementType: Type) extends Type {
    override def isValueType = false

    override def toString = s"Events[$elementType]"
  }

  case class ObjectType(memberTypes: Map[String, Type], isOpen: Boolean) extends Type {
    override def isValueType = memberTypes.values.forall(_.isValueType)

    override def toString = {
      val tupleKeys = (1 to memberTypes.keys.size).map(i => s"_$i")
      if (memberTypes.keys.toSet == tupleKeys.toSet && !isOpen) {
        memberTypes.mkString("(", ", ", ")")
      } else {
        var members = memberTypes.map { case (name, t) => s"$name: $t" }.toSeq
        if (isOpen) {
          members :+= "..."
        }
        members.mkString("{", ", ", "}")
      }
    }
  }

  case class FunctionType(typeParameters: Seq[Identifier], parameterTypes: Seq[Type], returnType: Type,
                          isLiftable: Boolean) extends Type {
    override def isValueType = false

    override def toString = {
      val annotationString = if (isLiftable) "@liftable " else ""
      val typeParamString = typeParameters.map(id => id.nameOpt.getOrElse(id.toString)).mkString(", ")
      s"$annotationString[$typeParamString](${parameterTypes.mkString(",")}) => $returnType"
    }
  }

  case class TypeParameter(id: Identifier, loc: Location) extends Type {
    // TODO: Handle this properly via constraints instead of restricting all type variables to only stand for
    //       value types.
    //       This will entail removing the isValueType method and instead handling this in the type checker
    //       where the type environment is available
    override def isValueType = true

    override def toString = id.nameOpt.getOrElse(id.toString)

    override def equals(other: Any) = other match {
      case tvar: TypeParameter => id == tvar.id
      case _ => false
    }

    override def hashCode() = id.hashCode()
  }

  case class OutStream(id: Identifier, nameOpt: Option[String], loc: Location) {
    override def toString = nameOpt match {
      case Some(name) => s"out $id as $name"
      case None => s"print $id"
    }
  }

  sealed abstract class Argument {
    def loc: Location
    def id: Identifier
  }

  case class PositionalArgument(id: Identifier, loc: Location) extends Argument {
    override def toString = id.toString
  }

  case class NamedArgument(name: String, idLoc: IdLoc, loc: Location) extends Argument {
    def id = idLoc.id

    override def toString = s"$name = $id"
  }
}

object FlatTessla extends FlatTessla {
  type TypeAnnotation = Option[Type]

  override def typeAnnotationToString(typeAnnotation: TypeAnnotation) = typeAnnotation match {
    case None => ""
    case Some(t) => s" : $t"
  }

}
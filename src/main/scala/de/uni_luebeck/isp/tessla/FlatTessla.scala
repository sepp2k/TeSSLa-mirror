package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.InternalError
import de.uni_luebeck.isp.tessla.util.mapValues
import scala.collection.mutable

abstract class FlatTessla extends HasUniqueIdentifiers {
  case class Specification(globalDefs: Definitions, outStreams: Seq[OutStream], outAllLocation: Option[Location],
                           globalNames: Map[String, Identifier]) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"$globalDefs\n${outStreams.mkString("\n")}$outAllString"
    }

    def outAll = outAllLocation.isDefined

    def lookupID(qname: String*): Option[Identifier] = lookupID(globalNames, qname)

    private def lookupID(names: Map[String, Identifier], qname: Seq[String]): Option[Identifier] = qname match {
      case Seq() => throw InternalError("Called lookup ID with empty list")
      case Seq(name) => names.get(name)
      case Seq(name, rest @ _*) => names.get(name).flatMap { id =>
        globalDefs.resolveVariable(id).flatMap {
          case VariableEntry(_, mod: ObjectLiteral, _, _, _) =>
            lookupID(mapValues(mod.members)(_.id), rest)
          case _ => None
        }
      }
    }
  }

  type TypeAnnotation
  def typeAnnotationToString(typeAnnotation: TypeAnnotation): String

  case class Annotation(name: String, arguments: Map[String, Tessla.ConstantExpression], loc: Location)

  case class VariableEntry(id: Identifier, expression: Expression, typeInfo: TypeAnnotation,
                           annotations: Seq[Annotation], loc: Location)
  case class TypeEntry(id: Identifier, arity: Int, typeConstructor: Seq[Type] => Type, loc: Location)

  class Definitions(val parent: Option[Definitions]) {
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
      s"-- Definitions $hashCode\n" + variables.map {
        case (id, entry) =>
          val typeAnnotation = typeAnnotationToString(entry.typeInfo)
          s"def $id$typeAnnotation = ${entry.expression}"
      }.mkString("\n") + types.map {
        case (id, entry) =>
          s"type $id[${entry.arity}] = ${entry.id}"
      }.mkString("\n") + parent.map(p => s"\n-- Parent = Definitions ${p.hashCode}\n").getOrElse("") ++ "-- /Definitions"
    }
  }

  sealed abstract class Expression {
    def loc: Location
  }

  case class Macro(typeParameters: Seq[Identifier],
                   parameters: Seq[Parameter],
                   body: Definitions,
                   returnType: TypeAnnotation,
                   headerLoc: Location,
                   result: Expression,
                   loc: Location,
                   isLiftable: Boolean) extends Expression {
    override def toString = {
      val annotationString = if (isLiftable) "@liftable " else ""
      s"$annotationString[${typeParameters.mkString(", ")}](${parameters.mkString(", ")}) => {\n$body\n$result\n}"
    }
  }

  case class BuiltInOperator(name: String,
                             typeParameters: Seq[Identifier],
                             parameters: Seq[Parameter],
                             referenceImplementation: Option[Identifier],
                             loc: Location) extends Expression {
    override def toString = s"__builtin__($name)"
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
    def isStreamType = false
    def isLiftableFunctionType = false
  }

  case class BuiltInType(name: String, typeArgs: Seq[Type]) extends Type {
    //TODO: Make this less stupid
    override def isValueType = name != "Events"

    override def isStreamType = name == "Events"

    override def toString = {
      if (typeArgs.isEmpty) name
      else typeArgs.mkString(s"$name[", ", ", "]")
    }
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

    override def isLiftableFunctionType = isLiftable

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
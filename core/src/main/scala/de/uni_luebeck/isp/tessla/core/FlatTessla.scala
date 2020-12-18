/*
 * Copyright 2020 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.core

import scala.collection.mutable

/**
 * A flat representation of TeSSLa code.
 *
  * All the definitions are grouped in an instance of [[Definitions]], which is potentially composed of more instances,
 * modelling the scopes of definitions.
 *
  * Entries to definitions are either [[TypeEntry]] for type definitions, or [[VariableEntry]] for all other kinds of
 * definitions.
 *
  * @see [[Flattener]]
 */

abstract class FlatTessla extends HasUniqueIdentifiers {
  case class Specification(
    annotations: Seq[Annotation],
    globalDefs: Definitions,
    outStreams: Seq[OutStream],
    outAll: Option[OutAll],
    globalNames: Map[String, Identifier]
  ) {
    override def toString: String = {
      val outAllString = outAll.map(_.toString).getOrElse("")
      s"${annotations.mkString("\n")}\n$globalDefs\n${outStreams.mkString("\n")}$outAllString"
    }

    def hasOutAll: Boolean = outAll.isDefined
  }

  type TypeAnnotation
  def typeAnnotationToString(typeAnnotation: TypeAnnotation): String

  case class Annotation(
    id: Identifier,
    arguments: Seq[Argument],
    loc: Location
  )

  case class VariableEntry(
    id: Identifier,
    expression: Expression,
    typeInfo: TypeAnnotation,
    annotations: Seq[Annotation],
    parens: Boolean,
    loc: Location
  )

  case class TypeEntry(
    id: Identifier,
    arity: Int,
    typeConstructor: Seq[Type] => Type,
    loc: Location
  )

  case class AnnotationEntry(
    id: Identifier,
    parameters: Seq[FlatTessla.Parameter],
    global: Boolean,
    loc: Location
  )

  class Definitions(val parent: Option[Definitions]) {
    val variables = mutable.Map[Identifier, VariableEntry]()
    val types = mutable.Map[Identifier, TypeEntry]()
    val annotations = mutable.Map[Identifier, AnnotationEntry]()

    def addVariable(entry: VariableEntry): Unit = {
      variables(entry.id) = entry
    }

    def addType(entry: TypeEntry): Unit = {
      types(entry.id) = entry
    }

    def addAnnotation(entry: AnnotationEntry): Unit = {
      annotations(entry.id) = entry
    }

    def resolveVariable(id: Identifier): Option[VariableEntry] = {
      variables.get(id).orElse(parent.flatMap(_.resolveVariable(id)))
    }

    def resolveType(id: Identifier): Option[TypeEntry] = {
      types.get(id).orElse(parent.flatMap(_.resolveType(id)))
    }

    def resolveAnnotation(id: Identifier): Option[AnnotationEntry] = {
      annotations.get(id).orElse(parent.flatMap(_.resolveAnnotation(id)))
    }

    override def toString: String = {
      s"-- Definitions $hashCode\n" +
        variables
          .map {
            case (id, entry) =>
              val typeAnnotation = typeAnnotationToString(entry.typeInfo)
              val annotations = entry.annotations.mkString("\n")
              s"$annotations\ndef $id$typeAnnotation = ${entry.expression}"
          }
          .mkString("\n") +
        types
          .map {
            case (id, entry) =>
              s"type $id[${entry.arity}] = ${entry.id}"
          }
          .mkString("\n") +
        annotations
          .map {
            case (id, entry) =>
              s"def @$id(${entry.parameters.mkString(", ")}) = ${entry.id}"
          }
          .mkString("\n") +
        parent
          .map(p => s"\n-- Parent = Definitions ${p.hashCode}\n")
          .getOrElse("") ++ "-- /Definitions"
    }
  }

  sealed abstract class Expression {
    def loc: Location
  }

  case class Macro(
    typeParameters: Seq[Identifier],
    parameters: Seq[(Option[TesslaAST.RuntimeEvaluation], Parameter)],
    body: Definitions,
    returnType: TypeAnnotation,
    headerLoc: Location,
    result: IdLoc,
    loc: Location,
    isLiftable: Boolean
  ) extends Expression {
    override def toString: String = {
      val liftable = if (isLiftable) "liftable " else ""
      s"$liftable[${typeParameters.mkString(", ")}](${parameters.mkString(", ")}) => {\n$body\n$result\n}"
    }
  }

  case class Extern(
    name: String,
    typeParameters: Seq[Identifier],
    parameters: Seq[(Option[TesslaAST.RuntimeEvaluation], Parameter)],
    referenceImplementation: Option[Identifier],
    loc: Location
  ) extends Expression {
    override def toString = s"extern($name)"
  }

  case class InputStream(name: String, streamType: Type, typeLoc: Location, loc: Location) extends Expression {
    override def toString = s"in $name: $streamType"
  }

  case class Parameter(param: Tessla.Parameter, parameterType: Type, id: Identifier) extends Expression {
    def name: String = param.id.name

    def nameWithLoc: Tessla.Identifier = param.id

    def idLoc: IdLoc = IdLoc(id, loc)

    override def loc: Location = param.loc

    override def toString = s"param $name: $parameterType"
  }

  case class Variable(id: Identifier, loc: Location) extends Expression {
    override def toString: String = id.toString
  }

  case class MacroCall(
    macroID: Identifier,
    macroLoc: Location,
    typeArgs: Seq[Type],
    args: Seq[Argument],
    loc: Location
  ) extends Expression {
    override def toString: String = args.mkString(s"$macroID(", ", ", ")")
  }

  case class IdLoc(id: Identifier, loc: Location)

  case class ObjectLiteral(members: Map[String, IdLoc], loc: Location) extends Expression

  case class MemberAccess(receiver: IdLoc, member: String, memberLoc: Location, loc: Location) extends Expression

  case class Literal(value: LiteralValue, loc: Location) extends Expression {
    override def toString: String = value.toString
  }

  type LiteralValue = Tessla.LiteralValue

  sealed abstract class Type {
    def isValueType: Boolean
    def isStreamType = false
    def isLiftableFunctionType = false
  }

  case class BuiltInType(name: String, typeArgs: Seq[Type]) extends Type {
    //TODO: Make this less stupid
    override def isValueType: Boolean = name != "Events"

    override def isStreamType: Boolean = name == "Events"

    override def toString: String = {
      if (typeArgs.isEmpty) name
      else typeArgs.mkString(s"$name[", ", ", "]")
    }
  }

  case class ObjectType(memberTypes: Map[String, Type]) extends Type {
    override def isValueType: Boolean = memberTypes.values.forall(_.isValueType)

    override def toString: String = {
      val tupleKeys = (1 to memberTypes.keys.size).map(i => s"_$i")
      if (memberTypes.keys.toSet == tupleKeys.toSet) {
        memberTypes.toList.sortBy(_._1).map(_._2).mkString("(", ", ", ")")
      } else {
        val members = memberTypes.map { case (name, t) => s"$name: $t" }.toSeq
        members.mkString("{", ", ", "}")
      }
    }
  }

  case class FunctionType(
    typeParameters: Seq[Identifier],
    parameterTypes: Seq[(Option[TesslaAST.RuntimeEvaluation], Type)],
    returnType: Type,
    isLiftable: Boolean
  ) extends Type {
    override def isValueType = false

    override def isLiftableFunctionType: Boolean = isLiftable

    override def toString: String = {
      val liftableString = if (isLiftable) "liftable " else ""
      val typeParamString =
        typeParameters.map(id => id.nameOpt.getOrElse(id.toString)).mkString(", ")
      def paramString = parameterTypes
        .map {
          case (e, t) => s"${e.map(_.toString + " ").getOrElse("")}$t"
        }
        .mkString(", ")
      s"$liftableString[$typeParamString]($paramString) => $returnType"
    }
  }

  case class TypeParameter(id: Identifier, loc: Location) extends Type {
    // TODO: Handle this properly via constraints instead of restricting all type variables to only stand for
    //       value types.
    //       This will entail removing the isValueType method and instead handling this in the type checker
    //       where the type environment is available
    override def isValueType = true

    override def toString: String = id.nameOpt.getOrElse(id.toString)

    override def equals(other: Any): Boolean = other match {
      case tvar: TypeParameter => id == tvar.id
      case _                   => false
    }

    override def hashCode(): Int = id.hashCode()
  }

  case class OutStream(id: Identifier, name: String, annotations: Seq[Annotation], loc: Location) {
    override def toString = s"out $id as $name"
  }

  case class OutAll(annotations: Seq[Annotation], loc: Location) {
    override def toString = "out *"
  }

  sealed abstract class Argument {
    def loc: Location
    def id: Identifier
  }

  case class PositionalArgument(id: Identifier, loc: Location) extends Argument {
    override def toString: String = id.toString
  }

  case class NamedArgument(name: String, idLoc: IdLoc, loc: Location) extends Argument {
    def id: Identifier = idLoc.id

    override def toString = s"$name = $id"
  }
}

object FlatTessla extends FlatTessla {
  type TypeAnnotation = Option[Type]

  override def typeAnnotationToString(typeAnnotation: TypeAnnotation): String = typeAnnotation match {
    case None    => ""
    case Some(t) => s" : $t"
  }

}

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

import cats._
import cats.data.Ior
import cats.implicits._

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.Try
import de.uni_luebeck.isp.tessla.core.util._

/**
 * A parametrized data structure for Tessla ASTs. This allows to define different ASTs used in the translation pipeline
 * using this structure as foundation.
 */
object TesslaAST {

  /**
   * A container for printing options, defining how expressions should be printed.
   */
  case class PrintOptions(
    defTypes: Boolean = true,
    expTypes: Boolean = false,
    refTypes: Boolean = false,
    paramTypes: Boolean = true,
    locations: Boolean = false
  )

  sealed trait CompiletimeEvaluation

  sealed trait RuntimeEvaluation extends CompiletimeEvaluation

  case object LazyEvaluation extends RuntimeEvaluation {
    override def toString = "lazy"
  }

  case object StrictEvaluation extends RuntimeEvaluation {
    override def toString = "strict"
  }

  case object ExpandEvaluation extends CompiletimeEvaluation {
    override def toString = "expand"
  }

  trait Locatable {
    def location: Location

    def withLocation(s: Boolean => String, options: PrintOptions, mayNeedParens: Boolean) =
      printWithLocation(s, options, mayNeedParens, location)
  }

  /**
   * Untyped Tessla, where expressions are not fully typed yet, and thus not necessarily have a type info attached
   * to them.
   */
  object Untyped extends WithAnnotations[Option] {
    override type DefinitionExpression = ExpressionArg

    override type Evaluation = CompiletimeEvaluation

    override def withTypeAnnotation(
      s: Boolean => String,
      tpe: Option[Type],
      types: Boolean,
      mayNeedParens: Boolean
    ): String =
      tpe.filter(_ => types).map(_ => s"${s(true)}").getOrElse(s(mayNeedParens))

  }

  /**
   * Typed Tessla. Here, all expressions are accompanied by type info.
   */
  object Typed extends WithAnnotations[Id] {
    override type DefinitionExpression = ExpressionArg

    override type Evaluation = CompiletimeEvaluation

    override def withTypeAnnotation(s: Boolean => String, tpe: Type, types: Boolean, mayNeedParens: Boolean): String =
      if (types) s"${s(true)}: $tpe" else s(mayNeedParens)

  }

  /**
   * Core Tessla. This is the AST used as final result of the compiler.
   */
  object Core extends WithAnnotations[Id] {
    override type DefinitionExpression = Expression

    override type Evaluation = RuntimeEvaluation

    override def withTypeAnnotation(s: Boolean => String, tpe: Type, types: Boolean, mayNeedParens: Boolean): String =
      withParens(if (types) s"${s(true)}: $tpe" else s(mayNeedParens), types && mayNeedParens)

    override def getOutputName(annotations: Annotations): Option[String] = Try(
      annotations("$name")(0).asInstanceOf[Core.StringLiteralExpression].value
    ).toOption

  }

  /**
   * An abstract class describing a Tessla AST with annotations, providing a generalized way of printing annotations
   * without restricting the generic TesslaAST itself on a specific annotation type.
   */
  abstract class WithAnnotations[T[_]: CommutativeApplicative] extends TesslaAST[T] {
    override type Annotations = Map[String, ArraySeq[ExpressionArg]]

    override def printAnnotations(annotations: Annotations, global: Boolean, options: PrintOptions): List[String] = {
      val prefix = if (global) "@@" else "@"
      annotations.toList
        .sortBy(_._1)
        .flatMap {
          case (name, argslists) => argslists.map((name, _))
        }
        .map {
          case (name, exp) =>
            val expStr = exp match {
              case rec: RecordConstructorExpression if rec.entries.isEmpty => ""
              case e                                                       => e.print(options, mayNeedParens = false)
            }
            s"$prefix$name($expStr)"
        }
    }
  }

  def printWithLocation(s: Boolean => String, options: PrintOptions, mayNeedParens: Boolean, location: Location) =
    withParens(
      if (options.locations && location != Location.unknown) s"${s(true)} @ $location"
      else s(mayNeedParens),
      mayNeedParens && options.locations
    )

  def withParens(s: String, addParens: Boolean): String = if (addParens) s"(${s})" else s

  def printRecord[A](
    entries: Map[String, A],
    sep: String,
    str: A => String,
    unit: String,
    options: PrintOptions = PrintOptions(),
    strName: (String, A) => String = (s: String, _: A) => s
  ): String = if (entries.isEmpty) unit
  else {
    val tuplified = entries.flatMap {
      case (key, value) =>
        if (key.matches("_\\d+")) {
          Some(key.substring(1).toInt -> value)
        } else None
    }
    if (tuplified.size == entries.size && tuplified.keys.min == 1 && tuplified.keys.max == entries.size) {
      val sorted = tuplified.toList.sortBy(_._1).map(x => str(x._2))
      s"(${sorted.mkString(", ")})"
    } else {
      val sorted = entries.toList.sortBy(_._1).map {
        case (k, v) =>
          strName(k, v) + sep + str(v)
      }
      s"{${sorted.mkString(", ")}}"
    }
  }

}

/**
 * A parametrized data structure for Tessla ASTs. This allows to define different ASTs used in the translation pipeline
 * using this structure as foundation.
 */
abstract class TesslaAST[TypeAnnotation[_]: CommutativeApplicative] {

  import TesslaAST._

  def withTypeAnnotation(
    s: Boolean => String,
    tpe: TypeAnnotation[Type],
    types: Boolean,
    mayNeedParens: Boolean
  ): String

  /**
   * The type of definition expressions.
   */
  type DefinitionExpression <: ExpressionArg

  /**
   * The type of evaluation strategies.
   */
  type Evaluation >: StrictEvaluation.type

  /**
   * The type of annotations.
   */
  type Annotations

  def getOutputName(annotations: Annotations): Option[String] = None

  def printAnnotationString(annotations: Annotations, global: Boolean, options: PrintOptions): String = {
    val as = printAnnotations(annotations, global, options)
    if (as.nonEmpty) as.mkString("", "\n", "\n") else ""
  }

  def printAnnotations(annotations: Annotations, global: Boolean, options: PrintOptions): List[String] = Nil

  def printWithTypeAndLocation(
    s: Boolean => String,
    tpe: TypeAnnotation[Type],
    location: Location,
    options: PrintOptions,
    types: Boolean,
    mayNeedTypeParens: Boolean,
    mayNeedLocationParens: Boolean
  ): String =
    printWithLocation(
      b => withTypeAnnotation(s, tpe, types, b && mayNeedTypeParens),
      options,
      mayNeedLocationParens,
      location
    )

  def indent(s: String): String = s.linesIterator.map(s => "  " + s).mkString("\n") +
    (if (s.endsWith("\n")) "\n" else "")

  /**
   * Describes a specification.
   * @param annotations the annotations of this specification
   * @param in the input streams
   * @param definitions the definitions
   * @param out the output streams
   * @param maxIdentifier the numeric value of the maximum identifier
   */
  case class Specification(
    annotations: Annotations,
    in: Map[Identifier, (TypeAnnotation[Type], Annotations)],
    definitions: Map[Identifier, DefinitionExpression],
    out: List[(ExpressionRef, Annotations)],
    maxIdentifier: Long
  ) {

    def print(options: PrintOptions): String = {
      val a = printAnnotationString(annotations, global = true, options)
      val i = in
        .map {
          case (id, (typ, ann)) =>
            val annotations = printAnnotationString(ann, global = false, options)
            val inStr = printWithTypeAndLocation(
              _ => s"in $id",
              typ,
              id.location,
              options,
              types = true,
              mayNeedLocationParens = false,
              mayNeedTypeParens = false
            )

            annotations + inStr
        }
        .mkString("\n")

      val o = out
        .map {
          case (ref, ann) =>
            val annotations = printAnnotationString(ann, global = false, options)
            val outStr = printWithLocation(_ => s"out $ref", options, mayNeedParens = false, ref.location)

            annotations + outStr
        }
        .mkString("\n")

      val d = definitions
        .map {
          case (id, definition) =>
            val idStr = printWithTypeAndLocation(
              _ => id.toString,
              definition.tpe,
              id.location,
              options,
              options.defTypes,
              mayNeedTypeParens = false,
              mayNeedLocationParens = false
            )
            val defStr = definition.print(options, mayNeedParens = false)

            s"def $idStr = $defStr"
        }
        .mkString("\n")

      s"$a\n\n$i\n\n$o\n\n$d"
    }

    override def toString: String = print(PrintOptions())
  }

  // Identifiers can either have a globally unique id or locally unique name or both.
  // For TeSSLa Core only external names, i.e. input streams do not carry a id.
  // TODO: currently also generated type parameters may not carry an id, this should be fixed once all externs are declared in the standard library.
  /**
   * An identifier.
   * @param idOrName the numeric id, name, or both
   * @param location the location
   */
  final class Identifier(val idOrName: Ior[String, Long], override val location: Location = Location.unknown)
      extends Locatable {
    override def hashCode: Int = idOrName.hashCode

    override def equals(o: Any): Boolean =
      o.isInstanceOf[Identifier] && o.asInstanceOf[Identifier].idOrName.equals(idOrName)

    val fullName: String = idOrName match {
      case Ior.Left(a)    => a
      case Ior.Right(b)   => "$" + b
      case Ior.Both(a, b) => a + "$" + b
    }

    def print(options: PrintOptions, mayNeedParens: Boolean): String =
      withLocation(_ => fullName, options, mayNeedParens)

    override def toString: String = print(PrintOptions(), mayNeedParens = false)
  }

  object Identifier {
    def apply(id: Ior[String, Long], location: Location = Location.unknown) =
      new Identifier(id, location)

    def apply(id: String) = new Identifier(Ior.Left(id))

    def unapply(arg: Identifier): Option[(Option[Long], Option[String], Location)] = Some(
      (arg.idOrName.right, arg.idOrName.left, arg.location)
    )
  }

  /**
   * An expression argument. This is either a normal expression or a reference.
   */
  sealed trait ExpressionArg extends Locatable {
    def tpe: TypeAnnotation[Type]

    def print(options: PrintOptions, mayNeedParens: Boolean): String

    override def toString: String = print(PrintOptions(), mayNeedParens = false)

    def withTypeAndLocation(
      s: Boolean => String,
      options: PrintOptions,
      types: Boolean,
      mayNeedParens: Boolean
    ): String =
      printWithTypeAndLocation(
        s,
        tpe,
        location,
        options,
        types,
        mayNeedTypeParens = true,
        mayNeedLocationParens = mayNeedParens
      )
  }

  sealed trait Expression extends ExpressionArg {
    def withTypeAndLocation(s: Boolean => String, options: PrintOptions, mayNeedParens: Boolean): String =
      printWithTypeAndLocation(
        s,
        tpe,
        location,
        options,
        options.expTypes,
        mayNeedTypeParens = true,
        mayNeedLocationParens = mayNeedParens
      )
  }

  /**
   * A reference to another identifier.
   * @param id the identifier to reference
   * @param tpe the type of this expression
   * @param location the location
   */
  case class ExpressionRef(id: Identifier, tpe: TypeAnnotation[Type], location: Location = Location.unknown)
      extends ExpressionArg {

    override def print(options: PrintOptions, mayNeedParens: Boolean): String =
      withTypeAndLocation(b => id.print(options, b), options, options.refTypes, mayNeedParens)

  }

  /**
   * A function expression
   * @param typeParams the type parameters of this function
   * @param params the parameters of this function
   * @param body the body
   * @param result the result expression
   * @param location the location
   */
  case class FunctionExpression(
    typeParams: List[Identifier],
    params: List[(Identifier, Evaluation, TypeAnnotation[Type])],
    body: Map[Identifier, DefinitionExpression],
    result: ExpressionArg,
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe: TypeAnnotation[Type] = Applicative[TypeAnnotation]
      .map2(params.map(x => x._3.map(y => (x._2, y))).sequence, result.tpe) { (m, r) =>
        FunctionType(typeParams, m, r)
      }

    override def print(options: PrintOptions, mayNeedParens: Boolean): String = {
      val resultStr = result.print(options, mayNeedParens = false)

      val bodyString = indent(
        body
          .map {
            case (id, defn) =>
              val defType = withTypeAnnotation(_ => id.toString, defn.tpe, options.defTypes, mayNeedParens = false)
              val defStr = defn.print(options, mayNeedParens = false)

              s"def $defType = $defStr"
          }
          .mkString("", "\n", "\n") + resultStr
      )

      val paramsString = params
        .map {
          case (id, eval, tpe) =>
            val idStr = id.print(options, mayNeedParens = false)
            val tpeStr = if (options.paramTypes) ": " + eval.toString + " " + tpe else ""
            idStr + tpeStr
        }
        .mkString(", ")

      val typeParamsString = if (typeParams.isEmpty) "" else s"[${typeParams.mkString(", ")}]"
      withTypeAndLocation(
        b => withParens(s"$typeParamsString($paramsString) => {\n$bodyString\n}", b),
        options,
        mayNeedParens
      )
    }

  }

  /**
   * An extern expression
   * @param name the name of the extern
   * @param tpe the type of this expression
   * @param location the location
   */
  case class ExternExpression(name: String, tpe: TypeAnnotation[Type], location: Location = Location.unknown)
      extends Expression {

    override def print(options: PrintOptions, mayNeedParens: Boolean): String = name match {
      case "true" | "false" => withTypeAndLocation(_ => name, options, mayNeedParens)
      case _                => withTypeAndLocation(_ => s"""extern("$name")""", options, mayNeedParens)
    }

  }

  /**
   * An application of arguments to an applicable component.
   * @param applicable the expression to apply the arguments to
   * @param args the arguments to apply
   * @param location the location
   */
  case class ApplicationExpression(
    applicable: ExpressionArg,
    args: ArraySeq[ExpressionArg],
    location: Location = Location.unknown
  ) extends Expression {

    override def tpe: TypeAnnotation[Type] = {
      applicable.tpe.map { r =>
        val ft = r.asInstanceOf[FunctionType] // TODO: Make typesafe?
        if (ft.typeParams.nonEmpty) throw new IllegalArgumentException("Unresolved type parameters")
        ft.resultType
      }

    }

    override def print(options: PrintOptions, mayNeedParens: Boolean): String =
      withTypeAndLocation(
        _ =>
          s"${applicable.print(options, mayNeedParens = true)}(${args.map(_.print(options, mayNeedParens = false)).mkString(", ")})",
        options,
        mayNeedParens
      )
  }

  /**
   * An application of type arguments.
   * @param applicable the expression to apply the type arguments to
   * @param typeArgs the type arguments to apply
   * @param location the location
   */
  case class TypeApplicationExpression(
    applicable: ExpressionArg,
    typeArgs: List[Type],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe: TypeAnnotation[Type] = {
      applicable.tpe.map { r =>
        val ft = r.asInstanceOf[FunctionType]
        val map = ft.typeParams.zip(typeArgs).toMap
        FunctionType(
          List(),
          ft.paramTypes.map(x => (x._1, x._2.resolve(map))),
          ft.resultType.resolve(map)
        )
      }
    }

    override def print(options: PrintOptions, mayNeedParens: Boolean): String = {
      if (typeArgs.isEmpty) applicable.print(options, mayNeedParens)
      else {
        val typeArgsString = s"[${typeArgs.mkString(", ")}]"
        withTypeAndLocation(
          _ => s"${applicable.print(options, mayNeedParens = typeArgs.nonEmpty)}$typeArgsString",
          options,
          mayNeedParens
        )
      }
    }
  }

  /**
   * Creation of a record value.
   * @param entries the entries of the record, as mapping from member names to their value and location
   * @param location the location
   */
  case class RecordConstructorExpression(
    entries: Map[String, (ExpressionArg, Location)],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe: TypeAnnotation[Type] =
      mapValues(entries)(x => x._1.tpe.map(t => (t, x._2))).unorderedSequence.map { m =>
        RecordType(m)
      }

    override def print(options: PrintOptions, mayNeedParens: Boolean): String =
      withTypeAndLocation(
        _ => {
          printRecord(entries, ": ", (x: (ExpressionArg, Location)) => x._1.print(options, mayNeedParens = false), "()")
        },
        options,
        mayNeedParens
      )
  }

  /**
   * Accessor to a member of a record.
   * @param name the member to access
   * @param target the target expression to apply the accessor to
   * @param nameLocation the location of the member name
   * @param location the location
   */
  case class RecordAccessorExpression(
    name: String,
    target: ExpressionArg,
    nameLocation: Location = Location.unknown,
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe: TypeAnnotation[Type] = target.tpe.map { t =>
      t.asInstanceOf[RecordType].entries(name)._1
    }

    override def print(options: PrintOptions, mayNeedParens: Boolean): String =
      withTypeAndLocation(
        _ =>
          s"${target.print(options, mayNeedParens = true)}.${printWithLocation(_ => name, options, mayNeedParens = false, nameLocation)}",
        options,
        mayNeedParens
      )
  }

  /**
   * A string literal.
   * @param value the literal value
   * @param location the location
   */
  case class StringLiteralExpression(value: String, location: Location = Location.unknown) extends Expression {
    override def tpe: TypeAnnotation[Type] = Applicative[TypeAnnotation].pure(StringType)

    override def print(options: PrintOptions, mayNeedParens: Boolean): String =
      withTypeAndLocation(_ => s""""$value"""", options, mayNeedParens)
  }

  /**
   * A int literal.
   * @param value the literal value
   * @param location the location
   */
  case class IntLiteralExpression(value: BigInt, location: Location = Location.unknown) extends Expression {
    override def tpe: TypeAnnotation[Type] = Applicative[TypeAnnotation].pure(IntType)

    override def print(options: PrintOptions, mayNeedParens: Boolean): String =
      withTypeAndLocation(_ => value.toString, options, mayNeedParens)
  }

  /**
   * A float literal.
   * @param value the literal value
   * @param location the location
   */
  case class FloatLiteralExpression(value: Double, location: Location = Location.unknown) extends Expression {
    override def tpe: TypeAnnotation[Type] = Applicative[TypeAnnotation].pure(FloatType)

    override def print(options: PrintOptions, mayNeedParens: Boolean): String =
      withTypeAndLocation(_ => value.toString, options, mayNeedParens)
  }

  sealed trait Type extends Locatable {
    def resolve(args: Map[Identifier, Type]): Type
  }

  /**
   * A function type.
   * @param typeParams the declared type parameters
   * @param paramTypes the declared parameters
   * @param resultType the type of the resulting value
   * @param location the location
   */
  case class FunctionType(
    typeParams: List[Identifier],
    paramTypes: List[(Evaluation, Type)],
    resultType: Type,
    location: Location = Location.unknown
  ) extends Type {
    override def resolve(args: Map[Identifier, Type]): Type = {
      val tmp = args -- typeParams
      FunctionType(
        typeParams,
        paramTypes.map(x => (x._1, x._2.resolve(tmp))),
        resultType.resolve(tmp)
      )
    }

    override def toString: String = {
      val typeParamsString = if (typeParams.isEmpty) "" else s"[${typeParams.mkString(", ")}]"
      s"$typeParamsString(${paramTypes.map(x => "" + x._1 + " " + x._2).mkString(", ")}) => $resultType"
    }
  }

  case class InstantiatedType(name: String, typeArgs: List[Type], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]): Type =
      InstantiatedType(name, typeArgs.map(_.resolve(args)))

    override def toString: String =
      name + (if (typeArgs.nonEmpty) "[" + typeArgs.mkString(", ") + "]" else "")
  }

  /**
   * A type for a record expression.
   * @param entries the type of its members, as mapping from member names to their type and location
   * @param location the location
   */
  case class RecordType(entries: Map[String, (Type, Location)], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]): Type = RecordType(
      mapValues(entries)(x => (x._1.resolve(args), x._2))
    )

    override def toString: String =
      printRecord(entries, ": ", (x: (Type, Location)) => x._1.toString, "Unit")
  }

  /**
   * A type parameter.
   * @param name the name of the type
   * @param location the location
   */
  case class TypeParam(name: Identifier, location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]): Type = args.getOrElse(name, this)

    override def toString: String = name.toString
  }

  val UnitType = RecordType(Map(), Location.builtIn)

  val FloatType = InstantiatedType("Float", Nil, Location.builtIn)
  val IntType = InstantiatedType("Int", Nil, Location.builtIn)
  val StringType = InstantiatedType("String", Nil, Location.builtIn)
  val BoolType = InstantiatedType("Bool", Nil, Location.builtIn)
}

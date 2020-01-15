package de.uni_luebeck.isp.tessla

import cats._
import cats.data.Ior
import cats.implicits._

import scala.collection.immutable.ArraySeq

object TesslaAST {

  case class PrintOptions(defTypes: Boolean = true, expTypes: Boolean = false, refTypes: Boolean = false,
    paramTypes: Boolean = true, locations: Boolean = false
  )

  trait Locatable {
    def location: Location

    def withLocation(s: Boolean => String, options: PrintOptions, mayNeedBraces: Boolean) =
      withBraces(if (options.locations && location != Location.unknown) s"${s(true)} @ $location" else s(mayNeedBraces), mayNeedBraces && options.locations)
  }

  final class Name(val name: String, override val location: Location = Location.unknown) extends Locatable {
    override def hashCode() = name.hashCode

    override def equals(o: Any) = o.isInstanceOf[Name] && o.asInstanceOf[Name].name == name

    def print(options: PrintOptions, mayNeedBraces: Boolean) = withLocation(_ => name, options, mayNeedBraces)

    override def toString = print(PrintOptions(), mayNeedBraces = false)
  }

  object Name {
    def apply(name: String, location: Location = Location.unknown) = new Name(name, location)

    def unapply(arg: Name): Option[(String, Location)] = Some((arg.name, arg.location))
  }

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

  object Untyped extends TesslaAST[Option] {
    override type DefinitionExpression = ExpressionArg

    override type Evaluation = CompiletimeEvaluation


    override def withTypeAnnotation(s: Boolean => String, tpe: Option[Type], types: Boolean, mayNeedBraces: Boolean) =
      tpe.filter(_ => types).map(tpe => s"${s(true)}").getOrElse(s(mayNeedBraces))
  }

  object Typed extends TesslaAST[Id] {
    override type DefinitionExpression = ExpressionArg

    override type Evaluation = CompiletimeEvaluation

    override def withTypeAnnotation(s: Boolean => String, tpe: Type, types: Boolean, mayNeedBraces: Boolean) =
      if (types) s"${s(true)}: $tpe" else s(mayNeedBraces)
  }

  object Core extends TesslaAST[Id] {
    override type DefinitionExpression = Expression

    override type Evaluation = RuntimeEvaluation

    override def withTypeAnnotation(s: Boolean => String, tpe: Type, types: Boolean, mayNeedBraces: Boolean) =
      withBraces(if (types) s"${s(true)}: $tpe" else s(mayNeedBraces), types && mayNeedBraces)
  }

  def withBraces(s: String, addBraces: Boolean) = if (addBraces) s"(${s})" else s
}

abstract class TesslaAST[TypeAnnotation[_] : CommutativeApplicative] {

  import TesslaAST._

  def withTypeAnnotation(s: Boolean => String, tpe: TypeAnnotation[Type], types: Boolean, mayNeedBraces: Boolean): String

  type DefinitionExpression <: ExpressionArg

  type Evaluation

  case class Specification(in: Map[Identifier, (TypeAnnotation[Type], List[DefinitionExpression])],
    definitions: Map[Identifier, DefinitionExpression],
    out: List[(Identifier, Option[String], List[DefinitionExpression])],
    maxIdentifier: Long
  ) { // TODO: make optional output name an annotation
    def print(options: PrintOptions) = {
      val i = in.map(s => withTypeAnnotation(_ => s"in ${s._1}", s._2._1, types = true, mayNeedBraces = false)).mkString("\n")
      val o = out.map(x => x._2 match {
        case Some(name) => s"out $name = ${x._1}"
        case None => "out" + x._1
      }).mkString("\n")
      val d = definitions.map { x =>
        val tpeString = if (options.defTypes) s": ${x._2.tpe}" else ""
        s"def ${x._1}$tpeString = ${x._2.print(options, mayNeedBraces = false)}"
      }.mkString("\n")
      s"$i\n\n$o\n\n$d"
    }

    override def toString = print(PrintOptions())
  }

  // Identifiers can either have a globaly unique id or locally unique name or both.
  // For TeSSLa Core only external names, i.e. input streams do not carry a id.
  // TODO: currently also generated type parameters may not carry an id, this should be fixed once all externs are declared in the standard library.
  final class Identifier(val idOrName: Ior[String, Long], override val location: Location = Location.unknown) extends Locatable {
    override def hashCode = idOrName.hashCode

    override def equals(o: Any) = o.isInstanceOf[Identifier] && o.asInstanceOf[Identifier].idOrName.equals(idOrName)

    val fullName = idOrName match {
      case Ior.Left(a) => a
      case Ior.Right(b) => "$" + b
      case Ior.Both(a, b) => a + "$" + b
    }

    def print(options: PrintOptions, mayNeedBraces: Boolean) = withLocation(_ => fullName, options, mayNeedBraces)

    override def toString = print(PrintOptions(), mayNeedBraces = false)
  }

  object Identifier {
    def apply(id: Ior[String, Long], location: Location = Location.unknown) = new Identifier(id, location)

    def apply(id: String) = new Identifier(Ior.Left(id))

    def unapply(arg: Identifier): Option[(Option[Long], Option[String], Location)] = Some((arg.idOrName.right, arg.idOrName.left, arg.location))
  }


  sealed trait ExpressionArg extends Locatable {
    def tpe: TypeAnnotation[Type]

    def print(options: PrintOptions, mayNeedBraces: Boolean): String

    override def toString = print(PrintOptions(), mayNeedBraces = false)

    def withTypeAndLocation2(s: Boolean => String, options: PrintOptions, types: Boolean, mayNeedBraces: Boolean) =
      withLocation(b => withTypeAnnotation(s, tpe, types, b && mayNeedBraces), options, mayNeedBraces)
  }

  sealed trait Expression extends ExpressionArg {
    def withTypeAndLocation(s: Boolean => String, options: PrintOptions, mayNeedBraces: Boolean) =
      withLocation(b => withTypeAnnotation(s, tpe, options.expTypes, b), options, mayNeedBraces)
  }

  case class ExpressionRef(id: Identifier, tpe: TypeAnnotation[Type], location: Location = Location.unknown) extends ExpressionArg {

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation2(b => id.print(options, b), options, options.refTypes, mayNeedBraces)

  }

  def indent(s: String) = s.linesIterator.map(s => "  " + s).mkString("\n")

  case class FunctionExpression(typeParams: List[Identifier], params: List[(Identifier, Evaluation, TypeAnnotation[Type])], body: Map[Identifier, DefinitionExpression],
    result: ExpressionArg, location: Location = Location.unknown
  ) extends Expression {
    override def tpe = Applicative[TypeAnnotation].map2(params.map(x => x._3.map(y => (x._2, y))).sequence, result.tpe) { (m, r) =>
      FunctionType(typeParams, m, r)
    }

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String = {
      val bodyString = indent(body.map { x =>
        val defType = if (options.defTypes) s": ${x._2.tpe}" else ""
        s"def ${x._1}$defType = ${x._2.print(options, mayNeedBraces = false)}"
      }.mkString("", "\n", "\n") + result.print(options, mayNeedBraces = false))
      val paramsString = params.map { x =>
        x._1.print(options, mayNeedBraces = false) + (if (options.paramTypes) " " + x._2.toString + " " + x._3 else "")
      }.mkString(", ")
      val typeParamsString = if (typeParams.isEmpty) "" else s"[${typeParams.mkString(", ")}]"
      withTypeAndLocation(b => withBraces(s"$typeParamsString($paramsString) => {\n$bodyString\n}", b), options, mayNeedBraces)
    }

  }

  case class ExternExpression(typeParams: List[Identifier], params: List[(Evaluation, TypeAnnotation[Type])],
    resultType: TypeAnnotation[Type], name: String, location: Location = Location.unknown
  ) extends Expression {
    override def tpe = Applicative[TypeAnnotation].map2(params.map(x => x._2.map(y => (x._1, y))).sequence, resultType) { (m, r) =>
      FunctionType(typeParams, m, r)
    }

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => s"extern_$name", options, mayNeedBraces)
  }

  case class ApplicationExpression(applicable: ExpressionArg, args: ArraySeq[ExpressionArg],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = {
      applicable.tpe.map { r =>
        val ft = r.asInstanceOf[FunctionType] // TODO: Make typesafe?
        if (ft.typeParams.nonEmpty) throw new IllegalArgumentException("Unresolved type parameters")
        ft.resultType
      }
    }

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => s"${applicable.print(options, mayNeedBraces = true)}(${args.map(_.print(options, mayNeedBraces = false)).mkString(", ")})", options, mayNeedBraces)
  }

  case class TypeApplicationExpression(applicable: ExpressionArg, typeArgs: List[Type],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = {
      applicable.tpe.map { r =>
        val ft = r.asInstanceOf[FunctionType]
        val map = ft.typeParams.zip(typeArgs).toMap
        FunctionType(List(), ft.paramTypes.map(x => (x._1, x._2.resolve(map))), ft.resultType.resolve(map))
      }
    }

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String = {
      val typeArgsString = if (typeArgs.isEmpty) "" else s"[${typeArgs.mkString(", ")}]"
      withTypeAndLocation(_ => s"${applicable.print(options, mayNeedBraces = typeArgs.nonEmpty)}$typeArgsString", options, mayNeedBraces)
    }
  }

  // TODO: print location of entry names
  case class RecordConstructorExpression(entries: Map[Name, ExpressionArg],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = entries.view.mapValues(_.tpe).toMap.unorderedSequence.map { m =>
      RecordType(m)
    }

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => {
        val entriesString = if (entries.isEmpty) "" else
          indent(entries.map(x => x._1.toString + ": " + x._2.print(options, mayNeedBraces = false)).mkString("\n", ",\n", "\n"))
        s"{$entriesString}"
      }, options, mayNeedBraces)
  }

  // TODO: print location of accesor name
  case class RecordAccesorExpression(name: Name, target: ExpressionArg,
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = target.tpe.map { t =>
      t.asInstanceOf[RecordType].entries(name)
    }

    override def toString = "" + target + "." + name

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => s"${target.print(options, mayNeedBraces = true)}.$name", options, mayNeedBraces)
  }

  case class StringLiteralExpression(value: String, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(StringType)

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => s""""$value"""", options, mayNeedBraces)
  }

  case class IntLiteralExpression(value: BigInt, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(IntType)

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => value.toString, options, mayNeedBraces)
  }

  case class FloatLiteralExpression(value: Double, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(FloatType)

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => value.toString, options, mayNeedBraces)
  }

  sealed trait Type extends Locatable {
    def resolve(args: Map[Identifier, Type]): Type
  }

  case class FunctionType(typeParams: List[Identifier], paramTypes: List[(Evaluation, Type)], resultType: Type, location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = {
      val tmp = args -- typeParams
      FunctionType(typeParams, paramTypes.map(x => (x._1, x._2.resolve(tmp))), resultType.resolve(tmp))
    }

    override def toString = {
      val typeParamsString = if (typeParams.isEmpty) "" else s"[${typeParams.mkString(", ")}]"
      s"$typeParamsString(${paramTypes.map(x => "" + x._1 + " " + x._2).mkString(", ")}) => $resultType"
    }
  }

  case class InstatiatedType(name: String, typeArgs: List[Type], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = InstatiatedType(name, typeArgs.map(_.resolve(args)))

    override def toString = name + (if (typeArgs.nonEmpty) "[" + typeArgs.mkString(", ") + "]" else "")
  }

  case class RecordType(entries: Map[Name, Type], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = RecordType(entries.view.mapValues(_.resolve(args)).toMap)

    override def toString = if (entries.isEmpty) "Unit" else {
      val isTuple = entries.keys.forall(_.name.matches("_\\d+"))
      if (isTuple) {
        val sorted = entries.toList.map(x => (x._1.name.substring(1).toInt, x._2)).sortBy(_._1).map(_._2)
        s"(${sorted.mkString(", ")})"
      } else {
        val sorted = entries.toList.sortBy(_._1.name).map(x => "" + x._1 + " = " + x._2)
        s"{${sorted.mkString(", ")}}"
      }
    }
  }

  val UnitType = RecordType(Map(), Location.unknown)

  case class TypeParam(name: Identifier, location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = args.getOrElse(name, this)

    override def toString = name.toString
  }

  val FloatType = InstatiatedType("Float", Nil, Location.builtIn)
  val IntType = InstatiatedType("Int", Nil, Location.builtIn)
  val StringType = InstatiatedType("String", Nil, Location.builtIn)
  val BoolType = InstatiatedType("Bool", Nil, Location.builtIn)
}

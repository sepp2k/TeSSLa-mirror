package de.uni_luebeck.isp.tessla

import cats._
import cats.data.Ior
import cats.implicits._

import scala.collection.immutable.ArraySeq
import scala.util.Try

object TesslaAST {

  case class PrintOptions(defTypes: Boolean = true, expTypes: Boolean = false, refTypes: Boolean = false,
    paramTypes: Boolean = true, locations: Boolean = false
  )

  trait Locatable {
    def location: Location

    def withLocation(s: Boolean => String, options: PrintOptions, mayNeedBraces: Boolean) = printWithLocation(s, options, mayNeedBraces, location)
  }

  def printWithLocation(s: Boolean => String, options: PrintOptions, mayNeedBraces: Boolean, location: Location) =
    withBraces(if (options.locations && location != Location.unknown) s"${s(true)} @ $location" else s(mayNeedBraces), mayNeedBraces && options.locations)

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

    override type Annotations = Map[String, ArraySeq[ExpressionArg]]

    override def printAnnotations(annotations: Annotations, options: PrintOptions) = annotations.toList.sortBy(_._1).map { case (name, exp) =>
      s"@$name(${exp.map(_.print(options, mayNeedBraces = false)).mkString(", ")})"
    }
  }

  object Typed extends TesslaAST[Id] {
    override type DefinitionExpression = ExpressionArg

    override type Evaluation = CompiletimeEvaluation

    override def withTypeAnnotation(s: Boolean => String, tpe: Type, types: Boolean, mayNeedBraces: Boolean) =
      if (types) s"${s(true)}: $tpe" else s(mayNeedBraces)

    override type Annotations = Map[String, ArraySeq[ExpressionArg]]

    override def printAnnotations(annotations: Annotations, options: PrintOptions) = annotations.toList.sortBy(_._1).map { case (name, exp) =>
      s"@$name(${exp.map(_.print(options, mayNeedBraces = false)).mkString(", ")})"
    }
  }

  object Core extends TesslaAST[Id] {
    override type DefinitionExpression = Expression

    override type Evaluation = RuntimeEvaluation

    override def withTypeAnnotation(s: Boolean => String, tpe: Type, types: Boolean, mayNeedBraces: Boolean) =
      withBraces(if (types) s"${s(true)}: $tpe" else s(mayNeedBraces), types && mayNeedBraces)

    override type Annotations = Map[String, ArraySeq[ExpressionArg]]

    override def getOutputName(annotations: Annotations) = Try(annotations("name")(0).asInstanceOf[Core.StringLiteralExpression].value).toOption

    override def printAnnotations(annotations: Annotations, options: PrintOptions) = annotations.toList.sortBy(_._1).map { case (name, exp) =>
      s"@$name(${exp.map(_.print(options, mayNeedBraces = false)).mkString(", ")})"
    }

  }

  def withBraces(s: String, addBraces: Boolean) = if (addBraces) s"(${s})" else s

  def printRecord[A](entries: Map[String, A], sep: String, str: A => String, unit: String, options: PrintOptions = PrintOptions(), strName: (String, A) => String = (s: String, _: A) => s) = if (entries.isEmpty) unit else {
    val tuplified = entries.flatMap { case (key, value) =>
      if (key.matches("_\\d+")) {
        Some(key.substring(1).toInt -> value)
      } else None
    }
    if (tuplified.size == entries.size && tuplified.keys.min == 1 && tuplified.keys.max == entries.size) {
      val sorted = tuplified.toList.sortBy(_._1).map(x => str(x._2))
      s"(${sorted.mkString(", ")})"
    } else {
      val sorted = entries.toList.sortBy(_._1).map { case (k, v) =>
        strName(k, v) + sep + str(v)
      }
      s"{${sorted.mkString(", ")}}"
    }
  }

}

abstract class TesslaAST[TypeAnnotation[_] : CommutativeApplicative] {

  import TesslaAST._

  def withTypeAnnotation(s: Boolean => String, tpe: TypeAnnotation[Type], types: Boolean, mayNeedBraces: Boolean): String

  type DefinitionExpression <: ExpressionArg

  type Evaluation >: StrictEvaluation.type

  type Annotations

  case class Specification(in: Map[Identifier, (TypeAnnotation[Type], Annotations)],
    definitions: Map[Identifier, DefinitionExpression],
    out: List[(ExpressionRef, Annotations)],
    maxIdentifier: Long
  ) {
    def print(options: PrintOptions) = {
      val i = in.map { s =>
        val as = printAnnotations(s._2._2, options)
        (if (as.nonEmpty) as.mkString("", "\n", "\n") else "") +
          withTypeAnnotation(_ => s"in ${s._1}", s._2._1, types = true, mayNeedBraces = false) +
          (if (options.locations && s._1.location != Location.unknown) " @ " + s._1.location else "")
      }.mkString("\n")
      val o = out.map { x =>
        val as = printAnnotations(x._2, options)
        (if (as.nonEmpty) as.mkString("", "\n", "\n") else "") + "out " + x._1 +
          (if (options.locations && x._1.location != Location.unknown) " @ " + x._1.location else "")
      }.mkString("\n")
      val d = definitions.map { x =>
        val tpeString = if (options.defTypes) s": ${x._2.tpe}" else ""
        val locString = if (options.locations && x._1.location != Location.unknown) " @ " + x._1.location else ""
        s"def ${x._1}$tpeString$locString = ${x._2.print(options, mayNeedBraces = false)}"
      }.mkString("\n")
      s"$i\n\n$o\n\n$d"
    }

    override def toString = print(PrintOptions())
  }

  def getOutputName(annotations: Annotations): Option[String] = None

  def printAnnotations(annotations: Annotations, options: PrintOptions): List[String] = Nil

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
        x._1.print(options, mayNeedBraces = false) + (if (options.paramTypes) ": " + x._2.toString + " " + x._3 else "")
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
      if (typeArgs.isEmpty) applicable.print(options, mayNeedBraces) else {
        val typeArgsString = s"[${typeArgs.mkString(", ")}]"
        withTypeAndLocation(_ => s"${applicable.print(options, mayNeedBraces = typeArgs.nonEmpty)}$typeArgsString", options, mayNeedBraces)
      }
    }
  }

  // TODO: use print record
  case class RecordConstructorExpression(entries: Map[String, (ExpressionArg, Location)],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = entries.view.mapValues(x => x._1.tpe.map(t => (t, x._2))).toMap.unorderedSequence.map { m =>
      RecordType(m)
    }

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => {
        val entriesString = if (entries.isEmpty) "" else
          indent(entries.map(x => printWithLocation(_ => x._1, options, mayNeedBraces = false, x._2._2) + ": " + x._2._1.print(options, mayNeedBraces = false)).mkString("\n", ",\n", "\n"))
        s"{$entriesString}"
      }, options, mayNeedBraces)
  }

  case class RecordAccessorExpression(name: String, target: ExpressionArg,
    nameLocation: Location = Location.unknown, location: Location = Location.unknown
  ) extends Expression {
    override def tpe = target.tpe.map { t =>
      t.asInstanceOf[RecordType].entries(name)._1
    }

    override def print(options: PrintOptions, mayNeedBraces: Boolean): String =
      withTypeAndLocation(_ => s"${target.print(options, mayNeedBraces = true)}.${printWithLocation(_ => name, options, mayNeedBraces = false, nameLocation)}", options, mayNeedBraces)
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

  case class InstantiatedType(name: String, typeArgs: List[Type], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = InstantiatedType(name, typeArgs.map(_.resolve(args)))

    override def toString = name + (if (typeArgs.nonEmpty) "[" + typeArgs.mkString(", ") + "]" else "")
  }

  case class RecordType(entries: Map[String, (Type, Location)], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = RecordType(entries.view.mapValues(x => (x._1.resolve(args), x._2)).toMap)

    override def toString = printRecord(entries, ": ", (x: (Type, Location)) => x._1.toString, "Unit")
  }

  val UnitType = RecordType(Map(), Location.builtIn)

  case class TypeParam(name: Identifier, location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = args.getOrElse(name, this)

    override def toString = name.toString
  }

  val FloatType = InstantiatedType("Float", Nil, Location.builtIn)
  val IntType = InstantiatedType("Int", Nil, Location.builtIn)
  val StringType = InstantiatedType("String", Nil, Location.builtIn)
  val BoolType = InstantiatedType("Bool", Nil, Location.builtIn)
}

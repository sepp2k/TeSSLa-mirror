package de.uni_luebeck.isp.tessla

import language.higherKinds

import scala.collection.mutable
import cats._
import cats.implicits._

object TesslaAST {

  trait Locatable {
    def location: Location
  }

  final class Identifier(val id: String, override val location: Location = Location.unknown) extends Locatable {
    override def hashCode() = id.hashCode

    override def equals(o: Any) = o match {
      case tmp: Identifier => tmp.id.equals(id)
      case _ => false
    }

    override def toString = id
  }

  object Identifier {
    def apply(id: String, location: Location = Location.unknown) = new Identifier(id, location)

    def unapply(arg: Identifier): Option[(String, Location)] = Some((arg.id, arg.location))
  }

  sealed trait CompiletimeEvaluation

  sealed trait RuntimeEvaluation extends CompiletimeEvaluation

  case object LazyEvaluation extends RuntimeEvaluation {
    override def toString = "lazy"
  }

  case object StrictEvaluation extends RuntimeEvaluation {
    override def toString = "strict"
  }

  case object ExpandEvaluation extends CompiletimeEvaluation

  object Untyped extends TesslaAST[Option] {
    override type DefinitionExpression = ExpressionArg

    override type Evaluation = CompiletimeEvaluation
  }

  object Typed extends TesslaAST[Id] {
    override type DefinitionExpression = ExpressionArg

    override type Evaluation = CompiletimeEvaluation
  }

  object Core extends TesslaAST[Id] {
    override type DefinitionExpression = ExpressionArg

    override type Evaluation = RuntimeEvaluation
  }

}

abstract class TesslaAST[TypeAnnotation[_] : CommutativeApplicative] {

  import TesslaAST._

  type DefinitionExpression <: ExpressionArg

  type Evaluation

  case class Specification(in: Map[Identifier, (TypeAnnotation[Type], List[DefinitionExpression])], definitions: Map[Identifier, DefinitionExpression], out: List[(Identifier, Option[String], List[DefinitionExpression])]) { // TODO: make optional output name an annotation
    override def toString = {
      val i = in.keys.map(s => s"in $s").mkString("\n")
      val o = out.map(x => x._2 match {
        case Some(name) => s"out $name = ${x._1}"
        case None => "out" + x._1
      }).mkString("\n")
      val d = definitions.map(x => s"def ${x._1}: ${x._2.tpe} = ${x._2}").mkString("\n")
      s"$i\n\n$o\n\n$d"
    }
  }

  sealed trait ExpressionArg extends Locatable {
    def tpe: TypeAnnotation[Type]
  }

  sealed trait Expression extends ExpressionArg

  case class ExpressionRef(id: Identifier, tpe: TypeAnnotation[Type], location: Location = Location.unknown) extends ExpressionArg {
    override def toString = id.toString
  }

  def indent(s: String) = s.lines.map(s => "  " + s).mkString("\n")

  case class FunctionExpression(typeParams: List[Identifier], params: List[(Identifier, Evaluation, TypeAnnotation[Type])], body: Map[Identifier, DefinitionExpression],
    result: ExpressionArg, location: Location = Location.unknown
  ) extends Expression {
    override def tpe = Applicative[TypeAnnotation].map2(params.map(x => x._3.map(y => (x._2, y))).sequence, result.tpe) { (m, r) =>
      FunctionType(typeParams, m, r)
    }

    override def toString = {
      val b = indent(body.map(x => s"def ${x._1}: ${x._2.tpe} = ${x._2}").mkString("", "\n", "\n") + result)
      s"(${params.map(_._1).mkString(", ")}) => {\n$b\n}"
    }
  }

  case class ExternExpression(typeParams: List[Identifier], params: List[(Identifier, Evaluation, TypeAnnotation[Type])],
    resultType: TypeAnnotation[Type], name: String, location: Location = Location.unknown
  ) extends Expression {
    override def tpe = Applicative[TypeAnnotation].map2(params.map(x => x._3.map(y => (x._2, y))).sequence, resultType) { (m, r) =>
      FunctionType(typeParams, m, r)
    }

    override def toString = s"extern_$name"
  }

  case class ApplicationExpression(applicable: ExpressionArg, args: List[ExpressionArg],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = {
      applicable.tpe.map { r =>
        val ft = r.asInstanceOf[FunctionType] // TODO: Make typesafe?
        if (ft.typeParams.nonEmpty) throw new IllegalArgumentException("Unresolved type parameters")
        ft.resultType
      }
    }

    override def toString = s"$applicable(${args.mkString(", ")})"
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

    override def toString = s"$applicable[${typeArgs.mkString(", ")}]"
  }

  case class RecordConstructorExpression(entries: Map[Identifier, ExpressionArg],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = entries.mapValues(_.tpe).unorderedSequence.map { m =>
      RecordType(m)
    }
  }

  case class RecordAccesorExpression(name: Identifier, target: ExpressionArg,
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = target.tpe.map { t =>
      t.asInstanceOf[RecordType].entries(name)
    }
  }

  case class StringLiteralExpression(value: String, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(StringType)

    override def toString = value.toString
  }

  case class IntLiteralExpression(value: BigInt, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(IntType)

    override def toString = value.toString
  }

  case class FloatLiteralExpression(value: Double, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(FloatType)

    override def toString = value.toString
  }

  sealed trait Type extends Locatable {
    def resolve(args: Map[Identifier, Type]): Type
  }

  case class FunctionType(typeParams: List[Identifier], paramTypes: List[(Evaluation, Type)], resultType: Type, location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = {
      val tmp = args -- typeParams
      FunctionType(typeParams, paramTypes.map(x => (x._1, x._2.resolve(tmp))), resultType.resolve(tmp))
    }

    override def toString = s"[${typeParams.mkString(", ")}](${paramTypes.map(x => x._1 + " " + x._2).mkString(", ")}) => $resultType"
  }

  case class InstatiatedType(name: String, typeArgs: List[Type], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = InstatiatedType(name, typeArgs.map(_.resolve(args)))

    override def toString = name + (if (typeArgs.nonEmpty) "(" + typeArgs.mkString(", ") + ")" else "")
  }

  case class RecordType(entries: Map[Identifier, Type], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = RecordType(entries.mapValues(_.resolve(args)))
  }

  val UnitType = RecordType(Map(), Location.unknown)

  case class TypeParam(name: Identifier, location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = args.getOrElse(name, this)

    override def toString = name.id
  }

  val FloatType = InstatiatedType("Float", Nil, Location.builtIn)
  val IntType = InstatiatedType("Int", Nil, Location.builtIn)
  val StringType = InstatiatedType("String", Nil, Location.builtIn)
  val BoolType = InstatiatedType("Bool", Nil, Location.builtIn) // TODO: consider removing as no corresponding literal


  ////// Non AST Stuff, kept for inspiration

  import de.uni_luebeck.isp.tessla.util.Lazy
  ////// Runtime Stuff


  ////// Compiletime Stuff

  sealed trait CompiletimeValue

  case class CompiletimeExternValue(value: Any) extends CompiletimeValue

  case class CompiletimeExpressionValue(expression: ExpressionArg) extends CompiletimeValue

  case class CompiletimeClosure(function: FunctionExpression, env: Map[Identifier, Lazy[CompiletimeValue]],
    translatedFunction: Identifier, translatedEpressions: mutable.Map[Identifier, Expression], queue: mutable.Queue[() => Unit]
  ) extends CompiletimeValue


  ////// Externs
  def evalExtern(name: String, args: List[Any]): Any = ???

  def reifyExtern(value: Any): Option[Expression] = ???


}

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
  }

  object Untyped extends TesslaAST[Option] {
    override type DefinitionExpression = ExpressionArg
  }

  object Core extends TesslaAST[Id] {
    override type DefinitionExpression = Expression
  }

}

abstract class TesslaAST[TypeAnnotation[_] : CommutativeApplicative] {

  import TesslaAST._

  type DefinitionExpression <: ExpressionArg


  case class Specification(in: Map[Identifier, (TypeAnnotation[Type], List[DefinitionExpression])], definitions: Map[Identifier, DefinitionExpression], out: Map[Identifier, List[DefinitionExpression]])

  sealed trait ExpressionArg extends Locatable {
    def tpe: TypeAnnotation[Type]
  }

  sealed trait Expression extends ExpressionArg

  case class ExpressionRef(id: Identifier, tpe: TypeAnnotation[Type], location: Location = Location.unknown) extends ExpressionArg

  case class FunctionExpression(typeParams: List[Identifier], params: List[(Identifier, Evaluation, TypeAnnotation[Type])], body: Map[Identifier, DefinitionExpression],
    result: ExpressionArg, location: Location = Location.unknown
  ) extends Expression {
    override def tpe = Applicative[TypeAnnotation].map2(params.map(_._3).sequence, result.tpe) { (m, r) =>
      FunctionType(typeParams, m, r)
    }
  }

  case class ExternExpression(typeParams: List[Identifier], params: List[(Identifier, Evaluation, TypeAnnotation[Type])],
    resultType: TypeAnnotation[Type], name: String, location: Location = Location.unknown
  ) extends Expression {
    override def tpe = Applicative[TypeAnnotation].map2(params.map(_._3).sequence, resultType) { (m, r) =>
      FunctionType(typeParams, m, r)
    }
  }

  case class ApplicationExpression(applicable: ExpressionArg, args: List[ExpressionArg],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = {
      applicable.tpe.map { r =>
        val ft = r.asInstanceOf[FunctionType]
        if (ft.typeParams.nonEmpty) throw new IllegalArgumentException("Unresolved type parameters")
        ft.resultType
      }
    }
  }

  case class TypeApplicationExpression(applicable: ExpressionArg, typeArgs: List[Type],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = {
      applicable.tpe.map { r =>
        val ft = r.asInstanceOf[FunctionType]
        val map = ft.typeParams.zip(typeArgs).toMap
        FunctionType(List(), ft.paramTypes.map(_.resolve(map)), ft.resultType.resolve(map))
      }
    }
  }

  case class RecordConstructorExpression(entries: Map[String, ExpressionArg],
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = entries.mapValues(_.tpe).unorderedSequence.map { m =>
      RecordType(m)
    }
  }

  case class RecordAccesorExpression(name: String, target: ExpressionArg,
    location: Location = Location.unknown
  ) extends Expression {
    override def tpe = target.tpe.map { t =>
      t.asInstanceOf[RecordType].entries(name)
    }
  }

  case class BoolLiteralExpression(value: Boolean, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(BoolType)
  }

  case class StringLiteralExpression(value: String, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(StringType)
  }

  case class IntLiteralExpression(value: BigInt, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(IntType)
  }

  case class FloatLiteralExpression(value: Double, location: Location = Location.unknown) extends Expression {
    override def tpe = Applicative[TypeAnnotation].pure(FloatType)
  }

  sealed trait Evaluation

  case object LazyEvaluation extends Evaluation

  case object StrictEvaluation extends Evaluation

  case object ExpandEvaluation extends Evaluation

  sealed trait Type extends Locatable {
    def resolve(args: Map[Identifier, Type]): Type
  }

  case class FunctionType(typeParams: List[Identifier], paramTypes: List[Type], resultType: Type, location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = {
      val tmp = args -- typeParams
      FunctionType(typeParams, paramTypes.map(_.resolve(tmp)), resultType.resolve(tmp))
    }
  }

  case class InstatiatedType(name: String, typeArgs: List[Type], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = InstatiatedType(name, typeArgs.map(_.resolve(args)))
  }

  case class RecordType(entries: Map[String, Type], location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = RecordType(entries.mapValues(_.resolve(args)))
  }

  case class TypeParam(name: Identifier, location: Location = Location.unknown) extends Type {
    override def resolve(args: Map[Identifier, Type]) = args.getOrElse(name, this)
  }

  case object IntType extends Type {
    override def resolve(args: Map[Identifier, Type]) = this

    override def location = Location.unknown
  }

  case object FloatType extends Type {
    override def resolve(args: Map[Identifier, Type]) = this

    override def location = Location.unknown
  }

  case object BoolType extends Type {
    override def resolve(args: Map[Identifier, Type]) = this

    override def location = Location.unknown
  }

  case object StringType extends Type {
    override def resolve(args: Map[Identifier, Type]) = this

    override def location = Location.unknown
  }


  ////// Non AST Stuff, kept for inspiration
  import de.uni_luebeck.isp.tessla.util.Lazy
  ////// Runtime Stuff

  sealed trait RuntimeValue

  case class RuntimeExternValue(value: Any) extends RuntimeValue

  case class RuntimeClosure(function: FunctionExpression, env: Map[Identifier, Lazy[RuntimeValue]]) extends RuntimeValue

  case class Record(entries: Map[String, Any])

  ////// Compiletime Stuff

  sealed trait CompiletimeValue

  case class CompiletimeExternValue(value: Any) extends CompiletimeValue

  case class CompiletimeExpressionValue(expression: ExpressionArg) extends CompiletimeValue

  case class CompiletimeClosure(function: FunctionExpression, env: Map[Identifier, Lazy[CompiletimeValue]],
    translatedFunction: Identifier, translatedEpressions: mutable.Map[Identifier, Expression], queue: mutable.Queue[() => Unit]
  ) extends RuntimeValue


  ////// Externs
  def evalExtern(name: String, args: List[Any]): Any = ???

  def reifyExtern(value: Any): Option[Expression] = ???


}

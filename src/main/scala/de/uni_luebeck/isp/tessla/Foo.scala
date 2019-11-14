package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.util.Lazy

import scala.collection.mutable

object Foo {

  case class Id()

  case class Annotation()

  case class Location()

  sealed trait Locatable {
    def loc: Location = ???
  }

  final case class Specification(in: Map[Id, (Type, Seq[Annotation])], definitions: Map[Id, Expression], out: Map[Id, Seq[Annotation]])

  sealed trait ExpressionArg extends Locatable {
    def tpe: Type
  }

  sealed trait Expression extends ExpressionArg

  final case class ExpressionRef(id: Id, tpe: Type) extends ExpressionArg

  final case class FunctionExpression(typeParams: Seq[Id], params: Seq[(Id, Evaluation, Type)], body: Map[Id, Expression], result: ExpressionArg) extends Expression {
    override def tpe = FunctionType(typeParams, params.map(_._3), result.tpe)
  }

  final case class ExternExpression(typeParams: Seq[Id], params: Seq[(Id, Evaluation, Type)], resultType: Type, name: String) extends Expression {
    override def tpe = FunctionType(typeParams, params.map(_._3), resultType)
  }

  final case class ApplicationExpression(appliable: ExpressionArg, args: Seq[ExpressionArg]) extends Expression {
    override def tpe = {
      val ft = appliable.tpe.asInstanceOf[FunctionType]
      if (ft.typeParams.nonEmpty) throw new IllegalArgumentException("Unresolved type parameters")
      ft.resultType
    }
  }

  final case class TypeApplicationExpression(applicable: ExpressionArg, typeArgs: Seq[Type]) extends Expression {
    override def tpe = {
      val ft = applicable.tpe.asInstanceOf[FunctionType]
      val map = ft.typeParams.zip(typeArgs).toMap
      FunctionType(Seq(), ft.paramTypes.map(_.resolve(map)), ft.resultType.resolve(map))
    }
  }

  final case class RecordConstructorExpression(entries: Map[String, ExpressionArg]) extends Expression {
    override def tpe = RecordType(entries.mapValues(_.tpe))
  }

  final case class RecordAccesorExpression(name: Id, target: ExpressionArg) extends Expression {
    override def tpe = target.tpe.asInstanceOf[RecordType].entries(name)
  }

  final case class BoolLiteralExpression(value: Boolean) extends Expression {
    override def tpe = BoolType
  }

  final case class StringLiteralExpression(value: String) extends Expression {
    override def tpe = StringType
  }

  final case class IntLiteralExpression(value: BigInt) extends Expression {
    override def tpe = IntType
  }

  final case class FloatLiteralExpression(value: Double) extends Expression {
    override def tpe = FloatType
  }

  sealed trait Evaluation

  case object LazyEvaluation extends Evaluation

  case object StrictEvaluation extends Evaluation

  case object ExpandEvaluation extends Evaluation

  sealed trait Type extends Locatable {
    def resolve(args: Map[Id, Type]): Type
  }

  final case class FunctionType(typeParams: Seq[Id], paramTypes: Seq[Type], resultType: Type) extends Type {
    override def resolve(args: Map[Id, Type]) = {
      val tmp = args -- typeParams
      FunctionType(typeParams, paramTypes.map(_.resolve(tmp)), resultType.resolve(tmp))
    }
  }

  final case class InstatiatedType(name: Id, typeArgs: Seq[Type]) extends Type {
    override def resolve(args: Map[Id, Type]) = InstatiatedType(name, typeArgs.map(_.resolve(args)))
  }

  final case class RecordType(entries: Map[String, Type]) extends Type {
    override def resolve(args: Map[Id, Type]) = RecordType(entries.mapValues(_.resolve(args)))
  }

  final case class TypeParam(name: Id) extends Type {
    override def resolve(args: Map[Id, Type]) = args.getOrElse(name, this)
  }

  final case object IntType extends Type {
    override def resolve(args: Map[Id, Type]) = this
  }

  final case object FloatType extends Type {
    override def resolve(args: Map[Id, Type]) = this
  }

  final case object BoolType extends Type {
    override def resolve(args: Map[Id, Type]) = this
  }

  final case object StringType extends Type {
    override def resolve(args: Map[Id, Type]) = this
  }


  ////// Runtime Stuff

  sealed trait RuntimeValue

  final case class RuntimeExternValue(value: Any) extends RuntimeValue

  final case class RuntimeClosure(function: FunctionExpression, env: Map[Id, Lazy[RuntimeValue]]) extends RuntimeValue

  final case class Record(entries: Map[String, Any])

  ////// Compiletime Stuff

  sealed trait CompiletimeValue

  final case class CompiletimeExternValue(value: Any) extends CompiletimeValue

  final case class CompiletimeExpressionValue(expression: ExpressionArg) extends CompiletimeValue

  final case class CompiletimeClosure(function: FunctionExpression, env: Map[Id, Lazy[CompiletimeValue]], translatedFunction: Id, translatedEpressions: mutable.Map[Id, Expression], queue: mutable.Queue[() => ()]) extends RuntimeValue


  ////// Externs
  def evalExtern(name: String, args: Seq[Any]): Any = ???

  def reifyExtern(value: Any): Option[Expression] = ???


}

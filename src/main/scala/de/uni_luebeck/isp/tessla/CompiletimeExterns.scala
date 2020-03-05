package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TesslaAST.{Core, StrictEvaluation}
import Core.Identifier
import scala.collection.immutable.ArraySeq
import cats._
import cats.implicits._
import de.uni_luebeck.isp.tessla.Errors.InternalError
import de.uni_luebeck.isp.tessla.util.ArraySeqMonad.instance

// TODO: use extern declarations from the standard library
object CompiletimeExterns {

  type Reifier = (Any, List[Core.Type]) => (ArraySeq[(Any, Core.Type)], ArraySeq[Core.ExpressionArg] => Core.Expression)

  type WithError[+A] = Either[InternalError, A]

  import ConstantEvaluator.lazyWithStack._

  def reify(value: Any, tpe: Core.Type): StackLazy[WithError[Core.Expression]] = tpe match {
    case Core.InstatiatedType(name, typeArgs, _) =>
      (for {
        reifier <- reifiers.get(name).map(Right(_)).getOrElse(Left(InternalError(s"No reifier for extern type $name.")))
        (subValues, reification) = reifier(value, typeArgs)
      } yield for {
        reified <- subValues.traverse(x => reify(x._1, x._2))
      } yield for {
        r <- reified.sequence
      } yield reification(r)).sequence.map(_.flatten)
    case Core.RecordType(entries, _) =>
      val entries = value.asInstanceOf[RuntimeEvaluator.Record].entries.map { v =>
        //reify(v._2, entries(Identifier(v._1))).map(y => (Identifier(v._1), y))
        v._1 -> ConstantEvaluator.getExpressionArgStrict(v._2.asInstanceOf[ConstantEvaluator.TranslationResult[Any, Some]]).map(x => (x, Location.unknown))
      }
      entries.unorderedSequence.map(v => Right(Core.RecordConstructorExpression(v)))
    case _ => Lazy(Left(InternalError(s"Could not reify value for type $tpe.")))
  }

  val true_extern = Core.ExternExpression(Nil, Nil, Core.BoolType, "true")

  val false_extern = Core.ExternExpression(Nil, Nil, Core.BoolType, "false")

  val option_some_extern = Core.ExternExpression(
    List(Identifier("A")),
    List((StrictEvaluation, Core.TypeParam(Identifier("A")))),
    Core.InstatiatedType("Option", List(Core.TypeParam(Identifier("A")))),
    "Some"
  )

  val option_none_extern = Core.ExternExpression(
    List(Identifier("A")),
    Nil,
    Core.InstatiatedType("Option", List(Core.TypeParam(Identifier("A")))),
    "None"
  )

  val list_prepend_extern = Core.ExternExpression(
    List(Identifier("A")),
    List(
      (StrictEvaluation, Core.TypeParam(Identifier("A"))),
      (StrictEvaluation, Core.InstatiatedType("List", List(Core.TypeParam(Identifier("A")))))
    ),
    Core.InstatiatedType("List", List(Core.TypeParam(Identifier("A")))),
    "List_prepend"
  )

  val list_empty_extern = Core.ExternExpression(
    List(Identifier("A")),
    Nil,
    Core.InstatiatedType("List", List(Core.TypeParam(Identifier("A")))),
    "List_empty"
  )

  val set_add_extern = Core.ExternExpression(
    List(Identifier("A")),
    List(
      (StrictEvaluation, Core.InstatiatedType("Set", List(Core.TypeParam(Identifier("A"))))),
      (StrictEvaluation, Core.TypeParam(Identifier("A")))
    ),
    Core.InstatiatedType("Set", List(Core.TypeParam(Identifier("A")))),
    "Set_add"
  )

  val set_empty_extern = Core.ExternExpression(
    List(Identifier("A")),
    Nil,
    Core.InstatiatedType("Set", List(Core.TypeParam(Identifier("A")))),
    "Set_empty"
  )

  val map_add_extern = Core.ExternExpression(
    List(Identifier("A"), Identifier("B")),
    List(
      (StrictEvaluation, Core.InstatiatedType("Map", List(Core.TypeParam(Identifier("A")), Core.TypeParam(Identifier("B"))))),
      (StrictEvaluation, Core.TypeParam(Identifier("A"))),
      (StrictEvaluation, Core.TypeParam(Identifier("B")))
    ),
    Core.InstatiatedType("Map", List(Core.TypeParam(Identifier("A")), Core.TypeParam(Identifier("B")))),
    "Map_add"
  )

  val map_empty_extern = Core.ExternExpression(
    List(Identifier("A"), Identifier("B")),
    Nil,
    Core.InstatiatedType("Map", List(Core.TypeParam(Identifier("A")), Core.TypeParam(Identifier("B")))),
    "Map_empty"
  )

  def app(extern: Core.ExternExpression, typeArgs: List[Core.Type])(args: ArraySeq[Core.ExpressionArg]) =
    Core.ApplicationExpression(Core.TypeApplicationExpression(extern, typeArgs), args)

  val reifiers = Map[String, Reifier](
    "Int" -> ((value, _) => (ArraySeq(), _ => Core.IntLiteralExpression(value.asInstanceOf[BigInt]))),
    "String" -> ((value, _) => (ArraySeq(), _ => Core.StringLiteralExpression(value.asInstanceOf[String]))),
    "Float" -> ((value, _) => (ArraySeq(), _ => Core.FloatLiteralExpression(value.asInstanceOf[Double]))),
    "Option" -> ((value, typeArgs) => value.asInstanceOf[Option[Any]] match {
      case Some(a) => (ArraySeq((a, typeArgs.head)), app(option_some_extern, typeArgs))
      case None => (ArraySeq(), app(option_none_extern, typeArgs))
    }),
    "List" -> ((value, typeArgs) => value.asInstanceOf[List[Any]] match {
      case a :: as => (ArraySeq((a, typeArgs.head), (as, Core.InstatiatedType("List", typeArgs))), app(list_prepend_extern, typeArgs))
      case Nil => (ArraySeq(), app(list_empty_extern, typeArgs))
    }),
    "Set" -> ((value, typeArgs) => {
      val set = value.asInstanceOf[Set[Any]]
      if (set.isEmpty)
        (ArraySeq(), app(set_empty_extern, typeArgs))
      else
        (ArraySeq((set.tail, Core.InstatiatedType("Set", typeArgs)), (set.head, typeArgs.head)), app(set_add_extern, typeArgs))
    }),
    "Map" -> ((value, typeArgs) => {
      val map = value.asInstanceOf[Map[Any, Any]]
      if (map.isEmpty)
        (ArraySeq(), app(map_empty_extern, typeArgs))
      else
        (ArraySeq((map.tail, Core.InstatiatedType("Map", typeArgs)), (map.head._1, typeArgs.head), (map.head._2, typeArgs(1))),
          app(map_add_extern, typeArgs))
    }),
    "Bool" -> ((value, _) => (ArraySeq(), if (value.asInstanceOf[Boolean]) app(true_extern, Nil) else app(false_extern, Nil)))
  )
}

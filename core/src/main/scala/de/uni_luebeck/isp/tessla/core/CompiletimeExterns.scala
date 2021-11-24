/*
 * Copyright 2021 The TeSSLa Community
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

import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, LazyEvaluation, StrictEvaluation}
import Core.Identifier

import scala.collection.immutable.ArraySeq
import cats.implicits._
import de.uni_luebeck.isp.tessla.core
import de.uni_luebeck.isp.tessla.core.ConstantEvaluator.errorExtern
import de.uni_luebeck.isp.tessla.core.Errors.InternalError
import de.uni_luebeck.isp.tessla.core.util.ArraySeqMonad.instance

/**
 * Provider for externs which are known at compile time, to use for reification within the [[ConstantEvaluator]].
 */

// TODO: use extern declarations from the standard library
object CompiletimeExterns {

  /**
   * A function that reifies (i.e. converts to an expression) some value of some type.
   * The first parameter is the value, the second a list of types for the type parameters of its type.
   * It returns
   *   - a sequence of values (with corresponding types) that still have to be reified (e.g. elements of a container type).
   *   - a function that can generate the reified expression given expressions for the values in the sequence.
   * If the sequence is empty, the function takes no arguments and can be called directly to obtain the reified expression.
   */
  type Reifier = (Any, List[Core.Type]) => (
    ArraySeq[(Any, Core.Type)],
    ArraySeq[Core.ExpressionArg] => Core.Expression
  )

  type WithError[+A] = Either[InternalError, A]

  import ConstantEvaluator.lazyWithStack._

  /**
   * Converts a value (given its type) to an expression.
   */
  def reify(value: Any, tpe: Core.Type): StackLazy[WithError[Core.Expression]] =
    value match {
      case error: ConstantEvaluator.RuntimeError =>
        Lazy(Right(ConstantEvaluator.mkErrorExpression(error, tpe)))
      case _ =>
        tpe match {
          case Core.InstantiatedType(name, typeArgs, _) =>
            (for {
              reifier <-
                reifiers
                  .get(name)
                  .map(Right(_))
                  .getOrElse(Left(InternalError(s"No reifier for extern type $name.")))
              (subValues, reification) = reifier(value, typeArgs)
            } yield for {
              reified <- subValues.traverse(x => reify(x._1, x._2))
            } yield for {
              r <- reified.sequence
            } yield reification(r)).sequence.map(_.flatten)
          case Core.RecordType(entries, _) =>
            val entries = value.asInstanceOf[ConstantEvaluator.Record].entries.map { v =>
              //reify(v._2, entries(Identifier(v._1))).map(y => (Identifier(v._1), y))
              v._1 -> ConstantEvaluator
                .getExpressionArgStrict(v._2.asInstanceOf[ConstantEvaluator.TranslationResult[Any, Some]])
                .map(x => (x, Location.unknown))
            }
            entries.unorderedSequence.map(v => Right(Core.RecordConstructorExpression(v)))
          case _ => Lazy(Left(InternalError(s"Could not reify value for type $tpe.")))
        }
    }

  val true_extern = Core.ExternExpression("true", Core.BoolType)

  val false_extern = Core.ExternExpression("false", Core.BoolType)

  val option_some_extern = Core.ExternExpression(
    "Some",
    Core.FunctionType(
      List(Identifier("A")),
      List((LazyEvaluation, Core.TypeParam(Identifier("A")))),
      Core.InstantiatedType("Option", List(Core.TypeParam(Identifier("A"))))
    )
  )

  val option_none_extern = Core.ExternExpression(
    "None",
    Core.FunctionType(
      List(Identifier("A")),
      Nil,
      Core.InstantiatedType("Option", List(Core.TypeParam(Identifier("A"))))
    )
  )

  val list_prepend_extern = Core.ExternExpression(
    "List_prepend",
    Core.FunctionType(
      List(Identifier("A")),
      List(
        (StrictEvaluation, Core.TypeParam(Identifier("A"))),
        (StrictEvaluation, Core.InstantiatedType("List", List(Core.TypeParam(Identifier("A")))))
      ),
      Core.InstantiatedType("List", List(Core.TypeParam(Identifier("A"))))
    )
  )

  val list_empty_extern = Core.ExternExpression(
    "List_empty",
    Core.FunctionType(
      List(Identifier("A")),
      Nil,
      Core.InstantiatedType("List", List(Core.TypeParam(Identifier("A"))))
    )
  )

  val set_add_extern = Core.ExternExpression(
    "Set_add",
    Core.FunctionType(
      List(Identifier("A")),
      List(
        (StrictEvaluation, Core.InstantiatedType("Set", List(Core.TypeParam(Identifier("A"))))),
        (StrictEvaluation, Core.TypeParam(Identifier("A")))
      ),
      Core.InstantiatedType("Set", List(Core.TypeParam(Identifier("A"))))
    )
  )

  val set_empty_extern = Core.ExternExpression(
    "Set_empty",
    Core.FunctionType(
      List(Identifier("A")),
      Nil,
      Core.InstantiatedType("Set", List(Core.TypeParam(Identifier("A"))))
    )
  )

  val map_add_extern = Core.ExternExpression(
    "Map_add",
    Core.FunctionType(
      List(Identifier("A"), Identifier("B")),
      List(
        (
          StrictEvaluation,
          Core.InstantiatedType(
            "Map",
            List(Core.TypeParam(Identifier("A")), Core.TypeParam(Identifier("B")))
          )
        ),
        (StrictEvaluation, Core.TypeParam(Identifier("A"))),
        (StrictEvaluation, Core.TypeParam(Identifier("B")))
      ),
      Core.InstantiatedType(
        "Map",
        List(Core.TypeParam(Identifier("A")), Core.TypeParam(Identifier("B")))
      )
    )
  )

  val map_empty_extern = Core.ExternExpression(
    "Map_empty",
    Core.FunctionType(
      List(Identifier("A"), Identifier("B")),
      Nil,
      Core.InstantiatedType(
        "Map",
        List(Core.TypeParam(Identifier("A")), Core.TypeParam(Identifier("B")))
      )
    )
  )

  def app(extern: Core.ExternExpression, typeArgs: List[Core.Type])(
    args: ArraySeq[Core.ExpressionArg]
  ) =
    Core.ApplicationExpression(Core.TypeApplicationExpression(extern, typeArgs), args)

  val reifiers = Map[String, Reifier](
    "Int" -> ((value, _) => (ArraySeq(), _ => Core.IntLiteralExpression(value.asInstanceOf[BigInt]))),
    "String" -> ((value, _) => (ArraySeq(), _ => Core.StringLiteralExpression(value.asInstanceOf[String]))),
    "Float" -> ((value, _) => (ArraySeq(), _ => Core.FloatLiteralExpression(value.asInstanceOf[Double]))),
    "Option" -> ((value, typeArgs) =>
      value.asInstanceOf[Option[Any]] match {
        case Some(a) => (ArraySeq((a, typeArgs.head)), app(option_some_extern, typeArgs))
        case None    => (ArraySeq(), app(option_none_extern, typeArgs))
      }
    ),
    "List" -> ((value, typeArgs) =>
      value.asInstanceOf[List[Any]] match {
        case a :: as =>
          (
            ArraySeq((a, typeArgs.head), (as, Core.InstantiatedType("List", typeArgs))),
            app(list_prepend_extern, typeArgs)
          )
        case Nil => (ArraySeq(), app(list_empty_extern, typeArgs))
      }
    ),
    "Set" -> ((value, typeArgs) => {
      val set = value.asInstanceOf[Set[Any]]
      if (set.isEmpty)
        (ArraySeq(), app(set_empty_extern, typeArgs))
      else
        (
          ArraySeq((set.tail, Core.InstantiatedType("Set", typeArgs)), (set.head, typeArgs.head)),
          app(set_add_extern, typeArgs)
        )
    }),
    "Map" -> ((value, typeArgs) => {
      val map = value.asInstanceOf[Map[Any, Any]]
      if (map.isEmpty)
        (ArraySeq(), app(map_empty_extern, typeArgs))
      else
        (
          ArraySeq(
            (map.tail, Core.InstantiatedType("Map", typeArgs)),
            (map.head._1, typeArgs.head),
            (map.head._2, typeArgs(1))
          ),
          app(map_add_extern, typeArgs)
        )
    }),
    "Bool" -> ((value, _) =>
      (
        ArraySeq(),
        _ => if (value.asInstanceOf[Boolean]) true_extern else false_extern
      )
    )
  )
}

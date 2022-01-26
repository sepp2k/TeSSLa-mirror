/*
 * Copyright 2022 The TeSSLa Community
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

import cats.data.Ior
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.core.util._

import scala.collection.mutable

/**
 * Flattens a [[TesslaAST.Core.Specification]] further, such that its representation is compliant to the definition in the
 * language specification. Per default, this phase is skipped as it reduces the readability of the resulting core
 * code.
 *
  * Specifically, this phase moves every argument which is not a constant or an identifier to a new definition.
 *
  * Example:
 * The following expression
 * {{{
 *   def \$2: Events[Int] = extern("foo")[Int](x)
 * }}}
 *
  * will in this phase be translated to
 * {{{
 *   def \$1: (Events[Int]) => Events[Int] = extern("foo")
 *   def \$2: Events[Int] = \$1(x)
 * }}}
 */

object FlattenCore extends TranslationPhase[Core.Specification, Core.Specification] {

  type Definitions = mutable.Map[Core.Identifier, Core.Expression]

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    object IdentifierFactory {
      private var _id: Long = spec.maxIdentifier

      def next: Core.Identifier = {
        _id += 1
        new Core.Identifier(Ior.right(_id))
      }

      def count: Long = _id
    }

    def addDefinition(e: Core.Expression, defs: Definitions): Core.ExpressionRef = {
      val newID = IdentifierFactory.next
      val flattenedExp = flattenExpression(e, defs)
      defs += newID -> flattenedExp
      Core.ExpressionRef(newID, e.tpe)
    }

    def flattenArg(e: Core.ExpressionArg, defs: Definitions): Core.ExpressionArg =
      e match {
        case r: Core.ExpressionRef => r
        case lit @ (_: Core.IntLiteralExpression | _: Core.StringLiteralExpression | _: Core.FloatLiteralExpression |
            Core.ExternExpression("true", _, _) | Core.ExternExpression("false", _, _)) =>
          lit
        case rec @ Core.RecordConstructorExpression(m, _) if m.isEmpty =>
          flattenExpression(rec, defs)
        case typeApp: Core.TypeApplicationExpression => flattenExpression(typeApp, defs)
        case ext: Core.ExternExpression =>
          defs
            .collectFirst {
              case (id, ext2: Core.ExternExpression) if ext.name == ext2.name && ext.tpe == ext2.tpe =>
                Core.ExpressionRef(id, ext.tpe)
            }
            .getOrElse(addDefinition(ext, defs))
        case e: Core.Expression => addDefinition(e, defs)
      }

    def flattenExpression(e: Core.Expression, defs: Definitions): Core.Expression =
      e match {
        case fun: Core.FunctionExpression =>
          val funDefs: mutable.Map[Core.Identifier, Core.Expression] = mutable.Map()
          val result = flattenArg(fun.result, funDefs)
          val body = flattenDefinitions(fun.body ++ funDefs)
          fun.copy(body = body.toMap, result = result)
        case app: Core.ApplicationExpression =>
          app.copy(flattenArg(app.applicable, defs), app.args.map(flattenArg(_, defs)))
        case typeApp: Core.TypeApplicationExpression =>
          typeApp.copy(flattenArg(typeApp.applicable, defs))
        case rec: Core.RecordConstructorExpression =>
          rec.copy(rec.entries.map { case (n, (e, loc)) => (n, (flattenArg(e, defs), loc)) })
        case acc: Core.RecordAccessorExpression =>
          acc.copy(target = flattenArg(acc.target, defs))
        case _ => e
      }

    def flattenDefinitions(defs: Map[Core.Identifier, Core.Expression]): Definitions = {
      val addDefs = mutable.Map[Core.Identifier, Core.Expression]()
      val flattened = mapValues(defs)(flattenExpression(_, addDefs))
      addDefs ++ flattened
    }

    val defs = flattenDefinitions(spec.definitions).toMap
    Success(
      Core.Specification(spec.annotations, spec.in, defs, spec.out, IdentifierFactory.count),
      Seq()
    )
  }

}

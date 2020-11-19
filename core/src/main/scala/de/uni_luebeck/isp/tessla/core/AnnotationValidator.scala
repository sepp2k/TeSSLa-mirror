/*
 * Copyright 2020 Institute of Software Engineering and Programming Languages,
 *                University of Lübeck, Germany
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

import de.uni_luebeck.isp.tessla.core.Errors.NonLiteralAnnotationParameter
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core

/**
 * Ensures that annotation parameters are only using constant values.
 * */
object AnnotationValidator extends TranslationPhase[Core.Specification, Core.Specification] {
  override def translate(spec: Core.Specification): TranslationPhase.Result[Core.Specification] = {
    new AnnotationValidatorWorker(spec).translate()
  }

  class AnnotationValidatorWorker(spec: Core.Specification) extends TranslationPhase.Translator[Core.Specification] {
    override protected def translateSpec(): TesslaAST.Core.Specification = {
      val allAnnotations = spec.out.map(_._2) ++ spec.in.toSeq.map(_._2._2) :+ spec.annotations

      allAnnotations.foreach { annotations =>
        annotations.toSeq
          .flatMap {
            case (name, entries) => entries.map(name -> _)
          }
          .foreach {
            case (name, arg) =>
              if (!checkExpressionArg(arg)) {
                error(NonLiteralAnnotationParameter(name, arg))
              }
          }
      }

      spec
    }

    private def checkExpressionArg(arg: Core.ExpressionArg): Boolean = arg match {
      case exp: Core.Expression =>
        exp match {
          case _: Core.StringLiteralExpression | _: Core.IntLiteralExpression | _: Core.FloatLiteralExpression => true
          case record: Core.RecordConstructorExpression =>
            record.entries.values.map(_._1).forall(checkExpressionArg)
          case _ => false
        }
      case _: Core.ExpressionRef => false
    }
  }
}

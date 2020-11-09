/*

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

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing

import scala.annotation.tailrec

/**
 * Class implementing [[TranslationPhase]] for the translation from TeSSLa Core to
 * abstract imperative code
 * The translation of stream functions is performed in StreamCodeGenerator
 * The translation of other expressions in NonStreamCodeGenerator
 */

class TesslaCoreToIntermediate(consoleInterface: Boolean)
    extends TranslationPhase[ExtendedSpecification, SourceListing] {

  /**
   * Function triggering the translation from a TeSSLa AST to a SourceListing of ImpLan statements
   * @param extSpec The TeSSLa Core AST to be translated
   * @return A SourceListing containing imperative code to be translated into concrete syntax and included in a source
   *         skeleton
   */
  override def translate(extSpec: ExtendedSpecification): Result[SourceListing] = {
    val nonStreamCodeGenerator = new NonStreamCodeGenerator(extSpec)
    val streamCodeGenerator = new StreamCodeGenerator(nonStreamCodeGenerator)

    val spec = extSpec.spec
    val in = spec.in
    val definitions = spec.definitions
    val out = spec.out

    @tailrec
    def externResolution(e: ExpressionArg): ExternExpression = {
      e match {
        case e: ExternExpression                                 => e
        case ExpressionRef(id, _, _) if definitions.contains(id) => externResolution(definitions(id))
        case _ =>
          throw Diagnostics.CoreASTError(
            "No extern or reference to extern in function application with stream result",
            e.location
          )
      }
    }

    def getStreamType(id: Identifier): Type = {
      if (definitions.contains(id)) {
        definitions(id).tpe
      } else if (in.contains(id)) {
        in(id)._1
      } else {
        throw Diagnostics.CoreASTError(s"Type of stream $id cannot be found")
      }
    }

    var currSrc = SourceListing(Seq(), Seq(), Seq(), Seq(), Seq())

    //Produce calculation section
    DefinitionOrdering.order(definitions).foreach {
      case (id, definition) =>
        currSrc = definition.tpe match {
          case InstantiatedType("Events", _, _) =>
            definition match {
              case ApplicationExpression(TypeApplicationExpression(e, typeArgs, _), args, _) =>
                streamCodeGenerator.translateExternSignalExpression(id, externResolution(e), args, typeArgs, currSrc)
              case ApplicationExpression(e, args, _) =>
                streamCodeGenerator.translateExternSignalExpression(id, externResolution(e), args, Seq(), currSrc)
              case e =>
                throw Diagnostics.CoreASTError("Non valid stream defining expression cannot be translated", e.location)
            }
          case _ =>
            SourceListing(
              currSrc.stepSource,
              currSrc.tailSource,
              currSrc.tsGenSource,
              currSrc.inputProcessing,
              currSrc.staticSource :+ nonStreamCodeGenerator
                .translateAssignment(id, definition, nonStreamCodeGenerator.TypeArgManagement.empty, definitions)
            )
        }
    }

    //Produce output generation
    out.foreach { o =>
      val name = o._2.get("$name") match {
        case Some(s) =>
          s(0) match {
            case StringLiteralExpression(n, _) => Some(n)
            case _                             => None
          }
        case None => None
      }

      if (consoleInterface) {
        currSrc =
          streamCodeGenerator.produceOutputCode(o._1.id, getStreamType(o._1.id), name, currSrc, o._2.contains("raw"))
      } else {
        throw Diagnostics.NotYetImplementedError("Translation without value output to stdout is not implemented yet")
      }
    }

    //Produce input consumption
    in.foreach { i =>
      if (consoleInterface) {
        currSrc = streamCodeGenerator.produceInputFromConsoleCode(i._1, i._2._1, currSrc)
      } else {
        throw Diagnostics.NotYetImplementedError(
          "Translation without value consumption from stdin is not implemented yet"
        )
      }
      currSrc = streamCodeGenerator.produceInputUnchangeCode(i._1, currSrc)
    }

    Success(currSrc, Seq())
  }

}
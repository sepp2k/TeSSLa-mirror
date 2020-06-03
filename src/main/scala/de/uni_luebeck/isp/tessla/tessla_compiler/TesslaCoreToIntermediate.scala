package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing
import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.TesslaAST.Core._

/**
  * Class implementing de.uni_luebeck.isp.tessla.TranslationPhase for the translation from TeSSLa Core to
  * abstract imperative code
  */

class TesslaCoreToIntermediate(consoleInterface : Boolean) extends
        TranslationPhase[ExtendedSpecification, SourceListing] {

  override def translate(extSpec: ExtendedSpecification): Result[SourceListing] = {
    val spec = extSpec.spec
    val in = spec.in
    val definitions = spec.definitions
    val out = spec.out

    @scala.annotation.tailrec
    def externResolution(e: ExpressionArg): ExternExpression = {
      e match {
        case e: ExternExpression => e
        case ExpressionRef(id, _, _) if definitions.contains(id) => externResolution(definitions(id))
        case _ => throw Errors.CoreASTError("No extern or reference to extern in function application with stream result", e.location)
      }
    }

    def getStreamType(id: Identifier) : Type = {
      if (definitions.contains(id)) {
        definitions(id).tpe
      } else if (in.contains(id)) {
        in(id)._1
      } else {
        throw Errors.CoreASTError(s"Type of stream $id cannot be found")
      }
    }

    var currSrc = SourceListing(Seq(), Seq(), Seq(), Seq(), Seq())

    DefinitionOrdering.order(definitions).foreach{ case (id, definition) =>
      currSrc = definition.tpe match {
        case InstantiatedType("Events", _, _) => definition match {
          case ApplicationExpression(TypeApplicationExpression(e, typeArgs, _), args, _) => StreamCodeGenerator.translateExternSignalExpression(id, externResolution(e), args, typeArgs, currSrc)
          case ApplicationExpression(e, args, _) => StreamCodeGenerator.translateExternSignalExpression(id, externResolution(e), args, Seq(), currSrc) //TODO: Does this exist?
          case e => throw Errors.CoreASTError("Non valid stream defining expression cannot be translated", e.location)
        }
        case _ => SourceListing(currSrc.stepSource, currSrc.tailSource, currSrc.tsGenSource, currSrc.inputProcessing, currSrc.staticSource :+ NonStreamCodeGenerator.translateDefinition(id, definition, NonStreamCodeGenerator.TypeArgManagement.empty, definitions))
      }
    }

    out.foreach {o =>
      val name = o._2.get("name") match {
        case Some(s) => s(0) match {
          case StringLiteralExpression(n, _) => Some(n)
          case _ => None
        }
        case None => None
      }

      if (consoleInterface) {
        currSrc = StreamCodeGenerator.produceOutputCode(o._1.id, getStreamType(o._1.id), name, currSrc)
      } else {
        throw Errors.NotYetImplementedError("Translation without value output to stdout is not implemented yet")
      }
    }

    in.foreach {i =>
      if (consoleInterface) {
        currSrc = StreamCodeGenerator.produceInputFromConsoleCode(i._1, i._2._1, currSrc)
      } else {
        throw Errors.NotYetImplementedError("Translation without value consumption from stdin is not implemented yet")
      }
      currSrc = StreamCodeGenerator.produceInputUnchangeCode(i._1, currSrc)
    }

    Success(currSrc, Seq())
  }

}

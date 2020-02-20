package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing
import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.TesslaCoreWithMutabilityInfo

import scala.collection.immutable.ArraySeq

/**
  * Class implementing de.uni_luebeck.isp.tessla.TranslationPhase for the translation from TeSSLa Core to
  * abstract imperative code
  */
class TesslaCoreToIntermediate(consoleInterface : Boolean) extends
        TranslationPhase[TesslaCoreWithMutabilityInfo, SourceListing] {


  override def translate(tcMut: TesslaCoreWithMutabilityInfo): Result[SourceListing] = {

    val spec = tcMut.spec

    val in = spec.in
    val definitions = spec.definitions
    val out = spec.out

    def getInStreamDefStreamType(id: Identifier) : Type = {
      if (definitions.contains(id)) {
        definitions(id).tpe
      } else if (in.contains(id)) {
        in(id)._1
      } else {
        throw Errors.CoreASTError(s"Type of output stream $id cannot be found")
      }
    }

    var currSource = SourceListing(Seq(), Seq(), Seq(), Seq(), Seq())
    var warnings = Seq()

    DefinitionOrdering.order(definitions).foreach { case (id, definition) => {
      currSource = definition.tpe match {
        case InstatiatedType("Events", _, _) => definition match {
          case ApplicationExpression(TypeApplicationExpression(e: ExternExpression, typeArgs, _), args, _) => translateExternSignalExpression(id, e, args, typeArgs, currSource)
          case ApplicationExpression(e: ExternExpression, args, _) => translateExternSignalExpression(id, e, args, Seq(), currSource) //TODO: Does this exist?
          case e => throw Errors.CoreASTError("Non valid stream defining expression cannot be translated", e.location)
        }
        case _ => SourceListing(currSource.stepSource, currSource.tailSource, currSource.tsGenSource, currSource.inputProcessing, currSource.staticSource :+ NonStreamCodeGenerator.translateDefinition(id, definition, definitions))
      }
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
      currSource = StreamCodeGenerator.produceOutputCode(o._1, getInStreamDefStreamType(o._1), name, currSource)
    }

    in.foreach {i =>
      if (consoleInterface) {
        currSource = StreamCodeGenerator.produceInputFromConsoleCode(i._1, i._2._1, currSource)
      } else {
        throw Errors.NotYetImplementedError("Translation without value consumption from stdin is not implemented yet")
      }
      currSource = StreamCodeGenerator.produceInputUnchangeCode(i._1, currSource)
    }

    Success(currSource, warnings)
  }

  def translateExternSignalExpression(id: Identifier, e: ExternExpression, args: Seq[ExpressionArg], typeArgs: Seq[Type], currSource: SourceListing) : SourceListing = {
    val typeParamMap = e.typeParams.zip(typeArgs).toMap
    e.name match {
      case "nil" =>
        StreamCodeGenerator.produceNilStepCode(id, e.resultType.resolve(typeParamMap), e.location, currSource)
      case "default" =>
        StreamCodeGenerator.produceDefaultStepCode(id, e.resultType.resolve(typeParamMap), args(0), args(1), e.location, currSource)
      case "defaultFrom" =>
        StreamCodeGenerator.produceDefaultFromStepCode(id, e.resultType.resolve(typeParamMap), args(0), args(1), e.location, currSource)
      case "time" =>
        StreamCodeGenerator.produceTimeStepCode(id, args(0), e.location, currSource)
      case "last" =>
        StreamCodeGenerator.produceLastStepCode(id, e.resultType.resolve(typeParamMap), args(0), args(1), e.location, currSource)
      case "delay" =>
        StreamCodeGenerator.produceDelayStepCode(id, args(0), args(1), e.location, currSource)
      case "lift" =>
        StreamCodeGenerator.produceLiftStepCode(id, e.resultType.resolve(typeParamMap), args.dropRight(1), args.last, e.location, currSource)
      case "slift" =>
        StreamCodeGenerator.produceSignalLiftStepCode(id, e.resultType.resolve(typeParamMap), args.dropRight(1), args.last, e.location, currSource)
      case "merge" =>
        StreamCodeGenerator.produceMergeStepCode(id, e.resultType.resolve(typeParamMap), args, e.location, currSource)
      case _ => throw Errors.CommandNotSupportedError(e.toString)
    }
  }

}

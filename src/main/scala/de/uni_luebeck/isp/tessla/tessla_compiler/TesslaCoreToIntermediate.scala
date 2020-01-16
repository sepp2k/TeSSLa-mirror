package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.SourceListing
import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check.TesslaCoreWithMutabilityInfo

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

    var currSource = SourceListing(Seq(), Seq(), Seq())
    var warnings = Seq()

    definitions.foreach { case (id, definition) => {
      currSource = definition match {
        case FunctionExpression(typeParams, params, body, result, location) => throw Errors.NotYetImplementedError("Translation of FunctionExpression not supported yet")
        case e: ExternExpression => throw Errors.NotYetImplementedError("Translation of ApplicationExpression not supported yet")
        case e: ApplicationExpression => translateApplication(id, e, currSource)
        case TypeApplicationExpression(applicable, typeArgs, location) => throw Errors.NotYetImplementedError("Translation of TypeApplicationExpression not supported yet")
        case RecordConstructorExpression(entries, location) => throw Errors.NotYetImplementedError("Translation of RecordConstructorExpression not supported yet")
        case RecordAccesorExpression(name, target, location) => throw Errors.NotYetImplementedError("Translation of RecordAccessorExpression not supported yet")
        case StringLiteralExpression(value, location) => throw Errors.NotYetImplementedError("Translation of StringLiteralExpression not supported yet")
        case IntLiteralExpression(value, location) => throw Errors.NotYetImplementedError("Translation of IntLiteralExpression not supported yet")
        case FloatLiteralExpression(value, location) => throw Errors.NotYetImplementedError("Translation of FloatLiteralExpression not supported yet")
        case _ => throw Errors.CommandNotSupportedError(definition.toString)
      }
    }
    }

    def getType(id: Identifier) : Type = { //TODO: Temporary solution
      definitions.find{case (did, definition) => id == did}.get._2.tpe
    }

    out.foreach {o =>
      currSource = IntermediateCodeGenerator.produceOutputCode(o._1, getType(o._1), o._2, currSource)
    }

    in.foreach {i =>
      if (consoleInterface) {
        currSource = IntermediateCodeGenerator.produceInputFromConsoleCode(i._1, i._2._1, currSource)
      } else {
        throw Errors.NotYetImplementedError("Translation without value consumption from stdin is not implemented yet")
      }
      currSource = IntermediateCodeGenerator.produceInputUnchangeCode(i._1, currSource)
    }

    Success(currSource, warnings)
  }

  def translateApplication(id: Identifier, e: ApplicationExpression, currSource: SourceListing) : SourceListing = {
    e.applicable match {
      case FunctionExpression(typeParams, params, body, result, location) =>
        throw Errors.NotYetImplementedError("Application of FunctionExpression not supported yet")
      case ExternExpression(typeParams, params, resultType, name, location) => name match {
        case "nil"  =>
          throw Errors.NotYetImplementedError("Translation of nil is not supported yet")
        case "default"  =>
          IntermediateCodeGenerator.produceDefaultStepCode(id, resultType, e.args(0), e.args(1), e.location, currSource)
        case "defaultFrom" =>
          IntermediateCodeGenerator.produceDefaultFromStepCode(id, resultType, e.args(0), e.args(1), e.location, currSource)
        case "time" =>
          IntermediateCodeGenerator.produceTimeStepCode(id, e.args(0), e.location, currSource)
        case "last" =>
          IntermediateCodeGenerator.produceLastStepCode(id, resultType, e.args(0), e.args(1), e.location, currSource)
        case "delay" =>
          IntermediateCodeGenerator.produceDelayStepCode(id, e.args(0), e.args(1), e.location, currSource)
        case "lift" =>
          IntermediateCodeGenerator.produceLiftStepCode(id, resultType, e.args.dropRight(1), e.args.last, e.location, currSource)
        case "slift" =>
          IntermediateCodeGenerator.produceSignalLiftStepCode(id, resultType, e.args.dropRight(1), e.args.last, e.location, currSource)
        case "merge" =>
          throw Errors.NotYetImplementedError("Translation of merge is not supported yet")
        case _ => throw Errors.CommandNotSupportedError(e.toString)
      }
      case r: ExpressionRef =>
        throw Errors.NotYetImplementedError("Application of ExpressionRef not supported yet")
      case _ => throw Errors.TranslationError("Application to non-applicable expression", e.location)
    }
  }

}

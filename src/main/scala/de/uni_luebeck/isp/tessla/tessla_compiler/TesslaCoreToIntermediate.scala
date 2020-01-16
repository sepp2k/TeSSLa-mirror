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
        case e: ApplicationExpression => translateExpressionArg(id, e, Seq(), Seq(), currSource)
        case e: TypeApplicationExpression => translateExpressionArg(id, e, Seq(), Seq(), currSource)

        //TODO: Can any of these happen???
        case FunctionExpression(typeParams, params, body, result, location) => throw Errors.NotYetImplementedError("Translation of FunctionExpression not supported yet")
        case _: ExternExpression => throw Errors.NotYetImplementedError("Translation of ExternExpression not supported yet")
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

  def translateExpressionArg(id: Identifier, e: ExpressionArg, args: Seq[ExpressionArg], typeArgs: Seq[Type], currSource: SourceListing) : SourceListing = {
    e match {
      case e: ExternExpression => translateExternExpression(id, e, args, typeArgs, currSource)
      case e: ApplicationExpression => translateExpressionArg(id, e.applicable, e.args, typeArgs, currSource)
      case e: TypeApplicationExpression => translateExpressionArg(id, e.applicable, args, e.typeArgs, currSource)

      //TODO: Can any of these happen???
      case FunctionExpression(typeParams, params, body, result, location) => throw Errors.NotYetImplementedError("Translation of FunctionExpression not supported yet")
      case RecordConstructorExpression(entries, location) => throw Errors.NotYetImplementedError("Translation of RecordConstructorExpression not supported yet")
      case RecordAccesorExpression(name, target, location) => throw Errors.NotYetImplementedError("Translation of RecordAccessorExpression not supported yet")
      case StringLiteralExpression(value, location) => throw Errors.NotYetImplementedError("Translation of StringLiteralExpression not supported yet")
      case IntLiteralExpression(value, location) => throw Errors.NotYetImplementedError("Translation of IntLiteralExpression not supported yet")
      case FloatLiteralExpression(value, location) => throw Errors.NotYetImplementedError("Translation of FloatLiteralExpression not supported yet")
      case _ => throw Errors.CommandNotSupportedError(e.toString)
    }
  }

  def translateExternExpression(id: Identifier, e: ExternExpression, args: Seq[ExpressionArg], typeArgs: Seq[Type], currSource: SourceListing) : SourceListing = {
    val typeParamMap = e.typeParams.zip(typeArgs).toMap
    e.name match {
      case "nil" =>
        throw Errors.NotYetImplementedError("Translation of nil is not supported yet")
      case "default" =>
        IntermediateCodeGenerator.produceDefaultStepCode(id, e.resultType.resolve(typeParamMap), args(0), args(1), e.location, currSource)
      case "defaultFrom" =>
        IntermediateCodeGenerator.produceDefaultFromStepCode(id, e.resultType.resolve(typeParamMap), args(0), args(1), e.location, currSource)
      case "time" =>
        IntermediateCodeGenerator.produceTimeStepCode(id, args(0), e.location, currSource)
      case "last" =>
        IntermediateCodeGenerator.produceLastStepCode(id, e.resultType.resolve(typeParamMap), args(0), args(1), e.location, currSource)
      case "delay" =>
        IntermediateCodeGenerator.produceDelayStepCode(id, args(0), args(1), e.location, currSource)
      case "lift" =>
        IntermediateCodeGenerator.produceLiftStepCode(id, e.resultType.resolve(typeParamMap), args.dropRight(1), args.last, e.location, currSource)
      case "slift" =>
        IntermediateCodeGenerator.produceSignalLiftStepCode(id, e.resultType.resolve(typeParamMap), args.dropRight(1), args.last, e.location, currSource)
      case "merge" =>
        throw Errors.NotYetImplementedError("Translation of merge is not supported yet")
      case _ => throw Errors.CommandNotSupportedError(e.toString)
    }
  }

}

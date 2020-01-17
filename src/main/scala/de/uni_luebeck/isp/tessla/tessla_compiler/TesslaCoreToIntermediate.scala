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


  //TODO: Stream operations can only occur here, but also everything else
  override def translate(tcMut: TesslaCoreWithMutabilityInfo): Result[SourceListing] = {

    val spec = tcMut.spec

    val in = spec.in
    val definitions = spec.definitions
    val out = spec.out

    var currSource = SourceListing(Seq(), Seq(), Seq())
    var warnings = Seq()

    //TODO: Ordering of definitions -> same in Function generator
    definitions.foreach { case (id, definition) => {
      currSource = definition.tpe match {
        case InstatiatedType("Events", _, _) => definition match {
          case ApplicationExpression(TypeApplicationExpression(e: ExternExpression, typeArgs, _), args, _) => translateExternSignalExpression(id, e, args, typeArgs, currSource)
          case ApplicationExpression(e: ExternExpression, args, _) => translateExternSignalExpression(id, e, args, Seq(), currSource) //TODO: Does this exist?
          case e => throw Errors.TranslationError("Non valid stream defining expression cannot be translated", e.location)
        }
        case _ => definition match {
          case e: FunctionExpression => {FunctionGenerator.translateFunction(e); currSource} //TODO
          case ExternExpression(typeParams, params, resultType, name, location) => ???
          case ApplicationExpression(applicable, args, location) => ???
          case TypeApplicationExpression(applicable, typeArgs, location) => ???
          case RecordConstructorExpression(entries, location) => ???
          case RecordAccesorExpression(name, target, location) => ???
          case StringLiteralExpression(value, location) => ???
          case IntLiteralExpression(value, location) => ???
          case FloatLiteralExpression(value, location) => ???
        }
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

  def translateExternSignalExpression(id: Identifier, e: ExternExpression, args: Seq[ExpressionArg], typeArgs: Seq[Type], currSource: SourceListing) : SourceListing = {
    val typeParamMap = e.typeParams.zip(typeArgs).toMap
    e.name match {
      case "nil" =>
        IntermediateCodeGenerator.produceNilStepCode(id, e.resultType.resolve(typeParamMap), e.location, currSource)
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
        IntermediateCodeGenerator.produceMergeStepCode(id, e.resultType.resolve(typeParamMap), args, e.location, currSource)
      case _ => throw Errors.CommandNotSupportedError(e.toString)
    }
  }

}

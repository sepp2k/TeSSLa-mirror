package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{DoubleValue, FunctionCall, FunctionType, FunctionVarApplication, ImpLanExpr, ImpLanStmt, LongValue, SourceListing, StringValue}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeDSL._

import scala.language.implicitConversions
import scala.language.postfixOps

/**
  * Class for the translation of TeSSLaCore-Functions to ImpLan Lambda expressions
  */
object FunctionGenerator {

  def translateFunctionCall(e: ExpressionArg, args: Seq[ImpLanExpr], typeArgs: Seq[Type]) : ImpLanExpr = {
    e match {
      case ExternExpression(typeParams, params, resultType, name, location) => {
        val typeParamMap = typeParams.zip(typeArgs).toMap
        FunctionCall(s"__${name}__", args, FunctionType(params.map{case (_,t) => IntermediateCodeDSL.typeConversion(t.resolve(typeParamMap))},IntermediateCodeDSL.typeConversion(resultType.resolve(typeParamMap)))) //TODO: KW check
      }
      case f: FunctionExpression => {
        val typeParamMap = f.typeParams.zip(typeArgs).toMap
        FunctionCall(s"__${translateFunction(f)}__", args, FunctionType(f.params.map{case (_,_,t) => IntermediateCodeDSL.typeConversion(t.resolve(typeParamMap))},IntermediateCodeDSL.typeConversion(f.result.tpe.resolve(typeParamMap)))) //TODO: KW check
      }
      case ExpressionRef(id, tpe, location) => FunctionVarApplication(s"var_${id.fullName}", args)
      case ApplicationExpression(applicable, args, _) => translateFunctionCall(applicable, args.map(translateExpressionArg), typeArgs)//TODO: Nested Calls???
      case TypeApplicationExpression(applicable, typeArgs, _) => translateFunctionCall(applicable, args, typeArgs)//TODO: Nested Calls???

      case e => throw Errors.CommandNotSupportedError("Non function expression cannot be translated", e.location)
    }
  }

  //TODO: Recursion detection ???
  //TODO: Where to put the translated function???
  def translateFunction(e: FunctionExpression) : String = {
    println(translateBody(e.body.toSeq).mkString("\n")) //TODO: Order body
    "..."//TODO: Function naming
  }

  def translateBody(body: Seq[(Identifier, DefinitionExpression)]) : Seq[ImpLanStmt] = {
    body.foldLeft[Seq[ImpLanStmt]](Seq()){
      case (curr, (id, exp)) => curr.Assignment(s"var_${id.fullName}", translateExpressionArg(exp), defaultValueForType(exp.tpe), exp.tpe)
    }
  }

  //TODO: Maybe put to more general place
  def translateExpressionArg(e: ExpressionArg): ImpLanExpr = {
    //TODO: Boolean translation
    e match {
      case FunctionExpression(typeParams, params, body, result, location) => ???
      case ExternExpression(typeParams, params, resultType, name, location) => ???
      case ApplicationExpression(applicable, args, _) => translateFunctionCall(applicable, args.map(translateExpressionArg), Seq())
      case TypeApplicationExpression(applicable, typeArgs, _) => translateFunctionCall(applicable, Seq(), typeArgs)
      case RecordConstructorExpression(entries, location) => ???
      case RecordAccesorExpression(name, target, location) => ???
      case StringLiteralExpression(value, _) => StringValue(value)
      case IntLiteralExpression(value, _) => LongValue(value.toLong)
      case FloatLiteralExpression(value, _) => DoubleValue(value)
      case ExpressionRef(id, _, _) => s"var_${id.fullName}"
    }
  }

}

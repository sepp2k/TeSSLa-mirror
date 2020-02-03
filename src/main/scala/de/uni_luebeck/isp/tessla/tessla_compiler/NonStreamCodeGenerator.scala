package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeDSL._

import scala.language.implicitConversions
import scala.language.postfixOps

/**
  * Class for the translation of TeSSLaCore-Functions to ImpLan Lambda expressions
  */
object NonStreamCodeGenerator {

  def translateFunctionCall(e: ExpressionArg, args: Seq[ImpLanExpr], typeArgs: Seq[Type]) : ImpLanExpr = {
    e match {
      case ExternExpression(_, _, _, "true", _) => BoolValue(true)
      case ExternExpression(_, _, _, "false", _) => BoolValue(false)
      case ExternExpression(tps, _, InstatiatedType("Option", Seq(t), _), "None", _) => None(t.resolve(tps.zip(typeArgs).toMap))
      case ExternExpression(typeParams, params, resultType, name, _) => {
        val typeParamMap = typeParams.zip(typeArgs).toMap
        FunctionCall(s"__${name}__", args, FunctionType(params.map{case (_,t) => IntermediateCodeDSL.typeConversion(t.resolve(typeParamMap))},IntermediateCodeDSL.typeConversion(resultType.resolve(typeParamMap))))
      }
      case _: FunctionExpression |
           _: ExpressionRef |
           ApplicationExpression(TypeApplicationExpression(_, _, _), _, _) => LambdaApplication(translateExpressionArg(e), args)
      case e => throw Errors.CommandNotSupportedError("Non function expression cannot be translated", e.location)
    }
  }

  def translateDefinition(id: Identifier, e: ExpressionArg) : ImpLanStmt = {
    Assignment(s"var_$id", translateExpressionArg(e), scala.Some(defaultValueForType(e.tpe)), e.tpe)
  }

  //TODO: Recursion detection ???
  def translateFunction(e: FunctionExpression) : ImpLanExpr = {
    //TODO: Possibly type param resolution
    //Fix for avoiding generic types where not necessary
      val resType = e.result match {
        case ExpressionRef(id, tpe, location) => e.body(id).tpe
        case _ => e.result.tpe
      }
      LambdaExpression(e.params.map{case (id,_,_) => s"var_$id"}, e.params.map{case (_,_,t) => t}, resType, translateBody(e.body, e.result))
  }

  def translateBody(body: Map[Identifier, DefinitionExpression], ret: ExpressionArg) : Seq[ImpLanStmt] = {
    val translatedBody = DefinitionOrdering.order(body).foldLeft[Seq[ImpLanStmt]](Seq()){
      case (curr, (id, exp)) => curr.Assignment(s"var_${id.fullName}", translateExpressionArg(exp), scala.None, exp.tpe)
    }
    translatedBody :+ ReturnStatement(translateExpressionArg(ret))
  }

  def translateExpressionArg(e: ExpressionArg): ImpLanExpr = {
    //TODO: Plain application expression
    e match {
      case f: FunctionExpression => translateFunction(f)
      case ApplicationExpression(TypeApplicationExpression(e, typeArgs, _), args, _) => translateFunctionCall(e, args.map(translateExpressionArg), typeArgs)
      case StringLiteralExpression(value, _) => StringValue(value)
      case IntLiteralExpression(value, _) => LongValue(value.toLong)
      case FloatLiteralExpression(value, _) => DoubleValue(value)
      case ExpressionRef(id, _, _) => s"var_${id.fullName}"
      case ExternExpression(typeParams, params, resultType, name, location) => ???
      case RecordConstructorExpression(entries, location) => ???
      case RecordAccesorExpression(name, target, location) => ???
      case _ => ???
    }
  }

}

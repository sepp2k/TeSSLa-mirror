package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils._

import scala.language.implicitConversions
import scala.language.postfixOps

/**
  * Class for the translation of TeSSLaCore-Functions to ImpLan Lambda expressions
  */
object NonStreamCodeGenerator {

  def translateFunctionCall(e: ExpressionArg, args: Seq[ImpLanExpr], typeArgs: Seq[Type], defContext: Map[Identifier, DefinitionExpression] = Map()) : ImpLanExpr = {
    e match {
      case TypeApplicationExpression(app, types, _) => translateFunctionCall(app, args, typeArgs ++ types, defContext)
      case ExternExpression(_, _, _, "true", _) => BoolValue(true)
      case ExternExpression(_, _, _, "false", _) => BoolValue(false)
      case ExternExpression(tps, _, InstantiatedType("Option", Seq(t), _), "None", _) => None(t.resolve(tps.zip(typeArgs).toMap))
      case ExternExpression(typeParams, params, resultType, name, _) => {
        val typeParamMap = typeParams.zip(typeArgs).toMap
        FunctionCall(s"__${name}__", args, FunctionType(params.map{case (_,t) => IntermediateCodeUtils.typeConversion(t.resolve(typeParamMap))}, IntermediateCodeUtils.typeConversion(resultType.resolve(typeParamMap))))
      }
      case _: FunctionExpression |
           _: ExpressionRef |
           ApplicationExpression(TypeApplicationExpression(_, _, _), _, _) => LambdaApplication(translateExpressionArg(e, defContext), args)
      case e => throw Errors.CoreASTError("Non function expression cannot be translated", e.location)
    }
  }

  def translateDefinition(id: Identifier, e: ExpressionArg, defContext: Map[Identifier, DefinitionExpression]) : ImpLanStmt = {
    Assignment(s"var_$id", translateExpressionArg(e, defContext), scala.Some(defaultValueForType(e.tpe)), e.tpe)
  }

  def translateFunction(e: FunctionExpression, defContext: Map[Identifier, DefinitionExpression]) : ImpLanExpr = {
    //TODO: Possibly type param resolution
      LambdaExpression(e.params.map{case (id,_,_) => if (id.fullName == "_")  "_" else s"var_$id"}, e.params.map{case (_,_,t) => t}, e.result.tpe, translateBody(e.body, e.result, defContext))
  }

  def translateBody(body: Map[Identifier, DefinitionExpression], ret: ExpressionArg, defContext: Map[Identifier, DefinitionExpression]) : Seq[ImpLanStmt] = {
    val newDefContext = defContext ++ body

    val translatedBody = DefinitionOrdering.order(body).foldLeft[Seq[ImpLanStmt]](Seq()){
      case (curr, (id, exp)) => curr :+ translateDefinition(id, exp, newDefContext)
    }
    translatedBody :+ ReturnStatement(translateExpressionArg(ret, newDefContext))
  }

  def translateExpressionArg(e: ExpressionArg, defContext: Map[Identifier, DefinitionExpression] = Map()): ImpLanExpr = {
    //TODO: Plain application expression
    e match {
      case f: FunctionExpression => translateFunction(f, defContext)
      case ApplicationExpression(TypeApplicationExpression(e, typeArgs, _), args, _) => {
        val inlinedArgs  = e match {
          case ExternExpression(_, _, _, "ite", _) |
               ExternExpression(_, _, _, "staticite", _) => args.map(reInlineTempVars(_, defContext))
          case _ => args
        }
        translateFunctionCall(e, inlinedArgs.map(translateExpressionArg(_, defContext)), typeArgs, defContext)
      }
      case StringLiteralExpression(value, _) => StringValue(value)
      case IntLiteralExpression(value, _) => LongValue(value.toLong)
      case FloatLiteralExpression(value, _) => DoubleValue(value)
      case ExpressionRef(id, _, _) => s"var_${id.fullName}"
      case ExternExpression(typeParams, params, resultType, name, location) => ???
      case RecordConstructorExpression(entries, _) if entries.isEmpty => UnitValue
      case RecordConstructorExpression(entries, _) => MkStruct(entries.toSeq.map{case (n, (ea, _)) => (n.name, translateExpressionArg(ea, defContext))}, e.tpe)
      case RecordAccessorExpression(name, target, _, _) => GetStruct(translateExpressionArg(target), name.name, target.tpe)
      case _ => ???
    }
  }


  def reInlineTempVars(e: ExpressionArg, defContext: Map[Identifier, DefinitionExpression]) : ExpressionArg = {
    e match {
      case e: Expression => reInlineTempVars(e, defContext)
      case ExpressionRef(id, _, _) if id.idOrName.left.isEmpty && defContext.contains(id) => reInlineTempVars(defContext(id), defContext)
      case _ => e
    }
  }

  def reInlineTempVars(e: DefinitionExpression, defContext: Map[Identifier, DefinitionExpression]) : DefinitionExpression = {
    e match {
      case e: Expression => e match {
        case FunctionExpression(typeParams, params, body, result, location) => FunctionExpression(typeParams, params, reInlineTempVarsBody(body, defContext), reInlineTempVars(result, defContext), location)
        case ApplicationExpression(applicable, args, location) => ApplicationExpression(reInlineTempVars(applicable, defContext), args.map{a => reInlineTempVars(a, defContext)}, location)
        case TypeApplicationExpression(applicable, typeArgs, location) => TypeApplicationExpression(reInlineTempVars(applicable, defContext), typeArgs, location)
        case RecordConstructorExpression(entries, location) => RecordConstructorExpression(entries.map{case (n,(e,l)) => (n, (reInlineTempVars(e, defContext), l))}, location)
        case RecordAccessorExpression(name, target, nameLoc, location) => RecordAccessorExpression(name, reInlineTempVars(target, defContext), nameLoc, location)
        case _ => e
      }
      case _ => e
    }
  }

  def reInlineTempVarsBody(b: Map[Identifier, DefinitionExpression], defContext: Map[Identifier, DefinitionExpression]) : Map[Identifier, DefinitionExpression] = {
      b.map {case (id, de) => (id, reInlineTempVars(de, defContext))}
   }

}

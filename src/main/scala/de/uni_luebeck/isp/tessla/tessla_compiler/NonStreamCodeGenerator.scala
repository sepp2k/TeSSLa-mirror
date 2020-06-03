package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.{Core, LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils._

import scala.language.implicitConversions
import scala.language.postfixOps

/**
  * Class for the translation of TeSSLaCore-Functions to ImpLan Lambda expressions
  */
class NonStreamCodeGenerator(extSpec: ExtendedSpecification) {

  object TypeArgManagement {
    def empty: TypeArgManagement = TypeArgManagement(Map(), Seq())
  }

  case class TypeArgManagement(resMap: Map[Identifier, Type], unappliedArgs: Seq[Type]) {
    def typeApp(types: Seq[Type]): TypeArgManagement = {
      TypeArgManagement(resMap, types)
    }

    def parsKnown(pars: Seq[Identifier]): TypeArgManagement = {
      TypeArgManagement(resMap.removedAll(pars) ++ pars.zip(unappliedArgs).toMap, Seq())
    }
  }

  def translateFunctionCall(e: ExpressionArg, args: Seq[ImpLanExpr], tm: TypeArgManagement, defContext: Map[Identifier, DefinitionExpression] = Map()) : ImpLanExpr = {
    e match {
      case TypeApplicationExpression(app, types, _) =>
        translateFunctionCall(app, args, tm.typeApp(types), defContext)
      case ExternExpression(_, _, _, "true", _) =>
        BoolValue(true)
      case ExternExpression(_, _, _, "false", _) =>
        BoolValue(false)
      case ExternExpression(tPars, _, InstantiatedType("Option", Seq(t), _), "None", _) =>
        None(t.resolve(tm.parsKnown(tPars).resMap))
      case ExternExpression(tPars, params, resultType, name, _) =>
        val newTm = tm.parsKnown(tPars)
        FunctionCall(s"__${name}__",
                     args,
                     FunctionType(params.map{case (_,t) => IntermediateCodeUtils.typeConversion(t.resolve(newTm.resMap))},
                                  IntermediateCodeUtils.typeConversion(resultType.resolve(newTm.resMap))))
      case _: FunctionExpression |
           _: ExpressionRef |
           _: ApplicationExpression |
           _: RecordAccessorExpression =>
        LambdaApplication(translateExpressionArg(e, tm, defContext), args)
      case e => throw Errors.CoreASTError("Function call to expression of wrong type cannot be translated", e.location)
    }
  }

  def translateDefinition(id: Identifier, e: ExpressionArg, tm: TypeArgManagement, defContext: Map[Identifier, DefinitionExpression]) : ImpLanStmt = {
    val resTpe = e.tpe.resolve(tm.resMap)
    if (extSpec.lazyVars.get.contains(id)) {
      FinalAssignment(s"var_$id", translateExpressionArg(e, tm, defContext), resTpe, true)
    } else {
      Assignment(s"var_$id", translateExpressionArg(e, tm, defContext), scala.Some(defaultValueForType(resTpe)), resTpe)
    }
  }

  def translateFunction(e: FunctionExpression, tm: TypeArgManagement, defContext: Map[Identifier, DefinitionExpression]) : ImpLanExpr = {
      val newTm = tm.parsKnown(e.typeParams)
      LambdaExpression(e.params.map{case (id,_,_) => if (id.fullName == "_")  "_" else s"var_$id"},
                       e.params.map(_._3.resolve(newTm.resMap)),
                       e.result.tpe.resolve(newTm.resMap),
                       translateBody(e.body, e.result, newTm, defContext))
  }

  def translateBody(body: Map[Identifier, DefinitionExpression], ret: ExpressionArg, tm: TypeArgManagement, defContext: Map[Identifier, DefinitionExpression]) : Seq[ImpLanStmt] = {
    val newDefContext = defContext ++ body

    val translatedBody = DefinitionOrdering.order(body).foldLeft[Seq[ImpLanStmt]](Seq()){
      case (curr, (id, exp)) => curr :+ translateDefinition(id, exp, tm, newDefContext)
    }
    translatedBody :+ ReturnStatement(translateExpressionArg(ret, tm, newDefContext))
  }

  def translateExternToLambda(e: ExternExpression, tm: TypeArgManagement, defContext: Map[Identifier, DefinitionExpression]) : ImpLanExpr = {
      val newTm = tm.parsKnown(e.typeParams)
      val argNames = e.params.indices.map(i => s"tLPar_$i")
      val argTypes = e.params.map(_._2.resolve(newTm.resMap)).map(IntermediateCodeUtils.typeConversion)
      val ret = translateFunctionCall(e, argNames.map(Variable), newTm, defContext)
      LambdaExpression(argNames, argTypes, e.resultType.resolve(newTm.resMap), Seq(ReturnStatement(ret)))
  }

  def translateExpressionArg(e: ExpressionArg, tm: TypeArgManagement, defContext: Map[Identifier, DefinitionExpression] = Map()): ImpLanExpr = {
    e match {
      case TypeApplicationExpression(e, tArgs, _) =>
        translateExpressionArg(e, tm.typeApp(tArgs), defContext)
      case f: FunctionExpression =>
        translateFunction(f, tm, defContext)
      case ApplicationExpression(e, args, _) =>
        translateFunctionCall(e, getInlinedArgs(e, args, defContext).map(translateExpressionArg(_, tm, defContext)), tm, defContext)
      case StringLiteralExpression(value, _) =>
        StringValue(value)
      case IntLiteralExpression(value, _) =>
        LongValue(value.toLong)
      case FloatLiteralExpression(value, _) =>
        DoubleValue(value)
      case ExpressionRef(id, _, _) =>
        s"var_${id.fullName}"
      case x: ExternExpression =>
        translateExternToLambda(x, tm, defContext)
      case RecordConstructorExpression(entries, _) if entries.isEmpty =>
        UnitValue
      case RecordConstructorExpression(entries, _) =>
        MkStruct(entries.toSeq.map{case (n, (ea, _)) => (n.name, translateExpressionArg(ea, tm, defContext))}, e.tpe)
      case RecordAccessorExpression(name, target, _, _) =>
        GetStruct(translateExpressionArg(target, tm, defContext), name.name, target.tpe)
      case _ =>
        throw Errors.CoreASTError("Unexpected ExpressionArg cannot be translated", e.location)
    }
  }

  def getInlinedArgs(e: ExpressionArg, args: Seq[ExpressionArg], defContext: Map[Identifier, DefinitionExpression] = Map()): Seq[ExpressionArg] = {
    e.tpe match {
      case Core.FunctionType(_, pars, _, _) => pars.map(_._1).zip(args).map{
        case (StrictEvaluation, e) => e
        case (LazyEvaluation, e) => inlineVars(e, defContext)
      }
      case _ => args
    }
  }

  def inlineVars(e: ExpressionArg, defContext: Map[Identifier, DefinitionExpression]) : ExpressionArg = {
    e match {
      case e: Expression => inlineVars(e, defContext)
      case ExpressionRef(id, tpe, _) if defContext.contains(id) && !tpe.isInstanceOf[Core.FunctionType] => inlineVars(defContext(id), defContext)
      case _ => e
    }
  }

  def inlineVars(e: DefinitionExpression, defContext: Map[Identifier, DefinitionExpression]) : DefinitionExpression = {
    e match {
      case e: Expression => e match {
        case FunctionExpression(typeParams, params, body, result, location) => FunctionExpression(typeParams, params, inlineVarsBody(body, defContext), inlineVars(result, defContext), location)
        case ApplicationExpression(applicable, args, location) => ApplicationExpression(inlineVars(applicable, defContext), args.map{ a => inlineVars(a, defContext)}, location)
        case TypeApplicationExpression(applicable, typeArgs, location) => TypeApplicationExpression(inlineVars(applicable, defContext), typeArgs, location)
        case RecordConstructorExpression(entries, location) => RecordConstructorExpression(entries.map{case (n,(e,l)) => (n, (inlineVars(e, defContext), l))}, location)
        case RecordAccessorExpression(name, target, nameLoc, location) => RecordAccessorExpression(name, inlineVars(target, defContext), nameLoc, location)
        case _ => e
      }
      case _ => e
    }
  }

  def inlineVarsBody(b: Map[Identifier, DefinitionExpression], defContext: Map[Identifier, DefinitionExpression]) : Map[Identifier, DefinitionExpression] = {
      b.map {case (id, de) => (id, inlineVars(de, defContext))}
   }

}

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

/**
  * Class containing code for determining the type of ImpLan-Expressions
  */

object IntermediateCodeTypeInference {

  //Note: Does no type checking
  def typeInference(impLanExpr: ImpLanExpr, varTypes: Map[String, ImpLanType]): ImpLanType = {
    impLanExpr match {
      case CastingExpression(_, targetType) => targetType
      case LongValue(_) => LongType
      case DoubleValue(_) => DoubleType
      case BoolValue(_) => BoolType
      case UnitValue => UnitType
      case StringValue(_) => StringType
      case GeneralValue => GeneralType
      case EmptyFunction(typeHint) => typeHint
      case None(typeHint) => OptionType(typeHint)
      case Some(content) => OptionType(typeInference(content, varTypes))
      case EmptyMutableSet(valType) => MutableSetType(valType)
      case EmptyImmutableSet(valType) => ImmutableSetType(valType)
      case EmptyMutableMap(keyType, valType) => MutableMapType(keyType, valType)
      case EmptyImmutableMap(keyType, valType) => ImmutableMapType(keyType, valType)
      case EmptyMutableList(valType) => MutableListType(valType)
      case EmptyImmutableList(valType) => ImmutableListType(valType)
      case FunctionCall(_, _, typeHint) => typeHint.retType
      case TernaryExpression(_, e1, e2) => getLeastCommonType(typeInference(e1, varTypes), typeInference(e2, varTypes), (_, _) => true)
      case Equal(_, _) => BoolType
      case Variable(name) if varTypes.contains(name) => varTypes(name)
      case Variable(name) => throw Errors.TypeError(s"Type of used variable $name is unknown")
      case LambdaExpression(_, argsTypes, retType, _) => FunctionType(argsTypes, retType)
      case LambdaApplication(exp, _) => {
        typeInference(exp, varTypes) match {
          case FunctionType(_, r) => r
          case _ => throw Errors.TypeError(s"Non-Lambda expression is used in application: $exp")
        }
      }
    }
      }

      def castingNecessary(e1_type: ImpLanType, e2_type: ImpLanType): Boolean = {
        if (e1_type == e2_type || e1_type == GeneralType) {
          false
        } else if (e1_type.getClass == e2_type.getClass) {
          e1_type match {
            case t1: GenericImpLanType => e2_type match {
                case t2: GenericImpLanType => t1.genTypes.zip(t2.genTypes).map{case (a,b) => castingNecessary(a,b)}.reduce(_ || _)
                case _ => true
              }
            case _ => false
          }
        } else {
          true
        }
      }

      def generateCodeWithCasts(listing: SourceListing, varTypes: Map[String, ImpLanType], languageSpecificCastRequired: (ImpLanType, ImpLanType) => Boolean) : SourceListing = {
        SourceListing(generateCodeWithCasts(listing.stepSource, varTypes, languageSpecificCastRequired),
                      generateCodeWithCasts(listing.tsGenSource, varTypes, languageSpecificCastRequired),
                      generateCodeWithCasts(listing.inputProcessing, varTypes, languageSpecificCastRequired),
                      generateCodeWithCasts(listing.staticSource, varTypes, languageSpecificCastRequired))
      }

      def generateCodeWithCasts(stmts: Seq[ImpLanStmt], varTypes: Map[String, ImpLanType], languageSpecificCastRequired: (ImpLanType, ImpLanType) => Boolean, retType: Option[ImpLanType] = scala.None) : Seq[ImpLanStmt] = {
          stmts.map {
            case expr: ImpLanExpr => castExpression(expr, scala.None, varTypes, languageSpecificCastRequired)
            case If(guard, stmts, elseStmts) => If (guard.map{_.map(e => castExpression(e, scala.Some(BoolType), varTypes, languageSpecificCastRequired))}, generateCodeWithCasts(stmts, varTypes, languageSpecificCastRequired, retType), generateCodeWithCasts(elseStmts, varTypes, languageSpecificCastRequired, retType))
            case TryCatchBlock(tr, cat) => TryCatchBlock(generateCodeWithCasts(tr, varTypes, languageSpecificCastRequired, retType), generateCodeWithCasts(cat, varTypes + ("var_err" -> GeneralType), languageSpecificCastRequired, retType))
            case Assignment(lhs, rexpr, defVal, typ) => Assignment(lhs, castExpression(rexpr, scala.Some(typ), varTypes, languageSpecificCastRequired), defVal, typ)
            case FinalAssignment(lhs, defVal, typ) => FinalAssignment(lhs, castExpression(defVal, scala.Some(typ), varTypes, languageSpecificCastRequired), typ)
            case ReturnStatement(expr) if retType.isDefined => ReturnStatement(castExpression(expr, scala.Some(retType.get), varTypes, languageSpecificCastRequired))
            case ReturnStatement(expr) => ReturnStatement(expr)
          }
      }

      def castExpression(exp: ImpLanExpr, target: Option[ImpLanType], varTypes: Map[String, ImpLanType], languageSpecificCastRequired: (ImpLanType, ImpLanType) => Boolean) : ImpLanExpr = {
        val innerExp : ImpLanExpr = exp match {
          case lanVal: ImpLanVal => lanVal
          case CastingExpression(e, target) => CastingExpression(castExpression(e, scala.None, varTypes, languageSpecificCastRequired), target)
          case FunctionCall(name, params, typeHint) => FunctionCall(name, params.zip(typeHint.argsTypes).map{case (e,t) => castExpression(e, scala.Some(t), varTypes, languageSpecificCastRequired)}, typeHint)
          case LambdaApplication(exp, params) => typeInference(exp, varTypes) match {
            case FunctionType(argsTypes, _) => LambdaApplication(exp, params.zip(argsTypes).map{case (e,t) => castExpression(e, scala.Some(t), varTypes, languageSpecificCastRequired)})
            case _ => throw Errors.TranslationError(s"Lambda Application to non-function expression $exp")
          }
          case Equal(a, b) => {
            val lct = scala.Some(getLeastCommonType(typeInference(a, varTypes), typeInference(b, varTypes), languageSpecificCastRequired))
            Equal(castExpression(a, lct, varTypes, languageSpecificCastRequired), castExpression(b, lct, varTypes, languageSpecificCastRequired))
          }
          case TernaryExpression(guard, e1, e2) => {
            val lct = scala.Some(getLeastCommonType(typeInference(e1, varTypes), typeInference(e2, varTypes), languageSpecificCastRequired))
            TernaryExpression(guard.map{_.map { c => castExpression(c, scala.Some(BoolType), varTypes, languageSpecificCastRequired)}}, castExpression(e1, lct, varTypes, languageSpecificCastRequired), castExpression(e2, lct, varTypes, languageSpecificCastRequired))
          }
          case Variable(name) => Variable(name)
          case LambdaExpression(argNames, argsTypes, retType, body) => LambdaExpression(argNames, argsTypes, retType,
              generateCodeWithCasts(
                body,
                IntermediateCodeUtils.getVariableMap(body, varTypes.view.mapValues{a => (a, scala.None)}.toMap ++ argNames.zip(argsTypes).map{case (a,b) => (a,(b, scala.None))}.toMap).view.mapValues{case (a,_) => a}.toMap,
                languageSpecificCastRequired,
                scala.Some(retType)
              ))
        }

        target match {
          case scala.None => innerExp
          case scala.Some(t) => {
            val actualType = typeInference(innerExp, varTypes)
            if (t == actualType || !languageSpecificCastRequired(t, actualType) || !castingNecessary(t, actualType)) {
              innerExp
            } else {
              CastingExpression(innerExp, t)
            }
          }
        }
      }

    def getLeastCommonType(t1: ImpLanType, t2: ImpLanType, languageSpecificCastRequired: (ImpLanType, ImpLanType) => Boolean) : ImpLanType = {
      if (t1 == t2 || !languageSpecificCastRequired(t1, t2) || castingNecessary(t1, t2)) {
          t1
      } else {
        //Note: No type checking here
        t2
      }
    }
}

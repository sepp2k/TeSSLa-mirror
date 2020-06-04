package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

/**
  * Class containing code for determining the type of ImpLan-Expressions
  */

object IntermediateCodeTypeInference {

  //Note: Does no type checking
  def typeInference(impLanExpr: ImpLanExpr, varTypes: Map[String, ImpLanType]): ImpLanType = {
    impLanExpr match {
      case CastingExpression(_, _, targetType) => targetType
      case LongValue(_) => LongType
      case DoubleValue(_) => DoubleType
      case BoolValue(_) => BoolType
      case UnitValue => UnitType
      case StringValue(_) => StringType
      case GeneralValue => GeneralType
      case StructValue(vals) => {
        val orderedVals = vals.toSeq.sortWith { case ((n1, _), (n2, _)) => IntermediateCodeUtils.structComparation(n1, n2) }
        StructType(orderedVals.map { case (_, v) => typeInference(v, varTypes) }, orderedVals.map(_._1))
      }
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
      case TernaryExpression(_, e1, e2) => getLeastCommonType(typeInference(e1, varTypes), typeInference(e2, varTypes))
      case Equal(_, _) => BoolType
      case Variable(name) if varTypes.contains(name) => varTypes(name)
      case Variable(name) => throw Errors.DSLTypeError(s"Type of used variable $name is unknown")
      case LambdaExpression(_, argsTypes, retType, _) => FunctionType(argsTypes, retType)
      case LambdaApplication(exp, _) => {
        typeInference(exp, varTypes) match {
          case FunctionType(_, r) => r
          case _ => throw Errors.DSLTypeError(s"Non function expression is used in application: $exp")
        }
      }
    }
  }

      def castingNecessary(e1_type: ImpLanType, e2_type: ImpLanType): Boolean = {
        if (e1_type.isInstanceOf[LazyContainer] ^ e2_type.isInstanceOf[LazyContainer]) {
          true
        } else if (e1_type == e2_type || e1_type == GeneralType) {
          false
        } else {
          e1_type match {
            case f1: FunctionType => e2_type match {
              case f2: FunctionType => f2.argsTypes.zip(f1.argsTypes).appended((f1.retType, f2.retType)).map{case (a,b) => castingNecessary(a,b)}.reduce(_ || _)
              case _ => true
            }
            case _ => true
          }
        }
      }

      def generateCodeWithCasts(listing: SourceListing, varTypes: Map[String, ImpLanType]) : SourceListing = {
        listing.mapAll(generateCodeWithCasts(_, varTypes))
      }

      def generateCodeWithCasts(stmts: Seq[ImpLanStmt], varTypes: Map[String, ImpLanType], retType: Option[ImpLanType] = scala.None) : Seq[ImpLanStmt] = {
          stmts.map {
            case expr: ImpLanExpr => castExpression(expr, scala.None, varTypes)
            case If(guard, stmts, elseStmts) => If (guard.map{_.map(e => castExpression(e, scala.Some(BoolType), varTypes))}, generateCodeWithCasts(stmts, varTypes, retType), generateCodeWithCasts(elseStmts, varTypes, retType))
            case TryCatchBlock(tr, cat) => TryCatchBlock(generateCodeWithCasts(tr, varTypes, retType), generateCodeWithCasts(cat, varTypes + ("var_err" -> GeneralType), retType))
            case Assignment(lhs, rexpr, defVal, typ) => Assignment(lhs, castExpression(rexpr, scala.Some(typ), varTypes), defVal, typ)
            case FinalAssignment(lhs, defVal, typ, ld) => FinalAssignment(lhs, castExpression(defVal, scala.Some(typ), varTypes), typ, ld)
            case ReturnStatement(expr) if retType.isDefined => ReturnStatement(castExpression(expr, scala.Some(retType.get), varTypes))
            case ReturnStatement(expr) => ReturnStatement(expr)
          }
      }

      def castExpression(exp: ImpLanExpr, target: Option[ImpLanType], varTypes: Map[String, ImpLanType]) : ImpLanExpr = {
        val innerExp : ImpLanExpr = exp match {
          case CastingExpression(e, from, target) => CastingExpression(castExpression(e, scala.Some(from), varTypes), from, target)
          case FunctionCall(name, params, typeHint) =>
            val np = params.zip(typeHint.argsTypes).map{case (e,t) => castExpression(e, scala.Some(t), varTypes)}
            FunctionCall(name, params.zip(typeHint.argsTypes).map{case (e,t) => castExpression(e, scala.Some(t), varTypes)}, typeHint)
          case LambdaApplication(exp, params) => typeInference(exp, varTypes) match {
            case FunctionType(argsTypes, _) => LambdaApplication(castExpression(exp, scala.None, varTypes), params.zip(argsTypes).map{case (e,t) => castExpression(e, scala.Some(t), varTypes)})
            case _ => throw Errors.DSLTypeError(s"Lambda Application to non-function expression $exp")
          }
          case Equal(a, b) => {
            val lct = scala.Some(getLeastCommonType(typeInference(a, varTypes), typeInference(b, varTypes)))
            Equal(castExpression(a, lct, varTypes), castExpression(b, lct, varTypes))
          }
          case TernaryExpression(guard, e1, e2) => {
            val lct = scala.Some(getLeastCommonType(typeInference(e1, varTypes), typeInference(e2, varTypes)))
            TernaryExpression(guard.map{_.map { c => castExpression(c, scala.Some(BoolType), varTypes)}}, castExpression(e1, lct, varTypes), castExpression(e2, lct, varTypes))
          }
          case LambdaExpression(argNames, argsTypes, retType, body) => LambdaExpression(argNames, argsTypes, retType,
              generateCodeWithCasts(
                body,
                IntermediateCodeUtils.getVariableMap(body, varTypes.view.mapValues{a => (a, scala.None, false)}.toMap ++ argNames.zip(argsTypes).map{case (a,b) => (a,(b, scala.None, false))}.toMap).view.mapValues{case (a,_, _) => a}.toMap,
                scala.Some(retType)
              ))
          case _ => exp
        }

        target match {
          case scala.None => innerExp
          case scala.Some(t) =>
            val actualType = typeInference(innerExp, varTypes)
            if (!castingNecessary(t, actualType)) {
              innerExp
            } else t match {
              case functionType: FunctionType =>
                generateFunctionCast(innerExp, actualType.asInstanceOf[FunctionType], functionType, varTypes)
              case _ =>
                CastingExpression(innerExp, actualType, t)
            }
        }
      }

  def generateFunctionCast(innerExp: ImpLanExpr, from: FunctionType, to: FunctionType, varTypes: Map[String, ImpLanType]): ImpLanExpr = {
    val newVarTypes = varTypes ++ to.argsTypes.zipWithIndex.map{case (t, i) => (s"tP$i" -> t)}
    val argNames = to.argsTypes.indices.map(i => s"tP$i")
    val args = from.argsTypes.indices.zip(from.argsTypes).map{
      case (i, f) => castExpression(Variable(s"tP$i"), scala.Some(f), newVarTypes)
    }
    val ret = castExpression(LambdaApplication(innerExp, args), scala.Some(to.retType), newVarTypes)
    LambdaExpression(argNames, to.argsTypes, to.retType, Seq(ReturnStatement(ret)))
  }

    def getLeastCommonType(t1: ImpLanType, t2: ImpLanType) : ImpLanType = {
      if (t1 == t2 || castingNecessary(t1, t2)) {
          t1
      } else {
        //Note: No type checking here
        t2
      }
    }
}

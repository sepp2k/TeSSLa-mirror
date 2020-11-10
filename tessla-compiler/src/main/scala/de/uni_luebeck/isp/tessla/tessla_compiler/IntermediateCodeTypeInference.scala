/*

 */

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

/**
 * Class containing code for determining the type of ImpLan expressions and adding casts.
 * Does not perform type checking in any way. Hence this class should only be used for
 * code which is known to be type correct.
 */

object IntermediateCodeTypeInference {

  /**
   * Determines the type of an [[IntermediateCode.ImpLanExpr]].
   * This function may return error-prone results if the examined expression is not type-correct.
   * @param impLanExpr Expression to be examined
   * @param varTypes Type environment, mapping every possibly involved variable identifier to its type
   * @return Type of the expression
   */
  def typeInference(impLanExpr: ImpLanExpr, varTypes: Map[String, ImpLanType]): ImpLanType = {
    impLanExpr match {
      case CastingExpression(_, _, targetType) => targetType
      case LongValue(_)                        => LongType
      case DoubleValue(_)                      => DoubleType
      case BoolValue(_)                        => BoolType
      case UnitValue                           => UnitType
      case StringValue(_)                      => StringType
      case GeneralValue                        => GeneralType
      case NoError                             => ErrorType
      case StructValue(vals) =>
        val orderedVals = vals.toSeq.sortWith {
          case ((n1, _), (n2, _)) => IntermediateCodeUtils.structComparison(n1, n2)
        }
        StructType(orderedVals.map { case (_, v) => typeInference(v, varTypes) }, orderedVals.map(_._1))
      case EmptyFunction(typeHint)                    => typeHint
      case NoneValue(typeHint)                        => OptionType(typeHint)
      case SomeValue(content)                         => OptionType(typeInference(content, varTypes))
      case EmptyMutableSet(valType)                   => MutableSetType(valType)
      case EmptyImmutableSet(valType)                 => ImmutableSetType(valType)
      case EmptyMutableMap(keyType, valType)          => MutableMapType(keyType, valType)
      case EmptyImmutableMap(keyType, valType)        => ImmutableMapType(keyType, valType)
      case EmptyMutableList(valType)                  => MutableListType(valType)
      case EmptyImmutableList(valType)                => ImmutableListType(valType)
      case FunctionCall(_, _, typeHint)               => typeHint.retType
      case TernaryExpression(_, e1, e2)               => getLeastCommonType(typeInference(e1, varTypes), typeInference(e2, varTypes))
      case Equal(_, _)                                => BoolType
      case Variable(name) if varTypes.contains(name)  => varTypes(name)
      case Variable(name)                             => throw Diagnostics.DSLTypeError(s"Type of used variable $name is unknown")
      case LambdaExpression(_, argsTypes, retType, _) => FunctionType(argsTypes, retType)
      case LambdaApplication(exp, _) =>
        typeInference(exp, varTypes) match {
          case FunctionType(_, r) => r
          case _                  => throw Diagnostics.DSLTypeError(s"Non function expression is used in application: $exp")
        }
    }
  }

  /**
   * Includes casts where necessary in a source listing
   * @param listing The source listing to be examined
   * @param varTypes Type environment, mapping every possibly involved variable identifier to its type
   * @return The source listing with casts added
   */
  def generateCodeWithCasts(listing: SourceListing, varTypes: Map[String, ImpLanType]): SourceListing = {
    listing.mapAll(generateCodeWithCasts(_, varTypes))
  }

  /**
   * Adds casts to sub-expressions of a statement sequence where necessary
   * @param stmts Statements to be examined.
   * @param varTypes Type environment, mapping every possibly involved variable identifier to its type
   * @param retType Type to which return statements from stmts are casted. Optional. If None, no casting is
   *                performed for these return statements (but still for sub-expressions)
   * @return Statement sequence with casts added
   */
  def generateCodeWithCasts(
    stmts: Seq[ImpLanStmt],
    varTypes: Map[String, ImpLanType],
    retType: Option[ImpLanType] = None
  ): Seq[ImpLanStmt] = {
    stmts.map {
      case expr: ImpLanExpr => castExpression(expr, None, varTypes)
      case If(guard, stmts, elseStmts) =>
        If(
          guard.map { _.map(e => castExpression(e, Some(BoolType), varTypes)) },
          generateCodeWithCasts(stmts, varTypes, retType),
          generateCodeWithCasts(elseStmts, varTypes, retType)
        )
      case TryCatchBlock(tr, cat) =>
        TryCatchBlock(
          generateCodeWithCasts(tr, varTypes, retType),
          generateCodeWithCasts(cat, varTypes + ("var_err" -> ErrorType), retType)
        )
      case Assignment(lhs, rexpr, defVal, typ) =>
        Assignment(lhs, castExpression(rexpr, Some(typ), varTypes), defVal, typ)
      case FinalAssignment(lhs, defVal, typ, ld) =>
        FinalAssignment(lhs, castExpression(defVal, Some(typ), varTypes), typ, ld)
      case ReturnStatement(expr) if retType.isDefined =>
        ReturnStatement(castExpression(expr, retType, varTypes))
    }
  }

  /**
   * Casts an expression to a target type if necessary.
   * Also assures type equality by wrapping sub-expressions into casts where necessary.
   * @param exp Expression to be casted
   * @param target Type to which the expression should be casted. Optional. If None only the sub-expressions are
   *               casted if necessary.
   * @param varTypes Type environment, mapping every possibly involved variable identifier to its type
   * @return Expression potentially wrapped into a [[IntermediateCode.CastingExpression]] and all sub-expression with
   *         non-expected type also wrapped into a cast expression.
   */
  def castExpression(exp: ImpLanExpr, target: Option[ImpLanType], varTypes: Map[String, ImpLanType]): ImpLanExpr = {
    val innerExp: ImpLanExpr = exp match {
      case CastingExpression(e, from, target) =>
        CastingExpression(castExpression(e, Some(from), varTypes), from, target)
      case FunctionCall(name, params, typeHint) =>
        FunctionCall(
          name,
          params.zip(typeHint.argsTypes).map { case (e, t) => castExpression(e, Some(t), varTypes) },
          typeHint
        )
      case LambdaApplication(exp, params) =>
        typeInference(exp, varTypes) match {
          case FunctionType(argsTypes, _) =>
            LambdaApplication(
              castExpression(exp, None, varTypes),
              params.zip(argsTypes).map { case (e, t) => castExpression(e, Some(t), varTypes) }
            )
          case _ => throw Diagnostics.DSLTypeError(s"Lambda Application to non-function expression $exp")
        }
      case Equal(a, b) =>
        val lct = Some(getLeastCommonType(typeInference(a, varTypes), typeInference(b, varTypes)))
        Equal(castExpression(a, lct, varTypes), castExpression(b, lct, varTypes))
      case TernaryExpression(guard, e1, e2) =>
        val lct = Some(getLeastCommonType(typeInference(e1, varTypes), typeInference(e2, varTypes)))
        TernaryExpression(
          guard.map { _.map { c => castExpression(c, Some(BoolType), varTypes) } },
          castExpression(e1, lct, varTypes),
          castExpression(e2, lct, varTypes)
        )
      case LambdaExpression(argNames, argsTypes, retType, body) =>
        LambdaExpression(
          argNames,
          argsTypes,
          retType,
          generateCodeWithCasts(
            body,
            IntermediateCodeUtils
              .getVariableMap(
                body,
                varTypes.view.mapValues { a => (a, None, false) }.toMap ++ argNames
                  .zip(argsTypes)
                  .map { case (a, b) => (a, (b, None, false)) }
                  .toMap
              )
              .view
              .mapValues { case (a, _, _) => a }
              .toMap,
            Some(retType)
          )
        )
      case _ => exp
    }

    target match {
      case None => innerExp
      case Some(t) =>
        val actualType = typeInference(innerExp, varTypes)
        if (!castingNecessary(t, actualType)) {
          innerExp
        } else
          t match {
            case functionType: FunctionType =>
              generateFunctionCast(innerExp, actualType.asInstanceOf[FunctionType], functionType, varTypes)
            case _ =>
              CastingExpression(innerExp, actualType, t)
          }
    }
  }

  /**
   * Casts a function expression into a function expression with another signature by wrapping a lambda expression
   * around the original function.
   * The parameters and return value are casted in the usual way when calling the original function.
   *
   * Example:
   *
   * f : LongType x LongType -> LongType casted to Lazy[GeneralType] x Lazy[GeneralType] -> Lazy[GeneralType]
   *
   * would produce
   *
   * (tp1: Lazy[GeneralType], tp2: Lazy[GeneralType]) => {(Lazy[GeneralType])(f((LongType)tp1, (LongType)tp2)}
   *
   *
   * @param innerExp Expression to be casted.
   * @param from Type of innerExp
   * @param to Type to cast to
   * @param varTypes Type environment, mapping every possibly involved variable identifier to its type
   * @return Casted expression (wrapped into lambda)
   */
  def generateFunctionCast(
    innerExp: ImpLanExpr,
    from: FunctionType,
    to: FunctionType,
    varTypes: Map[String, ImpLanType]
  ): ImpLanExpr = {
    val newVarTypes = varTypes ++ to.argsTypes.zipWithIndex.map {
      case (t, i) => s"tP$i" -> t
    }
    val argNames = to.argsTypes.indices.map(i => s"tP$i")
    val args = from.argsTypes.indices.zip(from.argsTypes).map {
      case (i, f) => castExpression(Variable(s"tP$i"), Some(f), newVarTypes)
    }
    val ret = castExpression(LambdaApplication(innerExp, args), Some(to.retType), newVarTypes)
    LambdaExpression(argNames, to.argsTypes, to.retType, Seq(ReturnStatement(ret)))
  }

  /**
   * Determines the type which is less specific.
   * I.e. the type in which both can be casted.
   * Such a type must exist otherwise the output is wrong.
   * @param t1 parameter one to compare
   * @param t2 parameter two to compare
   * @return The less specific type
   */
  private def getLeastCommonType(t1: ImpLanType, t2: ImpLanType): ImpLanType = {
    if (t1 == t2 || castingNecessary(t1, t2)) {
      t1
    } else {
      //Note: No type checking here
      t2
    }
  }

  /**
   * Determines if using actType as reqType requires a cast
   * @param reqType Required type
   * @param actType Actual type
   * @return True if cast from actType to reqType is necessary
   */
  private def castingNecessary(reqType: ImpLanType, actType: ImpLanType): Boolean = {
    if (reqType.isInstanceOf[LazyContainer] ^ actType.isInstanceOf[LazyContainer]) {
      true
    } else if (reqType == actType || reqType == GeneralType) {
      false
    } else {
      reqType match {
        case f1: FunctionType =>
          actType match {
            case f2: FunctionType =>
              f2.argsTypes
                .zip(f1.argsTypes)
                .appended((f1.retType, f2.retType))
                .map { case (a, b) => castingNecessary(a, b) }
                .reduce(_ || _)
            case _ => true
          }
        case _ => true
      }
    }
  }
}

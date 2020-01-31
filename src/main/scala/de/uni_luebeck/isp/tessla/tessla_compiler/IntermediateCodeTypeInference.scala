package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

/**
  * Class containing code for determining the type of ImpLan-Expressions
  */

//TODO: Ideally this class will not be needed anymore in the future
object IntermediateCodeTypeInference {

  def typeInference(impLanExpr: ImpLanExpr, varTypes: Map[String, ImpLanType]): ImpLanType = {
        impLanExpr match {
            case IntermediateCode.LongValue(_) => LongType
            case IntermediateCode.DoubleValue(_) => DoubleType
            case IntermediateCode.BoolValue(_) => BoolType
            case IntermediateCode.UnitValue => UnitType
            case IntermediateCode.StringValue(_) => StringType
            case IntermediateCode.GeneralValue => GeneralType
            case IntermediateCode.EmptyFunction(typeHint) => typeHint
            case IntermediateCode.None(typeHint) => OptionType(typeHint)
            case IntermediateCode.Some(content) => OptionType(typeInference(content, varTypes))
            case IntermediateCode.EmptyMutableSet(valType) => MutableSetType(valType)
            case IntermediateCode.EmptyImmutableSet(valType) => ImmutableSetType(valType)
            case IntermediateCode.EmptyMutableMap(keyType, valType) => MutableMapType(keyType, valType)
            case IntermediateCode.EmptyImmutableMap(keyType, valType) => ImmutableMapType(keyType, valType)
            case IntermediateCode.EmptyMutableList(valType) => MutableListType(valType)
            case IntermediateCode.EmptyImmutableList(valType) => ImmutableListType(valType)
            case FunctionCall(name, params, typeHint) => {
              val paramTypes = params.map(p => typeInference(p, varTypes))
              if (typeHint.argsTypes.zip(paramTypes).foldLeft[Boolean](true){case (r, (t1, t2)) => r && checkCompatibleType(t1, t2)})  {
                typeHint.retType
              } else {
                throw new Errors.TypeError(s"Function is applied to expressions with wrong type: $name")
              }
            }
            case LambdaApplication(exp, params) => {
              val paramTypes = params.map{p => typeInference(p, varTypes)}
              typeInference(exp, varTypes) match {
                case FunctionType(p, r) => {
                    if (p.zip(paramTypes).foldLeft[Boolean](true){case (r, (t1, t2)) => r && checkCompatibleType(t1, t2)}) {
                        r
                      } else {
                        throw new Errors.TypeError(s"Lambda application to expressions with wrong type: $exp")
                      }
                  }
                  case _ => throw new Errors.TypeError(s"Non-Lambda expression is used in application: $exp")
                }
            }
            case BitwiseOr(op1, op2) => {
              typeInferenceWithEqualityCheck(op1, op2, varTypes) match {
                case LongType => LongType
                case _ => throw new Errors.TypeError(s"$op1 and $op2 have invalid types for bitwise or")
                }
            }
            case Addition(op1, op2) => {
              typeInferenceWithEqualityCheck(op1, op2, varTypes) match {
                case LongType => LongType
                case DoubleType => DoubleType
                case _ => throw new Errors.TypeError(s"$op1 and $op2 have invalid types for addition")
              }
            }
            case Subtraction(op1, op2) => {
              typeInferenceWithEqualityCheck(op1, op2, varTypes) match {
                case LongType => LongType
                case DoubleType => DoubleType
                case _ => throw new Errors.TypeError(s"$op1 and $op2 have invalid types for subtraction")
                }
            }
            case TernaryExpression(cnf, e1, e2) => {
              val allSound = cnf.flatten.foldLeft[Boolean](true){case (res, e) => res && typeInference(e, varTypes) == BoolType}
              if (allSound) {
                typeInferenceWithEqualityCheck(e1, e2, varTypes)
              } else {
                throw new Errors.TypeError(s"Not all DNF members have bool type, but are used inside a ternary Expression: $cnf")
              }
            }
            case Equal(e1, e2) => {
              typeInferenceWithEqualityCheck(e1, e2, varTypes); BoolType
            }
            case NotEqual(e1, e2) => {
              typeInferenceWithEqualityCheck(e1, e2, varTypes); BoolType
            }
            case Greater(e1, e2) => {
              val t = typeInferenceWithEqualityCheck(e1, e2, varTypes);
              if (checkCompatibleType(t, LongType) || checkCompatibleType(t, DoubleType)) {
                BoolType
              } else {
                throw new Errors.TypeError(s"$e1, $e2 have invalid types for Greater operation")
              }
            }
            case GreaterEqual(e1, e2) => {
              val t = typeInferenceWithEqualityCheck(e1, e2, varTypes);
              if (checkCompatibleType(t, LongType) || checkCompatibleType(t, DoubleType)) {
                BoolType
              } else {
                throw new Errors.TypeError(s"$e1, $e2 have invalid types for GreaterEqual operation")
              }
            }
            case Negation(e) => if (checkCompatibleType(typeInference(e, varTypes), BoolType)) {
              BoolType
            } else {
              throw new Errors.TypeError(s"$e has no bool type, but is used inside a negation")
            }
            case Variable(name)  => if (varTypes.contains(name)) varTypes(name) else GeneralType //FIXME: Dirty, nasty hack
            case LambdaExpression(_, argsTypes, retType, _) => FunctionType(argsTypes, retType)
          }
      }

      def typeInferenceWithEqualityCheck(impLanExpr1: ImpLanExpr, impLanExpr2: ImpLanExpr, varTypes: Map[String, ImpLanType]): ImpLanType = {
        val e1_type = typeInference(impLanExpr1, varTypes)
        val e2_type = typeInference(impLanExpr2, varTypes)

          if (checkCompatibleType(e1_type, e2_type)) {
            e1_type
          } else {
            throw new Errors.TypeError(s"$impLanExpr1 of type $e1_type and $impLanExpr2 of type $e2_type are supposed to have the same type")
          }
      }

      def checkCompatibleType(e1_type: ImpLanType, e2_type: ImpLanType): Boolean = {
        e1_type == e2_type || e1_type == GeneralType || e2_type == GeneralType
      }

      //TODO: A string-based extendable type system would be advantageous here
      def castingNecessary(e1_type: ImpLanType, e2_type: ImpLanType): Boolean = {
        if (e1_type == e2_type || e1_type == GeneralType) {
          false
        } else {
          e1_type match {
            case OptionType(vt1) => e2_type match {
              case OptionType(vt2) => castingNecessary(vt1, vt2)
              case _ => true
            }
            case MutableSetType(vt1) => e2_type match {
              case MutableSetType(vt2) => castingNecessary(vt1, vt2)
              case _ => true
            }
            case ImmutableSetType(vt1) => e2_type match {
              case ImmutableSetType(vt2) => castingNecessary(vt1, vt2)
              case _ => true
            }
            case MutableMapType(kt1, vt1) =>e2_type match {
              case MutableMapType(kt2, vt2) => castingNecessary(kt1, kt2) && castingNecessary(vt1, vt2)
              case _ => true
            }
            case ImmutableMapType(kt1, vt1) =>e2_type match {
              case ImmutableMapType(kt2, vt2) => castingNecessary(kt1, kt2) && castingNecessary(vt1, vt2)
              case _ => true
            }
            case MutableListType(vt1) => e2_type match {
              case MutableListType(vt2) => castingNecessary(vt1, vt2)
              case _ => true
            }
            case ImmutableListType(vt1) => e2_type match {
              case ImmutableListType(vt2) => castingNecessary(vt1, vt2)
              case _ => true
            }
            case FunctionType(argsTypes1, retType1) => e2_type match {
              case FunctionType(argsTypes2, retType2) => castingNecessary(retType1, retType2) || argsTypes1.zip(argsTypes2).foldLeft[Boolean](false){case (r, (t1,t2)) => r || castingNecessary(t1, t2)}
              case _ => true
            }
            case _ => true
          }
        }
      }
}

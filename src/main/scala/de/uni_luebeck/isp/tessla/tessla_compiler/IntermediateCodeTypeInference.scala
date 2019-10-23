package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._

object IntermediateCodeTypeInference {

  def typeInference(impLanExpr: ImpLanExpr, varTypes: Map[String, ImpLanType]): ImpLanType = {
        impLanExpr match {
          case IntermediateCode.LongValue(_) => LongType
            case IntermediateCode.DoubleValue(_) => DoubleType
            case IntermediateCode.BoolValue(_) => BoolType
            case IntermediateCode.UnitValue => UnitType
            case IntermediateCode.StringValue(_) => StringType
            case IntermediateCode.None => {
              throw new Errors.NotYetImplementedError("Type inference of None-Option is not implemented yet")
            }
            case IntermediateCode.Some(content) => OptionType(typeInference(content, varTypes))
            case IntermediateCode.EmptyMutableSet(valType) => MutableSetType(valType)
            case IntermediateCode.EmptyImmutableSet(valType) => ImmutableSetType(valType)
            case IntermediateCode.EmptyMutableMap(keyType, valType) => MutableMapType(keyType, valType)
            case IntermediateCode.EmptyImmutableMap(keyType, valType) => ImmutableMapType(keyType, valType)
            case IntermediateCode.EmptyMutableList(valType) => MutableListType(valType)
            case IntermediateCode.EmptyImmutableList(valType) => ImmutableListType(valType)
            case FunctionCall(name, params, typeHint) => {
              if (params.map(p => typeInference(p, varTypes)) == typeHint.argsTypes) {
                typeHint.retType
              } else {
                throw new Errors.TypeError(s"Function is applied to expressions with wrong type: $name")
              }
            }
            case FunctionVarApplication(variable, params) => {
              val paramTypes = params.map{p => typeInference(p, varTypes)}
              varTypes(variable.name) match {
                case FunctionType(p, r) => {
                    if (p == paramTypes) {
                        r
                      } else {
                        throw new Errors.TypeError(s"Lambda variable is applied to expressions with wrong type: $variable")
                      }
                  }
                  case _ => throw new Errors.TypeError(s"Non-Lambda variable is used in application: $variable")
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
            case TernaryExpression(_, e1, e2) => typeInferenceWithEqualityCheck(e1, e2, varTypes)
            case Equal(_, _) | NotEqual(_, _) | Greater(_, _) | GreaterEqual(_, _) | Negation(_) => BoolType
            case Variable(name)  => varTypes(name)
            case LambdaExpression(_, argsTypes, retType, _) => FunctionType(argsTypes, retType)
          }
      }

      def typeInferenceWithEqualityCheck(impLanExpr1: ImpLanExpr, impLanExpr2: ImpLanExpr, varTypes: Map[String, ImpLanType]): ImpLanType = {
        val e1_type = typeInference(impLanExpr1, varTypes)
        val e2_type = typeInference(impLanExpr2, varTypes)

          if (e1_type == e2_type) {
            e1_type
          } else {
            throw new Errors.TypeError(s"$impLanExpr1 of type $e1_type and $impLanExpr1 of type $e1_type are supposed to have the same type")
          }
      }

}

package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.tessla_compiler.{Errors, IntermediateCode}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{FunctionCall, FunctionType, ImpLanExpr, ImpLanType, ImpLanVal}

/**
  * Class containing Java-specific constants for the translation
  */
object JavaConstants {

  def typeTranslation(t: ImpLanType) : String = {
    t match {
      case IntermediateCode.LongType => "long"
      case IntermediateCode.DoubleType => "double"
      case IntermediateCode.BoolType => "boolean"
      case IntermediateCode.UnitType => "boolean"
      case IntermediateCode.StringType => "String"
      case IntermediateCode.OptionType(valType) => s"java.util.Optional<${objectTypeTranslation(valType)}>"
      case IntermediateCode.MutableSetType(valType) => s"scala.collection.mutable.HashSet<${objectTypeTranslation(valType)}>"
      case IntermediateCode.ImmutableSetType(valType) => s"scala.collection.immutable.HashSet<${objectTypeTranslation(valType)}>"
      case IntermediateCode.MutableMapType(keyType, valType) => s"scala.collection.mutable.HashMap<${objectTypeTranslation(keyType)}, ${objectTypeTranslation(valType)}>"
      case IntermediateCode.ImmutableMapType(keyType, valType) => s"scala.collection.immutable.HashMap<${objectTypeTranslation(keyType)}, ${objectTypeTranslation(valType)}>"
      case IntermediateCode.MutableListType(valType) => s"scala.collection.mutable.ArrayBuffer<${objectTypeTranslation(valType)}>"
      case IntermediateCode.ImmutableListType(valType) => s"scala.collection.immutable.ArrayBuffer<${objectTypeTranslation(valType)}>"
      case IntermediateCode.FunctionType(argsTypes, retType) => {
        val ret = ((if (argsTypes.size == 0) "" else ", ") + objectTypeTranslation(retType))
        s"scala.Function${argsTypes.size}<${argsTypes.map(objectTypeTranslation).mkString(", ")}${ret}>"
      }
    }
  }

  def objectTypeTranslation(t: ImpLanType) : String = {
    t match {
      case IntermediateCode.LongType => "Long"
      case IntermediateCode.DoubleType => "Double"
      case IntermediateCode.BoolType => "Boolean"
      case IntermediateCode.UnitType => "Boolean"
      case _ => typeTranslation(t)
    }
  }

  def valueTranslation(v: ImpLanVal) : String = {
    v match {
      case IntermediateCode.LongValue(value) => value.toString()
      case IntermediateCode.DoubleValue(value) => value.toString()
      case IntermediateCode.BoolValue(value) => value.toString()
      case IntermediateCode.UnitValue => "true"
      case IntermediateCode.StringValue(value) => s""""${value.replaceAllLiterally("\"", "\\\"")}"""" //TODO: Find better solution, re-escaping all special chars
      case IntermediateCode.None(_) => "java.util.Optional.empty()"
      case IntermediateCode.Some(content) => s"java.util.Optional.of(${valueTranslation(content)})"
      case IntermediateCode.EmptyMutableSet(valType) => s"new scala.collection.mutable.HashSet<${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyImmutableSet(valType) => s"new scala.collection.immutable.HashSet<${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyMutableMap(keyType, valType) => s"new scala.collection.mutable.HashMap<${objectTypeTranslation(keyType)}, ${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyImmutableMap(keyType, valType) => s"new scala.collection.immutable.HashMap<${objectTypeTranslation(keyType)}, ${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyMutableList(valType) => s"new scala.collection.mutable.ArrayBuffer<${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyImmutableList(valType) => s"new scala.collection.immutable.ArrayBuffer<${objectTypeTranslation(valType)}>()"
    }
  }

  def builtinFunctionCallTranslation(name: String, args: Seq[String], typeHint: FunctionType) : String = {
    name match {
      case "__[TC]output__" => s"outputVar(${getParseExpressionToString(typeHint.argsTypes(0), args(0))}, ${args(1)}, ${args(2)}, currTs)"
      case "__[TC]inputParse__" => getStringParseExpression(typeHint.retType, args(0))
      case "__not__" => s"!${args(0)}"
      case "__negate__" |
           "__fnegate__"  => s"-${args(0)}"
      case "__bitflip__" => s"~${args(0)}"
      case "__and__" => s"${args(0)} && ${args(1)}"
      case "__or__" => s"${args(0)} || ${args(1)}"
      case "__eq__" => s"${args(0)} == ${args(1)}"
      case "__neq__" => s"${args(0)} != ${args(1)}"
      case "__gt__" |
           "__fgt__" => s"${args(0)} > ${args(1)}"
      case "__lt__" |
           "__flt__" => s"${args(0)} < ${args(1)}"
      case "__geq__" |
           "__fgeq__" => s"${args(0)} >= ${args(1)}"
      case "__leq__" |
           "__fleq__"=> s"${args(0)} <= ${args(1)}"
      case "__add__" |
           "__fadd__" => s"${args(0)} + ${args(1)}"
      case "__sub__" |
           "__fsub__" => s"${args(0)} - ${args(1)}"
      case "__mul__" |
           "__fmul__" => s"${args(0)} * ${args(1)}"
      case "__div__" |
           "__fdiv__" => s"${args(0)} / ${args(1)}"
      case "__mod__" => s"${args(0)} % ${args(1)}"
      case "__bitand__" => s"${args(0)} & ${args(1)}"
      case "__bitor__" => s"${args(0)} | ${args(1)}"
      case "__bitxor__" => s"${args(0)} ^ ${args(1)}"
      case "__leftshift__" => s"${args(0)} << ${args(1)}"
      case "__rightshift__" => s"${args(0)} >> ${args(1)}"
      case _ => throw new Errors.CommandNotSupportedError(s"Unsupported built-in function for Java backend: $name")
    }
  }

  def getStringParseExpression(to: ImpLanType, exp: String) : String = {
    to match {
      case IntermediateCode.LongType => s"Long.parseLong($exp)"
      case IntermediateCode.DoubleType => s"Double.parseDouble($exp)"
      case IntermediateCode.BoolType => s"Boolean.parseBoolean($exp)"
      case IntermediateCode.UnitType => "true"
      case IntermediateCode.StringType => s"$exp"
      case t => throw new Errors.CommandNotSupportedError(s"Input parsing of type $t is not supported in the Java translation")
    }
  }

  def getParseExpressionToString(from: ImpLanType, exp: String) : String = {
    from match {
      case IntermediateCode.LongType |
           IntermediateCode.DoubleType |
           IntermediateCode.BoolType => s"String.valueOf($exp)"
      case IntermediateCode.UnitType => "\"()\""
      case IntermediateCode.StringType => s"$exp"
      case t => s"$exp.toString()"
    }
  }

}

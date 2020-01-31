package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.tessla_compiler.{Errors, IntermediateCode}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{FunctionType, ImpLanType, ImpLanVal}

/**
  * Class containing Java-specific constants for the translation
  */
object ScalaConstants {

  def typeTranslation(t: ImpLanType) :String = typeTranslation(t, false)

  def typeTranslation(t: ImpLanType, asTypePar: Boolean) : String = {
    t match {
      case IntermediateCode.LongType => "Long"
      case IntermediateCode.DoubleType => "Double"
      case IntermediateCode.BoolType => "Boolean"
      case IntermediateCode.UnitType => "Boolean"
      case IntermediateCode.StringType => "String"
      case IntermediateCode.GeneralType if asTypePar => "_"
      case IntermediateCode.GeneralType => "Object"
      case IntermediateCode.OptionType(valType) => s"java.util.Optional[${typeTranslation(valType, true)}]"
      case IntermediateCode.MutableSetType(valType) => s"scala.collection.mutable.HashSet[${typeTranslation(valType, true)}]"
      case IntermediateCode.ImmutableSetType(valType) => s"scala.collection.immutable.HashSet[${typeTranslation(valType, true)}]"
      case IntermediateCode.MutableMapType(keyType, valType) => s"scala.collection.mutable.HashMap[${typeTranslation(keyType, true)}, ${typeTranslation(valType, true)}]"
      case IntermediateCode.ImmutableMapType(keyType, valType) => s"scala.collection.immutable.HashMap[${typeTranslation(keyType, true)}, ${typeTranslation(valType, true)}]"
      case IntermediateCode.MutableListType(valType) => s"scala.collection.mutable.ArrayBuffer[${typeTranslation(valType, true)}]"
      case IntermediateCode.ImmutableListType(valType) => s"scala.collection.immutable.ArrayBuffer[${typeTranslation(valType, true)}]"
      case IntermediateCode.FunctionType(argsTypes, retType) => {
        val ret = ((if (argsTypes.size == 0) "" else ", ") + typeTranslation(retType))
        s"scala.Function${argsTypes.size}[${argsTypes.map(typeTranslation).mkString(", ")}${ret}]"
      }
    }
  }

  def valueTranslation(v: ImpLanVal) : String = {
    v match {
      case IntermediateCode.LongValue(value) => value.toString()
      case IntermediateCode.DoubleValue(value) => value.toString()
      case IntermediateCode.BoolValue(value) => value.toString()
      case IntermediateCode.UnitValue => "true"
      case IntermediateCode.StringValue(value) => s""""${value.replaceAllLiterally("\"", "\\\"")}"""" //TODO: Find better solution, re-escaping all special chars
      case IntermediateCode.GeneralValue => "null"
      case IntermediateCode.EmptyFunction(_) => "null"
      case IntermediateCode.None(_) => "java.util.Optional.empty()"
      case IntermediateCode.Some(content) => s"java.util.Optional.of(${valueTranslation(content)})"
      case IntermediateCode.EmptyMutableSet(valType) => s"new scala.collection.mutable.HashSet()"
      case IntermediateCode.EmptyImmutableSet(valType) => s"new scala.collection.immutable.HashSet()"
      case IntermediateCode.EmptyMutableMap(keyType, valType) => s"new scala.collection.mutable.HashMap()"
      case IntermediateCode.EmptyImmutableMap(keyType, valType) => s"new scala.collection.immutable.HashMap()"
      case IntermediateCode.EmptyMutableList(valType) => s"new scala.collection.mutable.ArrayBuffer()"
      case IntermediateCode.EmptyImmutableList(valType) => s"new scala.collection.immutable.ArrayBuffer()"
    }
  }

  def builtinFunctionCallTranslation(name: String, args: Seq[String], typeHint: FunctionType) : String = {
    name match {
      case "__[TC]output__" => s"outputVar(${getParseExpressionToString(typeHint.argsTypes(0), args(0))}, ${args(1)}, ${args(2)}, currTs)"
      case "__[TC]inputParse__" => getStringParseExpression(typeHint.retType, args(0))
      case "__ite__" => s"if (${args(0)}) ${args(1)} else ${args(2)}"
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
      case "__Some__" => s"java.util.Optional.of(${args(0)})"
      case "__getSome__" => s"${args(0)}.get()"
      case "__isSome__" => s"${args(0)}.isPresent()"
      case "__isNone__" => s"!${args(0)}.isPresent()"
      case _ => throw new Errors.CommandNotSupportedError(s"Unsupported built-in function for Java backend: $name")
    }
  }

  def getStringParseExpression(to: ImpLanType, exp: String) : String = {
    to match {
      case IntermediateCode.LongType => s"java.lang.Long.parseLong($exp)"
      case IntermediateCode.DoubleType => s"java.lang.Double.parseDouble($exp)"
      case IntermediateCode.BoolType => s"java.lang.Boolean.parseBoolean($exp)"
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

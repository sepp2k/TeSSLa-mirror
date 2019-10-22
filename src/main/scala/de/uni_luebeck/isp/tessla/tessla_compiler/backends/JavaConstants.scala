package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{ImpLanType, ImpLanVal}

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
      case IntermediateCode.None => "java.util.Optional.empty()"
      case IntermediateCode.Some(content) => s"java.util.Optional.of(${valueTranslation(content)})"
      case IntermediateCode.EmptyMutableSet(valType) => s"new scala.collection.mutable.HashSet<${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyImmutableSet(valType) => s"new scala.collection.immutable.HashSet<${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyMutableMap(keyType, valType) => s"new scala.collection.mutable.HashMap<${objectTypeTranslation(keyType)}, ${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyImmutableMap(keyType, valType) => s"new scala.collection.immutable.HashMap<${objectTypeTranslation(keyType)}, ${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyMutableList(valType) => s"new scala.collection.mutable.ArrayBuffer<${objectTypeTranslation(valType)}>()"
      case IntermediateCode.EmptyImmutableList(valType) => s"new scala.collection.immutable.ArrayBuffer<${objectTypeTranslation(valType)}>()"
    }
  }

}

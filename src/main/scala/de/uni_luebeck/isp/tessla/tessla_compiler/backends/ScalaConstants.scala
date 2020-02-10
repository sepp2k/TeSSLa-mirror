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
      case IntermediateCode.OptionType(valType) => s"Option[${typeTranslation(valType, true)}]"
      case IntermediateCode.MutableSetType(valType) => s"scala.collection.mutable.HashSet[${typeTranslation(valType, true)}]"
      case IntermediateCode.ImmutableSetType(valType) => s"Set[${typeTranslation(valType, true)}]"
      case IntermediateCode.MutableMapType(keyType, valType) => s"scala.collection.mutable.HashMap[${typeTranslation(keyType, true)}, ${typeTranslation(valType, true)}]"
      case IntermediateCode.ImmutableMapType(keyType, valType) => s"Map[${typeTranslation(keyType, true)}, ${typeTranslation(valType, true)}]"
      case IntermediateCode.MutableListType(valType) => s"scala.collection.mutable.ArrayBuffer[${typeTranslation(valType, true)}]"
      case IntermediateCode.ImmutableListType(valType) => s"List[${typeTranslation(valType, true)}]"
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
      case IntermediateCode.None(_) => "None"
      case IntermediateCode.Some(content) => s"Option(${valueTranslation(content)})"
      case IntermediateCode.EmptyMutableSet(_) => s"scala.collection.mutable.HashSet()"
      case IntermediateCode.EmptyImmutableSet(_) => s"Set()"
      case IntermediateCode.EmptyMutableMap(_, _) => s"scala.collection.mutable.HashMap()"
      case IntermediateCode.EmptyImmutableMap(_, _) => "Map()"
      case IntermediateCode.EmptyMutableList(_) => "scala.collection.mutable.ArrayBuffer()"
      case IntermediateCode.EmptyImmutableList(_) => "List()"
    }
  }

  def builtinFunctionCallTranslation(name: String, args: Seq[String], typeHint: FunctionType) : String = {
    name match {
      case "__[TC]output__" => s"outputVar(${getParseExpressionToString(typeHint.argsTypes(0), args(0))}, ${args(1)}, ${args(2)}, currTs)"
      case "__[TC]inputParse__" => getStringParseExpression(typeHint.retType, args(0))
      case "__[TC]getErrorCode__" => s"getErrorCode(${args(0)})"

      case "__ite__" => s"if (${args(0)}) ${args(1)} else ${args(2)}"
      case "__not__" => s"!(${args(0)})"
      case "__negate__" |
           "__fnegate__"  => s"-${args(0)}"
      case "__bitflip__" => s"~${args(0)}"
      case "__and__" => args.mkString(" && ")
      case "__or__" => args.mkString(" || ")
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
      case "__bitand__" => args.mkString(" & ")
      case "__bitor__" => args.mkString(" | ")
      case "__bitxor__" => args.mkString(" ^ ")
      case "__leftshift__" => s"${args(0)} << ${args(1)}"
      case "__rightshift__" => s"${args(0)} >> ${args(1)}"

      case "__Some__" => s"Option(${args(0)})"
      case "__getSome__" => s"${args(0)}.get"
      case "__isSome__" => s"${args(0)}.isDefined"
      case "__isNone__" => s"${args(0)}.isEmpty"

      //TODO: Handle mutable datastructures
      case "__Map_empty__" => "Map()"
      case "__Map_add__" => s"${args(0)} + (${args(1)} -> ${args(2)})"
      case "__Map_contains__" => s"${args(0)}.contains(${args(1)})"
      case "__Map_get__" => s"${args(0)}(${args(1)})"
      case "__Map_remove__" => s"${args(0)} - ${args(1)}"
      case "__Map_size__" => s"${args(0)}.size"
      case "__Map_fold__" => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1), true)}](${args(1)})(${args(2)})"
      case "__Map_keys__" => s"${args(0)}.keys"

      case "__Set_empty__" => "Set()"
      case "__Set_add__" => s"${args(0)} :+ ${args(1)}"
      case "__Set_contains__" => s"${args(0)}(${args(1)})"
      case "__Set_remove__" => s"${args(0)} - ${args(1)}"
      case "__Set_size__" => s"${args(0)}.size"
      case "__Set_union__" => s"${args(0)}.union(${args(1)})"
      case "__Set_intersection__" => s"${args(0)}.intersect(${args(1)})"
      case "__Set_minus__" => s"${args(0)} -- ${args(1)}"
      case "__Set_fold__" => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1), true)}](${args(1)})(${args(2)})"

      case "__List_empty__" => s"List()"
      case "__List_size__" => s"${args(0)}.size"
      case "__List_append__" => s"${args(0)} :+ ${args(1)}"
      case "__List_prepend__" => s"${args(0)} +: ${args(1)}"
      case "__List_tail__" => s"${args(0)}.tail"
      case "__List_init__" => ???
      case "__List_get__" => s"${args(0)}(${args(1)})"
      case "__List_set__" => s"${args(0)}.updated(${args(1)}, ${args(2)})"
      case "__List_fold__" => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1), true)}](${args(1)})(${args(2)})"

      case _ => throw Errors.CommandNotSupportedError(s"Unsupported built-in function for Java backend: $name")
    }
  }

  def getStringParseExpression(to: ImpLanType, exp: String) : String = {
    to match {
      case IntermediateCode.LongType => s"java.lang.Long.parseLong($exp)"
      case IntermediateCode.DoubleType => s"java.lang.Double.parseDouble($exp)"
      case IntermediateCode.BoolType => s"java.lang.Boolean.parseBoolean($exp)"
      case IntermediateCode.UnitType => "true"
      case IntermediateCode.StringType => s"$exp"
      case t => throw Errors.CommandNotSupportedError(s"Input parsing of type $t is not supported in the Java translation")
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

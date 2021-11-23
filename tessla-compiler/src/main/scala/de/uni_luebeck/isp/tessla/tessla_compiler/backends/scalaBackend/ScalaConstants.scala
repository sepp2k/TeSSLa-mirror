package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.{Errors, IntermediateCode, IntermediateCodeUtils}

/**
  * Class containing Java-specific constants for the translation
  */
object ScalaConstants {

  def typeTranslation(t: ImpLanType) : String = {
    t match {
      case VoidType => "Unit"
      case LongType => "Long"
      case DoubleType => "Double"
      case BoolType => "Boolean"
      case UnitType => "Boolean"
      case StringType => "String"
      case GeneralType => "Any"
      case OptionType(valType) => s"Option[${typeTranslation(valType)}]"
      case MutableSetType(valType) => s"scala.collection.mutable.HashSet[${typeTranslation(valType)}]"
      case ImmutableSetType(valType) => s"Set[${typeTranslation(valType)}]"
      case MutableMapType(keyType, valType) => s"scala.collection.mutable.HashMap[${typeTranslation(keyType)}, ${typeTranslation(valType)}]"
      case ImmutableMapType(keyType, valType) => s"Map[${typeTranslation(keyType)}, ${typeTranslation(valType)}]"
      case MutableListType(valType) => s"scala.collection.mutable.ArrayBuffer[${typeTranslation(valType)}]"
      case ImmutableListType(valType) => s"List[${typeTranslation(valType)}]"
      case MutableQueueType(valType) => s"scala.collection.mutable.Queue[${typeTranslation(valType)}]"
      case ImmutableQueueType(valType) => s"scala.collection.immutable.Queue[${typeTranslation(valType)}]"
      case FunctionType(argsTypes, retType) => {
        val ret = ((if (argsTypes.isEmpty) "" else ", ") + typeTranslation(retType))
        s"scala.Function${argsTypes.size}[${argsTypes.map(typeTranslation).mkString(", ")}${ret}]"
      }
      case StructType(types, _) => s"(${types.map(typeTranslation).mkString(", ")})"
    }
  }

  def valueTranslation(v: ImpLanVal) : String = {
    v match {
      case LongValue(value) => s"${value}L"
      case DoubleValue(value) => s"${value}d"
      case BoolValue(value) => value.toString()
      case UnitValue => "true"
      case StringValue(value) => s""""${value.replaceAllLiterally("\"", "\\\"")}""""
      case GeneralValue => "null"
      case EmptyFunction(_) => "null"
      case None(_) => "None"
      case Some(content) => s"Option(${valueTranslation(content)})"
      case EmptyMutableSet(t) => typeTranslation(MutableSetType(t)) + "()"
      case EmptyImmutableSet(_) => s"Set()"
      case EmptyMutableMap(t1, t2) => typeTranslation(MutableMapType(t1, t2)) + "()"
      case EmptyImmutableMap(_, _) => "Map()"
      case EmptyMutableList(t) => typeTranslation(MutableListType(t)) + "()"
      case EmptyImmutableList(_) => "List()"
      case EmptyMutableQueue(_) => "scala.collection.mutable.Queue()"
      case EmptyImmutableQueue(_) => "scala.collection.immutable.Queue()"
      case StructValue(vals) => s"(${vals.toSeq.sortWith{case ((n1,_),(n2,_)) => n1 < n2}.map{case (_, v) => valueTranslation(v)}.mkString(", ")})"
    }
  }

  def builtinFunctionCallTranslation(name: String, args: Seq[String], typeHint: FunctionType) : String = {
    name match {
      case "__[TC]output__" => s"outputVar(${ScalaIOHandling.getParseExpressionToString(typeHint.argsTypes(0), args(0))}, ${args(1)}, ${args(2)}, currTs)"
      case "__[TC]inputParse__" => ScalaIOHandling.getInputParseExpression(typeHint.retType, args(0))
      case "__[TC]getErrorCode__" => s"getErrorCode(${args(0)})"

      case "__ite__" |
           "__staticite__" => s"(if (${args(0)}) ${args(1)} else ${args(2)})"
      case "__not__" => s"!(${args(0)})"
      case "__negate__" |
           "__fnegate__"  => s"-${args(0)}"
      case "__bitflip__" => s"~${args(0)}"
      case "__and__" => "(" + args.mkString(" && ") + ")"
      case "__or__" => "(" +  args.mkString(" || ") + ")"
      case "__eq__" => s"(${args(0)} == ${args(1)})"
      case "__neq__" => s"(${args(0)} != ${args(1)})"
      case "__gt__" |
           "__fgt__" => s"(${args(0)} > ${args(1)})"
      case "__lt__" |
           "__flt__" => s"(${args(0)} < ${args(1)})"
      case "__geq__" |
           "__fgeq__" => s"(${args(0)} >= ${args(1)})"
      case "__leq__" |
           "__fleq__"=> s"(${args(0)} <= ${args(1)})"
      case "__add__" |
           "__fadd__" |
           "__String_concat__" => s"(${args(0)} + ${args(1)})"
      case "__sub__" |
           "__fsub__" => s"(${args(0)} - ${args(1)})"
      case "__mul__" |
           "__fmul__" => s"(${args(0)} * ${args(1)})"
      case "__div__" |
           "__fdiv__" => s"(${args(0)} / ${args(1)})"
      case "__mod__" => s"(${args(0)} % ${args(1)})"
      case "__bitand__" => "(" + args.mkString(" & ") + ")"
      case "__bitor__" => "(" + args.mkString(" | ") + ")"
      case "__bitxor__" => "(" + args.mkString(" ^ ") + ")"
      case "__leftshift__" => "(" + s"${args(0)} << ${args(1)}" + ")"
      case "__rightshift__" => "(" + s"${args(0)} >> ${args(1)}" + ")"

      case "__pow__" => s"java.lang.Math.pow(${args(0)}, ${args(1)})"
      case "__log__" => s"java.lang.Math.log(${args(0)}, ${args(1)})"
      case "__sin__" => s"java.lang.Math.sin(${args(0)})"
      case "__cos__" => s"java.lang.Math.cos(${args(0)})"
      case "__tan__" => s"java.lang.Math.tan(${args(0)})"
      case "__atan__" => s"java.lang.Math.atan(${args(0)})"

      case "__intToFloat__" => s"${args(0)}.asInstanceOf[Float]"
      case "__floatToInt__" => s"${args(0)}.asInstanceOf[Long]"

      case "__Some__" => s"Option(${args(0)})"
      case "__None__" => s"None"
      case "__getSome__" => s"${args(0)}.get"
      case "__isSome__" => s"${args(0)}.isDefined"
      case "__isNone__" => s"${args(0)}.isEmpty"

      case "__toString__" =>  s"${args(0)}.toString"
      case "__String_format__" => s"${args(0)}.formatLocal(java.util.Locale.ROOT, ${args(1)})"

      case "__Map_empty__" if typeHint.retType.isInstanceOf[MutableMapType] => typeTranslation(typeHint.retType) + "()"
      case "__Map_empty__" => "Map()"
      case "__Map_add__" if typeHint.retType.isInstanceOf[MutableMapType] => s"${args(0)} += ((${args(1)}) -> (${args(2)}))"
      case "__Map_add__" => s"${args(0)} + ((${args(1)}) -> (${args(2)}))"
      case "__Map_contains__" => s"${args(0)}.contains(${args(1)})"
      case "__Map_get__" => s"${args(0)}(${args(1)})"
      case "__Map_remove__" if typeHint.retType.isInstanceOf[MutableMapType] => s"${args(0)} -= ${args(1)}"
      case "__Map_remove__" => s"${args(0)} - ${args(1)}"
      case "__Map_size__" => s"${args(0)}.size"
      case "__Map_fold__" =>
        s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)}){case (c, (k, v)) => val f = ${args(2)}; f(c, k, v)}"
      case "__Map_keys__" if typeHint.retType.isInstanceOf[MutableListType] => s"${args(0)}.keys.toList"
      case "__Map_sortedKeys__" => s"${args(0)}.keys.toSeq.sortWith((a: Long, b: Long) => a < b).toList"

      case "__Set_empty__" if typeHint.retType.isInstanceOf[MutableSetType] => typeTranslation(typeHint.retType) + "()"
      case "__Set_empty__" => "Set()"
      case "__Set_add__" if typeHint.retType.isInstanceOf[MutableSetType] => s"${args(0)} += (${args(1)})"
      case "__Set_add__" => s"${args(0)} + (${args(1)})"
      case "__Set_contains__" => s"${args(0)}(${args(1)})"
      case "__Set_remove__" if typeHint.retType.isInstanceOf[MutableSetType] => s"${args(0)} -= ${args(1)}"
      case "__Set_remove__" => s"${args(0)} - ${args(1)}"
      case "__Set_size__" => s"${args(0)}.size"
      case "__Set_union__" => s"${args(0)}.union(${args(1)})"
      case "__Set_intersection__" => s"${args(0)}.intersect(${args(1)})"
      case "__Set_minus__" if typeHint.retType.isInstanceOf[MutableSetType] => s"${args(0)} --= ${args(1)}"
      case "__Set_minus__" => s"${args(0)} -- ${args(1)}"
      case "__Set_fold__" => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)})(${args(2)})"

      case "__List_empty__" if typeHint.retType.isInstanceOf[MutableListType] => typeTranslation(typeHint.retType) + "()"
      case "__List_empty__" => s"List()"
      case "__List_size__" => s"${args(0)}.size"
      case "__List_append__" if typeHint.retType.isInstanceOf[MutableListType] => s"${args(0)} += ${args(1)}"
      case "__List_append__" => s"${args(0)} :+ ${args(1)}"
      case "__List_prepend__" if typeHint.retType.isInstanceOf[MutableListType] => s"${args(0)} +=: ${args(1)}"
      case "__List_prepend__" => s"${args(0)} +: ${args(1)}"
      case "__List_tail__" => s"${args(0)}.tail"
      case "__List_init__" => s"${args(0)}.init"
      case "__List_get__" => s"${args(0)}(${args(1)}.asInstanceOf[Int])"
      case "__List_set__" if typeHint.retType.isInstanceOf[MutableListType] => s"${args(0)}.update(${args(1)}.asInstanceOf[Int], ${args(2)})"
      case "__List_set__" => s"${args(0)}.updated(${args(1)}.asInstanceOf[Int], ${args(2)})"
      case "__List_fold__" => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)})(${args(2)})"

      case "__Queue_empty__" if typeHint.retType.isInstanceOf[MutableQueueType] => "scala.collection.mutable.Queue()"
      case "__Queue_empty__" => "scala.collection.immutable.Queue()"
      case "__Queue_enq__" => s"${args(0)}.enqueue(${args(1)})"
      case "__Queue_deq__" if typeHint.retType.isInstanceOf[MutableQueueType]=> s"${args(0)}.dropInPlace(1)"
      case "__Queue_deq__" => s"${args(0)}.drop(1)"
      case "__Queue_first__" => s"${args(0)}.front"
      case "__Queue_size__" => s"${args(0)}.size"

      case "__getStruct__" => {
        typeHint match {
          case FunctionType(Seq(StructType(_, fieldNames), IntermediateCode.StringType), _) => {
            s"${args(0)}._${fieldNames.indexOf(args(1).replace("\"", "")) + 1}"
          }
          case _ => throw Errors.DSLError(s"__getStruct__ call has wrong type hint $typeHint")
        }
      }
      case "__mkStruct__" => s"(${args.mkString(", ")})"

      case _ => throw Errors.CommandNotSupportedError(s"Unsupported built-in function for Scala backend: $name")
    }
  }

}

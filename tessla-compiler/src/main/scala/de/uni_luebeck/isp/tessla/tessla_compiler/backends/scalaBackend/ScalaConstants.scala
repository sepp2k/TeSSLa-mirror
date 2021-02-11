/*
 * Copyright 2021 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.scalaBackend

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.{Diagnostics, IntermediateCode}

/**
 * Class containing Scala-specific constants for the translation
 */
object ScalaConstants {

  /**
   * Translates an [[IntermediateCode.ImpLanType]] to its corresponding Scala type.
   *
   * - The Lazy container type is translated as Function0
   * - The generic type is translated as Any
   * - The error type is translated as Throwable
   * - Options are translated to an own type ErrorOption which is able to capture an error
   *
   * @param t Type to be translated
   * @return Corresponding Scala type
   */
  def typeTranslation(t: ImpLanType): String = {
    t match {
      case VoidType                  => "Unit"
      case LongType                  => "Long"
      case DoubleType                => "Double"
      case BoolType                  => "Boolean"
      case UnitType                  => "Boolean"
      case StringType                => "String"
      case GeneralType               => "Any"
      case ErrorType                 => "Throwable"
      case OptionType(valType)       => s"ErrorOption[${typeTranslation(valType)}]"
      case MutableSetType(valType)   => s"scala.collection.mutable.HashSet[${typeTranslation(valType)}]"
      case ImmutableSetType(valType) => s"Set[${typeTranslation(valType)}]"
      case MutableMapType(keyType, valType) =>
        s"scala.collection.mutable.HashMap[${typeTranslation(keyType)}, ${typeTranslation(valType)}]"
      case ImmutableMapType(keyType, valType) => s"Map[${typeTranslation(keyType)}, ${typeTranslation(valType)}]"
      case MutableListType(valType)           => s"scala.collection.mutable.ArrayBuffer[${typeTranslation(valType)}]"
      case ImmutableListType(valType)         => s"List[${typeTranslation(valType)}]"
      case FunctionType(argsTypes, retType) =>
        val ret = (if (argsTypes.isEmpty) "" else ", ") + typeTranslation(retType)
        s"scala.Function${argsTypes.size}[${argsTypes.map(typeTranslation).mkString(", ")}$ret]"
      case StructType(types, _)       => s"(${types.map(typeTranslation).mkString(", ")})"
      case LazyContainer(typ)         => s"Function0[${typeTranslation(typ)}]"
      case NativeType(name, subTypes) => s"$name" + (if (subTypes.nonEmpty) s"[${subTypes.mkString(",")}]" else "")
    }
  }

  /**
   * Translates an [[IntermediateCode.ImpLanVal]] to its corresponding Scala value.
   *
   * - None/Some are translated to special values EONone/EOSome to be able to capture errors in Somes
   *
   * @param v The value to be translated
   * @return The corresponding value in Scala
   */
  def valueTranslation(v: ImpLanVal): String = {
    v match {
      case LongValue(value)   => s"${value}L"
      case DoubleValue(value) => s"${value}d"
      case BoolValue(value)   => value.toString
      case UnitValue          => "true"
      case StringValue(value) =>
        s""""${value.replace("\"", "\\\"")}"""" //TODO: Find better solution, re-escaping all special chars
      case GeneralValue            => "null"
      case EmptyFunction(_)        => "null"
      case NoError                 => "null"
      case NoneValue(_)            => "EONone()"
      case SomeValue(content)      => s"EOSome(${valueTranslation(content)})"
      case EmptyMutableSet(_)      => s"scala.collection.mutable.HashSet()"
      case EmptyImmutableSet(_)    => s"Set()"
      case EmptyMutableMap(_, _)   => s"scala.collection.mutable.HashMap()"
      case EmptyImmutableMap(_, _) => "Map()"
      case EmptyMutableList(_)     => "scala.collection.mutable.ArrayBuffer()"
      case EmptyImmutableList(_)   => "List()"
      case StructValue(vals) =>
        s"(${vals.toSeq.sortWith { case ((n1, _), (n2, _)) => n1 < n2 }.map { case (_, v) => valueTranslation(v) }.mkString(", ")})"
    }
  }

  /**
   * Performs the translation of functions in ImpLan to functions in Scala
   * Only functions starting and ending with two underscores (_) are translated.
   * Note: Mutable datastructures are not yet handled on this branch since they are not generated
   * @param name Name of the function which is called
   * @param oArgs Argument expressions of the function call
   * @param transFunc Function used to translate the arguments
   * @param typeHint The type signature of the called function.
   * @return The translated function call in Scala
   */
  def builtinFunctionCallTranslation(
    name: String,
    oArgs: Seq[ImpLanExpr],
    transFunc: ImpLanExpr => String,
    typeHint: FunctionType
  ): String = {
    val args = oArgs.map(transFunc).toIndexedSeq
    name match {
      case "__[TC]output__" =>
        val toStr = ScalaIOHandling.getParseExpressionToString(typeHint.argsTypes.head, args(0))
        s"outputVar($toStr, ${args(1)}, ${args(2)}, ${args(3)}, ${args(4)})"
      case "__[TC]inputParse__"   => ScalaIOHandling.getInputParseExpression(typeHint.retType, args(0))
      case "__[TC]getErrorCode__" => s"getErrorCode(${args(0)})"
      case "__[TC]throw__"        => s"throw ${args(0)}"
      case "__[TC]delayPanic__" =>
        "{System.err.println(s\"FATAL: Due to previous errors a delay could not be evaluated.\"); System.exit(1);}"
      case "__[TC]UnknownEventError__" => s"UnknownEventError(${args(0)})"

      case "__ite__" | "__staticite__"                  => s"(if (${args(0)}) ${args(1)} else ${args(2)})"
      case "__not__"                                    => s"!(${args(0)})"
      case "__negate__" | "__fnegate__"                 => s"-${args(0)}"
      case "__bitflip__"                                => s"~${args(0)}"
      case "__and__"                                    => "(" + args.mkString(" && ") + ")"
      case "__or__"                                     => "(" + args.mkString(" || ") + ")"
      case "__eq__"                                     => s"(${args(0)} == ${args(1)})"
      case "__neq__"                                    => s"(${args(0)} != ${args(1)})"
      case "__gt__" | "__fgt__"                         => s"(${args(0)} > ${args(1)})"
      case "__lt__" | "__flt__"                         => s"(${args(0)} < ${args(1)})"
      case "__geq__" | "__fgeq__"                       => s"(${args(0)} >= ${args(1)})"
      case "__leq__" | "__fleq__"                       => s"(${args(0)} <= ${args(1)})"
      case "__add__" | "__fadd__" | "__String_concat__" => s"(${args(0)} + ${args(1)})"
      case "__sub__" | "__fsub__"                       => s"(${args(0)} - ${args(1)})"
      case "__mul__" | "__fmul__"                       => s"(${args(0)} * ${args(1)})"
      case "__div__" | "__fdiv__"                       => s"(${args(0)} / ${args(1)})"
      case "__mod__"                                    => s"(${args(0)} % ${args(1)})"
      case "__bitand__"                                 => "(" + args.mkString(" & ") + ")"
      case "__bitor__"                                  => "(" + args.mkString(" | ") + ")"
      case "__bitxor__"                                 => "(" + args.mkString(" ^ ") + ")"
      case "__leftshift__"                              => "(" + s"${args(0)} << ${args(1)}" + ")"
      case "__rightshift__"                             => "(" + s"${args(0)} >> ${args(1)}" + ")"

      case "__pow__"  => s"java.lang.Math.pow(${args(0)}, ${args(1)})"
      case "__log__"  => s"(java.lang.Math.log(${args(0)}) / java.lang.Math.log(${args(1)}))"
      case "__sin__"  => s"java.lang.Math.sin(${args(0)})"
      case "__cos__"  => s"java.lang.Math.cos(${args(0)})"
      case "__tan__"  => s"java.lang.Math.tan(${args(0)})"
      case "__atan__" => s"java.lang.Math.atan(${args(0)})"

      case "__intToFloat__" => s"${args(0)}.asInstanceOf[Float]"
      case "__floatToInt__" => s"${args(0)}.asInstanceOf[Long]"

      case "__Some__"    => s"EOSome(${args(0)})"
      case "__None__"    => s"EONone()"
      case "__getSome__" => s"${args(0)}.get"
      case "__isSome__"  => s"${args(0)}.isDefined"
      case "__isNone__"  => s"${args(0)}.isEmpty"

      case "__toString__" if typeHint.argsTypes.head == GeneralType => s"${args(0)}.toString"
      case "__toString__"                                           => ScalaIOHandling.getParseExpressionToString(typeHint.argsTypes.head, args(0))
      case "__String_format__"                                      => s"${args(0)}.formatLocal(java.util.Locale.ROOT, ${args(1)})"

      case "__Map_empty__" => "Map()"
      case "__Map_add__" if typeHint.retType.isInstanceOf[MutableMapType] =>
        s"(${args(0)} += ((${args(1)}) -> (${args(2)})))"
      case "__Map_add__"      => s"(${args(0)} + ((${args(1)}) -> (${args(2)})))"
      case "__Map_contains__" => s"${args(0)}.contains(${args(1)})"
      case "__Map_get__"      => s"${args(0)}(${args(1)})"
      case "__Map_remove__"   => s"(${args(0)} - ${args(1)})"
      case "__Map_size__"     => s"${args(0)}.size"
      case "__Map_fold__" =>
        s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)}){case (c, (k, v)) => val f = ${args(2)}; f(c, k, v)}"
      case "__Map_keys__" => s"${args(0)}.keys.toList"

      case "__Set_empty__"        => "Set()"
      case "__Set_add__"          => s"(${args(0)} + (${args(1)}))"
      case "__Set_contains__"     => s"${args(0)}(${args(1)})"
      case "__Set_remove__"       => s"(${args(0)} - ${args(1)})"
      case "__Set_size__"         => s"${args(0)}.size"
      case "__Set_union__"        => s"${args(0)}.union(${args(1)})"
      case "__Set_intersection__" => s"${args(0)}.intersect(${args(1)})"
      case "__Set_minus__"        => s"(${args(0)} -- ${args(1)})"
      case "__Set_fold__"         => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)})(${args(2)})"

      case "__List_empty__"   => s"List()"
      case "__List_size__"    => s"${args(0)}.size"
      case "__List_append__"  => s"(${args(0)} :+ ${args(1)})"
      case "__List_prepend__" => s"(${args(0)} +: ${args(1)})"
      case "__List_tail__"    => s"${args(0)}.tail"
      case "__List_init__"    => s"${args(0)}.init"
      case "__List_get__"     => s"${args(0)}(${args(1)}.asInstanceOf[Int])"
      case "__List_set__" if typeHint.retType.isInstanceOf[MutableListType] =>
        s"${args(0)}.update(${args(1)}.asInstanceOf[Int], ${args(2)})"
      case "__List_set__"  => s"${args(0)}.updated(${args(1)}.asInstanceOf[Int], ${args(2)})"
      case "__List_fold__" => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)})(${args(2)})"

      case "__getStruct__" =>
        typeHint match {
          case FunctionType(Seq(StructType(_, fieldNames), IntermediateCode.StringType), _) =>
            val fieldName = oArgs(1).asInstanceOf[StringValue].value
            val add = if (fieldNames.size == 1) "" else s"._${fieldNames.indexOf(fieldName) + 1}"
            s"${args(0)}$add"
          case _ => throw Diagnostics.DSLError(s"__getStruct__ call has wrong type hint $typeHint")
        }
      case "__mkStruct__" => s"(${args.mkString(", ")})"

      case s if s.startsWith("__native:") => s"${s.stripPrefix("__native:").stripSuffix("__")}(${args.mkString(", ")})"

      case "__error__" => "(throw new java.lang.Exception(\"Runtime error: \" + " + args(0) + "))"
      case _           => throw Diagnostics.CommandNotSupportedError(s"Unsupported built-in function for Scala backend: $name")
    }
  }

}

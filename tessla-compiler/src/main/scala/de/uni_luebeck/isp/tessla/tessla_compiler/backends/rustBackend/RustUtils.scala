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

package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

import de.uni_luebeck.isp.tessla.core.TesslaAST
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.{LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils.{stringToVariable, structComparison}

import scala.collection.mutable.ListBuffer

object RustUtils {

  /**
   * Converts TeSSLa type to corresponding rust types
   *
   * @param t Type to be converted. If type is Events[t] the result is equal to calling the function with t.
   * @return The converted type
   */
  def convertType(t: Type): String = {
    t match {
      case InstantiatedType("Events", Seq(t), _)                    => convertType(t)
      case RecordType(entries, _) if entries.isEmpty                => "TesslaUnit"
      case InstantiatedType("Bool", Seq(), _)                       => "TesslaBool"
      case InstantiatedType("Int", Seq(), _)                        => "TesslaInt"
      case InstantiatedType("Float", Seq(), _)                      => "TesslaFloat"
      case InstantiatedType("String", Seq(), _)                     => "TesslaString"
      case InstantiatedType("Option", Seq(t), _)                    => s"TesslaOption<${convertType(t)}>"
      case InstantiatedType("Set", Seq(t), _)                       => s"im::set<${convertType(t)}>"
      case InstantiatedType("Map", Seq(t1, t2), _)                  => s"im::map<${convertType(t1)}, ${convertType(t2)}>"
      case InstantiatedType("List", Seq(t), _)                      => s"im::vec<${convertType(t)}>"
      case InstantiatedType(n, Seq(), _) if n.startsWith("native:") => n.stripPrefix("native:")
      case InstantiatedType(n, tps, _) if n.startsWith("native:") =>
        s"${n.stripPrefix("native:")}<${tps.map(convertType).mkString(", ")}>"
      case FunctionType(_, paramTypes, resultType, _) =>
        s"""fn(${paramTypes
          .map {
            case (LazyEvaluation, t)   => s"Lazy<${convertType(t)}>"
            case (StrictEvaluation, t) => convertType(t)
          }
          .mkString(", ")}) -> ${convertType(resultType)}"""
      case RecordType(entries, _) => {
        // FIXME Frankly these need to be handled differently
        //  since rust does not allow anonymous structs, we need to name them,
        //  perhaps in a repeatable way, and then instantiate the type once in <static>
        val sortedEntries = entries.toSeq.sortWith { case ((n1, _), (n2, _)) => structComparison(n1.name, n2.name) }
        val names = sortedEntries.map(_._1.name) // TODO named tuple...
        val types = sortedEntries.map { case (_, t) => convertType(t._1) }
        s"(${types.mkString(", ")})"
      }
      case TypeParam(name, _) => name.toString
      case _ =>
        throw Diagnostics.CommandNotSupportedError(s"Type translation for type $t not supported")
    }
  }

  /**
   * Extracts all [[TypeParam]] generic types used in a list of types
   *
   * @param types a list of types
   * @return a [[Set]] containing the names of all generic types used
   */
  def getGenericTypeNames(types: Iterable[Type]): Set[Identifier] = {
    types
      .collect {
        case InstantiatedType(_, Seq(), _) => Seq()
        case InstantiatedType(_, types, _) => getGenericTypeNames(types)
        case FunctionType(_, paramTypes, resultType, _) =>
          (getGenericTypeNames(paramTypes.map { case (_, typ) => typ })
            ++ getGenericTypeNames(Seq(resultType)))
        case RecordType(entries, _) =>
          getGenericTypeNames(entries.map { case (_, (t, _)) => t })
        case TypeParam(name, _) => Seq(name)
      }
      .flatten
      .toSet
  }

  /**
   * Performs the translation of calls to built-in function to a Rust expression
   * Note: Mutable datastructures are not yet handled on this branch since they are not generated
   * @param name Name of the function which is called
   * @param oArgs Argument expressions of the function call
   * @param typeHint The type signature of the called function.
   * @return The translated function call in Rust
   */
  def translateBuiltinFunctionCall(
    name: String,
    oArgs: Seq[String],
    typeHint: FunctionType
  ): String = {
    val args = oArgs.toIndexedSeq
    name match {
      case "__[TC]inputParse__" => "" // TODO RustIOHandling.getInputParseExpression(typeHint.retType, args(0))

      // TODO some brackets here are superfluous and will produce warnings
      case "__ite__" | "__staticite__"  => s"if ${args(0)} { ${args(1)} } else { ${args(2)} }"
      case "__not__"                    => s"!(${args(0)})"
      case "__negate__" | "__fnegate__" => s"-${args(0)}"
      case "__bitflip__"                => s"!${args(0)}"
      case "__and__"                    => "(" + args.mkString(" && ") + ")"
      case "__or__"                     => "(" + args.mkString(" || ") + ")"
      case "__eq__"                     => s"(${args(0)} == ${args(1)})"
      case "__neq__"                    => s"(${args(0)} != ${args(1)})"
      case "__gt__" | "__fgt__"         => s"(${args(0)} > ${args(1)})"
      case "__lt__" | "__flt__"         => s"(${args(0)} < ${args(1)})"
      case "__geq__" | "__fgeq__"       => s"(${args(0)} >= ${args(1)})"
      case "__leq__" | "__fleq__"       => s"(${args(0)} <= ${args(1)})"
      case "__add__" | "__fadd__"       => s"(${args(0)} + ${args(1)})"
      case "__String_concat__"          => s"(${args(0)} + ${args(1)}.as_str())"
      case "__sub__" | "__fsub__"       => s"(${args(0)} - ${args(1)})"
      case "__mul__" | "__fmul__"       => s"(${args(0)} * ${args(1)})"
      case "__div__" | "__fdiv__"       => s"(${args(0)} / ${args(1)})"
      case "__mod__"                    => s"(${args(0)} % ${args(1)})"
      case "__bitand__"                 => "(" + args.mkString(" & ") + ")"
      case "__bitor__"                  => "(" + args.mkString(" | ") + ")"
      case "__bitxor__"                 => "(" + args.mkString(" ^ ") + ")"
      case "__leftshift__"              => "(" + s"${args(0)} << ${args(1)}" + ")"
      case "__rightshift__"             => "(" + s"${args(0)} >> ${args(1)}" + ")"

      case "__pow__"  => s"${args(0)}.powf(${args(1)})"
      case "__log__"  => s"${args(0)}.log(${args(1)})"
      case "__sin__"  => s"${args(0)}.sin()"
      case "__cos__"  => s"${args(0)}.cos()"
      case "__tan__"  => s"${args(0)}.tan()"
      case "__atan__" => s"${args(0)}.atan()"

      case "__intToFloat__" => s"${args(0)} as f64"
      case "__floatToInt__" => s"${args(0)} as i64"

      case "__Some__"    => s"Value(Some(${args(0)}))"
      case "__None__"    => s"Value(None)"
      case "__getSome__" => s"${args(0)}.get_some()"
      case "__isSome__"  => s"${args(0)}.is_some()"
      case "__isNone__"  => s"${args(0)}.is_none()"

      // FIXME this will probably fail, cause the underlying String is not stored anywhere
      case "__toString__" => s"${args(0)}.to_string()"
      // TODO format string syntax is entirely different in rust???
      //  s"format!(${args(0)}, ${args(1)})"
      case "__String_format__" =>
        throw Diagnostics.CommandNotSupportedError("Format requires a statically known format string")

      /* TODO https://docs.rs/im/15.0.0/im/
      case "__Map_empty__" => "im::HashMap::new()"
      case "__Map_add__" if typeHint.retType.isInstanceOf[MutableMapType] =>
        s"${args(0)}.insert(${args(1)}, ${args(2)})"
      case "__Map_add__"      => s"${args(0)} + ((${args(1)}) -> (${args(2)}))"
      case "__Map_contains__" => s"${args(0)}.contains_key(${args(1)})"
      case "__Map_get__"      => s"${args(0)}.get(${args(1)})"
      case "__Map_remove__"   => s"${args(0)}.remove(${args(1)})"
      case "__Map_size__"     => s"${args(0)}.len()"
      case "__Map_fold__" =>
        s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)}){case (c, (k, v)) => val f = ${args(2)}; f(c, k, v)}"
      case "__Map_keys__" => s"Vec::from_iter(${args(0)}.keys())"

      case "__Set_empty__"        => "std::collections::HashSet::new()"
      case "__Set_add__"          => s"${args(0)}.insert(${args(1)})"
      case "__Set_contains__"     => s"${args(0)}.contains(${args(1)})"
      case "__Set_remove__"       => s"${args(0)}.remove(${args(1)})"
      case "__Set_size__"         => s"${args(0)}.len()"
      case "__Set_union__"        => s"${args(0)}.union(${args(1)})"
      case "__Set_intersection__" => s"${args(0)}.intersection(${args(1)})"
      case "__Set_minus__"        => s"${args(0)}.difference(${args(1)})"
      case "__Set_fold__"         => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)})(${args(2)})"

      case "__List_empty__"   => s"std::vec::Vec::new()"
      case "__List_size__"    => s"${args(0)}.len()"
      case "__List_append__"  => s"${args(0)}.append(${args(1)})"
      case "__List_prepend__" => s"${args(0)}.insert(0, ${args(1)})"
      case "__List_tail__"    => s"${args(0)}.split_off(1)"
      case "__List_init__"    => s"${args(0)}.truncate(${args(0)}.len() - 1)"
      case "__List_get__"     => s"${args(0)}[${args(1)} as usize]"
      case "__List_set__" if typeHint.retType.isInstanceOf[MutableListType] =>
        s"${args(0)}.insert(${args(1)} as usize, ${args(2)})"
      case "__List_set__"  => s"${args(0)}.updated(${args(1)}.asInstanceOf[Int], ${args(2)})"
      case "__List_fold__" => s"${args(0)}.foldLeft[${typeTranslation(typeHint.argsTypes(1))}](${args(1)})(${args(2)})"
       */

      case _ => throw Diagnostics.CommandNotSupportedError(s"Unsupported built-in function for Rust backend: $name")
    }
  }
}

case class SourceSegments(
  variables: ListBuffer[String] = new ListBuffer[String],
  stateDef: ListBuffer[String] = new ListBuffer[String],
  stateInit: ListBuffer[String] = new ListBuffer[String],
  input: ListBuffer[String] = new ListBuffer[String],
  timestamp: ListBuffer[String] = new ListBuffer[String],
  computation: ListBuffer[String] = new ListBuffer[String],
  output: ListBuffer[String] = new ListBuffer[String],
  static: ListBuffer[String] = new ListBuffer[String],
  store: ListBuffer[String] = new ListBuffer[String]
)

/*
 * Copyright 2022 The TeSSLa Community
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

import de.uni_luebeck.isp.tessla.core.Location
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.Diagnostics
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils.structComparison

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object RustUtils {

  val NON_ALPHA_PATTERN: Regex = "[^a-zA-Z0-9_\\p{L}\\p{M}\\p{N}]".r

  /**
   * Converts TeSSLa type to corresponding rust types
   *
   * @param t Type to be converted. If type is Events[t] the result is equal to calling the function with t.
   * @param mask_generics Replace all generic types with _, resulting in them being inferred by rust.
   *                      This is needed in anonymous functions, where you cannot specify generic types
   * @param use_abstract_fn_type Where possible we want to use this type, since it does not limit what exactly is put in.
   *                             At the moment this type description is only allowed for arguments and return types of
   *                             proper static functions: [[https://doc.rust-lang.org/reference/types/impl-trait.html]]
   *
   *                             In places where we cannot use this, we instead use {{{Box<dyn Fn(...) -> ..>}}}
   * @return The converted type
   */
  def convertType(t: Type, mask_generics: Boolean = false, use_abstract_fn_type: Boolean = false): String = {
    t match {
      case InstantiatedType("Events", Seq(t), _)     => convertType(t, mask_generics, use_abstract_fn_type)
      case RecordType(entries, _) if entries.isEmpty => "TesslaUnit"
      case InstantiatedType("Bool", Seq(), _)        => "TesslaBool"
      case InstantiatedType("Int", Seq(), _)         => "TesslaInt"
      case InstantiatedType("Float", Seq(), _)       => "TesslaFloat"
      case InstantiatedType("String", Seq(), _)      => "TesslaString"
      case InstantiatedType("Option", Seq(t), _) =>
        s"TesslaOption<${convertType(t, mask_generics, use_abstract_fn_type)}>"
      case InstantiatedType("Set", Seq(t), _) =>
        s"TesslaSet<${convertType(t, mask_generics, use_abstract_fn_type)}>"
      case InstantiatedType("Map", Seq(t1, t2), _) =>
        s"TesslaMap<${convertType(t1, mask_generics, use_abstract_fn_type)}, ${convertType(t2, mask_generics, use_abstract_fn_type)}>"
      case InstantiatedType("List", Seq(t), _) =>
        s"TesslaList<${convertType(t, mask_generics, use_abstract_fn_type)}>"
      case InstantiatedType(n, Seq(), _) if n.startsWith("native:") => n.stripPrefix("native:")
      case InstantiatedType(n, tps, _) if n.startsWith("native:") =>
        s"${n.stripPrefix("native:")}<${tps
          .map { t => convertType(t, mask_generics, use_abstract_fn_type) }
          .mkString(", ")}>"
      case FunctionType(_, paramTypes, resultType, _) =>
        val params = paramTypes
          .map {
            // FIXME case (LazyEvaluation, t) =>
            case (_, t) => convertType(t, mask_generics, use_abstract_fn_type)
          }
          .mkString(", ")
        val result = convertType(resultType, mask_generics, use_abstract_fn_type)
        if (use_abstract_fn_type)
          s"impl Fn($params) -> $result"
        else
          s"Box<dyn Fn($params) -> $result>"
      case RecordType(entries, _) =>
        val genericBounds = getGenericTraitBounds(entries.map { case (_, (typ, _)) => typ })("")
        s"TesslaValue<${RustUtils.getStructName(entries)}$genericBounds>"
      case TypeParam(name, _) => s"TesslaValue<${if (mask_generics) "_" else name.toString}>"
      case _ =>
        throw Diagnostics.CommandNotSupportedError(s"Type translation for type $t not supported")
    }
  }

  /**
   * Helper to determine whether a type can be hashed in Rust.
   * The only types that cannot be hashed are Floats and Functions
   * @param t a type
   * @return a bool describing if it is completely made up of hashable types
   */
  def canBeHashed(t: Type): Boolean = {
    t match {
      case InstantiatedType("Float", Seq(), _)     => false
      case InstantiatedType("Events", Seq(t), _)   => canBeHashed(t)
      case InstantiatedType("Bool", Seq(), _)      => true
      case InstantiatedType("Int", Seq(), _)       => true
      case InstantiatedType("String", Seq(), _)    => true
      case InstantiatedType("Option", Seq(t), _)   => canBeHashed(t)
      case InstantiatedType("Set", Seq(t), _)      => canBeHashed(t)
      case InstantiatedType("Map", Seq(t1, t2), _) => canBeHashed(t1) && canBeHashed(t2)
      case InstantiatedType("List", Seq(t), _)     => canBeHashed(t)
      case TypeParam(_, _)                         => false
      case InstantiatedType(n, types, _) if n.startsWith("native:") =>
        types.forall { t => canBeHashed(t) }
      case RecordType(entries, _) =>
        entries.forall { case (_, (tpe, _)) => canBeHashed(tpe) }
      case FunctionType(_, _, _, _) =>
        throw Diagnostics.CommandNotSupportedError("Function type can not be hashed")
      case _ =>
        throw Diagnostics.CommandNotSupportedError(s"Found unknown type $t")
    }
  }

  /**
   * Extracts all [[TypeParam]] types used in a list of types, and returns a function that takes a string of trait bounds.
   *
   * This function either returns an empty string, if no generic types were found, or
   *
   * - if an empty trait bound string is given, the generic types are just listed &lt;T1, T2, ...&gt;
   *
   * - otherwise each type gets that trait bound, eg: &lt;T1: Trait1 + Trait2, T2: Trait1 + Trait2, ...&gt;
   *
   *
   * @param types a list of types
   * @return a function that takes a string specifying the trait bounds to be applied to all generic type
   */
  def getGenericTraitBounds(types: Iterable[Type]): String => String = {
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
    val genericTypes = getGenericTypeNames(types)
    (requiredTraits: String) => {
      if (genericTypes.nonEmpty)
        s"<${genericTypes
          .map { name => if (requiredTraits.nonEmpty) s"$name: $requiredTraits" else name }
          .mkString(", ")}>"
      else
        ""
    }
  }

  /**
   * Create a struct name repeatably from the field names and their types
   * This name, along with the signature is also stored, so that the struct can be generated
   * @param fields the field names and their types
   * @return the name for that struct datatype
   */
  def getStructName(fields: Map[String, (Type, Location)]): String = {
    NON_ALPHA_PATTERN.replaceAllIn(
      s"Structſ${fields.toSeq
        .map { case (name, (tpe, _)) => (name, tpe) }
        .sortWith { case ((n1, _), (n2, _)) => structComparison(n1, n2) }
        .map { case (name, tpe) => s"${name.capitalize}þ$tpe" }
        .mkString("ſ")}",
      "ø"
    )
  }

  /**
   * Figures out if a struct is a tuple. i.e. has field names _1,_2 ...
   * @param fieldNames The struct field names
   * @return Whether given struct is tuple
   */
  def isStructTuple(fieldNames: Seq[String]): Boolean = {
    fieldNames.indices.forall(i => fieldNames.contains(s"_${i + 1}"))
  }
}

case class SourceSegments(
  userIncludes: ListBuffer[String] = new ListBuffer[String], //TODO
  stateDef: ListBuffer[String] = new ListBuffer[String],
  stateInit: ListBuffer[String] = new ListBuffer[String],
  input: ListBuffer[String] = new ListBuffer[String],
  timestamp: ListBuffer[String] = new ListBuffer[String],
  computation: ListBuffer[String] = new ListBuffer[String],
  lazyStatic: ListBuffer[String] = new ListBuffer[String],
  static: ListBuffer[String] = new ListBuffer[String],
  store: ListBuffer[String] = new ListBuffer[String],
  delayReset: ListBuffer[String] = new ListBuffer[String]
)

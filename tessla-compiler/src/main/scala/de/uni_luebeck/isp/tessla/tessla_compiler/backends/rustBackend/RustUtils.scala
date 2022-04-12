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
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.preprocessing.SanitizeIdentifiers

import scala.collection.mutable.ListBuffer
import scala.io.Source

object RustUtils {

  /**
   * Converts TeSSLa type to corresponding rust types
   *
   * @param t Type to be converted. If type is Events[t] the result is equal to calling the function with t.
   * @param mask_generics Replace all generic types with _, resulting in them being inferred by rust.
   *                      This is needed in anonymous functions, where you cannot specify generic types
   * @param wrap_type In some places we need the underlying type without wrapping it in TesslaValue&lt;...&gt;.
   *                  For example in type parameters for structs: this way we can define structs such that
   *                  their fields are guaranteed to be a TesslaValue with their content type as a type parameter.
   * @return The converted type
   */
  def convertType(
    t: Type,
    mask_generics: Boolean,
    wrap_type: Boolean = true
  ): String = {
    val typName = t match {
      case InstantiatedType("Events", Seq(t), _)     => convertType(t, mask_generics, wrap_type = false)
      case RecordType(entries, _) if entries.isEmpty => "()"
      case InstantiatedType("Bool", Seq(), _)        => "bool"
      case InstantiatedType("Int", Seq(), _)         => "i64"
      case InstantiatedType("Float", Seq(), _)       => "f64"
      case InstantiatedType("String", Seq(), _)      => "String"
      case InstantiatedType("Option", Seq(t), _) =>
        s"Option<${convertType(t, mask_generics)}>"
      case InstantiatedType("Set", Seq(t), _) =>
        s"HashSet<${convertType(t, mask_generics)}>"
      case InstantiatedType("Map", Seq(t1, t2), _) =>
        s"HashMap<${convertType(t1, mask_generics)}, ${convertType(t2, mask_generics)}>"
      case InstantiatedType("List", Seq(t), _) =>
        s"Vector<${convertType(t, mask_generics)}>"
      case InstantiatedType(n, Seq(), _) if n.startsWith("native:") => n.stripPrefix("native:")
      case InstantiatedType(n, tps, _) if n.startsWith("native:") =>
        s"${n.stripPrefix("native:")}<${tps
          .map { t => convertType(t, mask_generics) }
          .mkString(", ")}>"
      case FunctionType(_, paramTypes, resultType, _) =>
        // abstract fn types may not be used for parameter/return types of abstract functions
        // reverting to [use_abstract_fn_type = false] is deliberate here
        val params = paramTypes
          .map {
            // FIXME case (LazyEvaluation, t) =>
            case (_, t) => convertType(t, mask_generics)
          }
          .mkString(", ")
        val result = convertType(resultType, mask_generics)
        s"Rc<dyn Fn($params) -> $result>"
      case RecordType(entries, _) =>
        val typeParams = entries.toSeq
          .sortWith { case ((name1, _), (name2, _)) => structComparison(name1, name2) }
          .map { case (_, (typ, _)) => typ }
        if (isStructTuple(entries.toSeq.map { case (name, _) => name }))
          s"(${typeParams.map(convertType(_, mask_generics)).mkString(", ")},)"
        else
          s"${getStructName(entries)}<${typeParams.map(convertType(_, mask_generics, wrap_type = false)).mkString(", ")}>"
      case TypeParam(name, _) => if (mask_generics) "_" else name.toString
      case _ =>
        throw Diagnostics.CommandNotSupportedError(s"Type translation for type $t not supported")
    }
    if (wrap_type) s"TesslaValue<$typName>" else typName
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
   * Extracts all [[TypeParam]] types used in a list of types, and returns a set of their names.
   *
   * @param types a list of types
   * @return a set of all generic types used
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
   * Replaces the type of all fields of a [[RecordType]] with [[TypeParam]]s.
   * @param record The [[RecordType]] to modify.
   * @return The modified [[RecordType]].
   */
  def genericiseRecordType(record: RecordType): RecordType = record match {
    case RecordType(entries, location) =>
      RecordType(
        entries.map {
          case (field, (typ: TypeParam, location)) => (field, (typ, location))
          case (field, (_, location))              => (field, (TypeParam(Identifier(s"ST_$field")), location))
        },
        location
      )
  }

  /**
   * Create a struct name repeatably from the field names
   *
   * @param fields the field names and their types
   * @return the name for that struct datatype
   */
  def getStructName(fields: Map[String, (Type, Location)]): String = {
    s"Struct_${fields.toSeq
      .map { case (name, _) => name }
      .sortWith(structComparison)
      .map(SanitizeIdentifiers.escapeName)
      .mkString("_")}"
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

/**
 * Contains buffers for all segments where code should be inserted.
 */
case class SourceSegments(
  stateDef: ListBuffer[String] = new ListBuffer[String],
  stateStatic: ListBuffer[String] = new ListBuffer[String],
  stateInit: ListBuffer[String] = new ListBuffer[String],
  input: ListBuffer[String] = new ListBuffer[String],
  timestamp: ListBuffer[String] = new ListBuffer[String],
  computation: ListBuffer[String] = new ListBuffer[String],
  static: ListBuffer[String] = new ListBuffer[String],
  store: ListBuffer[String] = new ListBuffer[String],
  delayReset: ListBuffer[String] = new ListBuffer[String]
) {

  /**
   * Inserts the specified segments into the template.
   * @param monitorTemplate Path to the monitor library template.
   * @param userIncludes The additional user includes.
   * @param mainTemplate Path to the template for the main file handling IO.
   * @return A collection of Rust source files.
   */
  def insertSegments(monitorTemplate: String, userIncludes: String, mainTemplate: String): RustFiles = {
    RustFiles(
      Source
        .fromResource(monitorTemplate)
        .mkString
        .replace("//USERINCLUDES", userIncludes)
        .replace("//STATEDEF", stateDef.mkString(",\n"))
        .replace("//STATIC", static.mkString("\n"))
        .replace("//STATESTATIC", stateStatic.mkString("\n"))
        .replace("//STATEINIT", stateInit.mkString(",\n"))
        .replace("//STORE", store.mkString("\n"))
        .replace("//TIMESTAMP", timestamp.mkString("\n"))
        .replace("//COMPUTATION", computation.mkString("\n"))
        .replace("//DELAYRESET", delayReset.mkString("\n")),
      Source
        .fromResource(mainTemplate)
        .mkString
        .replace("//INPUT", input.mkString("\n"))
    )
  }
}

/**
 * Contains the content of the translated Rust file.
 * @param monitor The monitor library.
 * @param main The main file handling IO.
 */
case class RustFiles(monitor: String, main: String)

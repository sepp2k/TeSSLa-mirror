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
      case RecordType(entries, _) if entries.isEmpty                => "()" // unit type
      case InstantiatedType("Bool", Seq(), _)                       => "bool"
      case InstantiatedType("Int", Seq(), _)                        => "u64"
      case InstantiatedType("Float", Seq(), _)                      => "f64"
      case InstantiatedType("String", Seq(), _)                     => "&str"
      case InstantiatedType("Option", Seq(t), _)                    => s"Option<${convertType(t)}>"
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
        //  perhaps in a repeatable way, and then instantiate the type once
        val sortedEntries = entries.toSeq.sortWith { case ((n1, _), (n2, _)) => structComparison(n1.name, n2.name) }
        val names = sortedEntries.map(_._1.name) // TODO named tuple...
        val types = sortedEntries.map { case (_, t) => convertType(t._1) }
        s"(${types.mkString(", ")})"
      }
      case TypeParam(name, _) => s"TypeId[$name]" // TODO type type?
      case _ =>
        throw Diagnostics.CommandNotSupportedError(s"Type translation for type $t not supported")
    }
  }
}

case class SourceSegments(
  variables: ListBuffer[String] = new ListBuffer[String],
  input: ListBuffer[String] = new ListBuffer[String],
  timestamp: ListBuffer[String] = new ListBuffer[String],
  computation: ListBuffer[String] = new ListBuffer[String],
  output: ListBuffer[String] = new ListBuffer[String],
  static: ListBuffer[String] = new ListBuffer[String],
  store: ListBuffer[String] = new ListBuffer[String]
)

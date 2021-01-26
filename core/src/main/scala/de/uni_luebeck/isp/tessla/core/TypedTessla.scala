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

package de.uni_luebeck.isp.tessla.core

/**
 * An extension of the flattened Tessla AST, with all expressions annotated with their respective type.
 */
object TypedTessla extends FlatTessla {

  case class TypedSpecification(
    annotations: Seq[Annotation],
    globalDefs: Definitions,
    outStreams: Seq[OutStream],
    outAllLocation: Option[Location]
  ) {
    override def toString = {
      val outAllString = if (outAll) "\nout *" else ""
      s"${annotations.mkString("\n")}\n$globalDefs\n${outStreams.mkString("\n")}$outAllString"
    }

    def outAll = outAllLocation.isDefined
  }

  override type TypeAnnotation = Type

  override def typeAnnotationToString(typ: Type) = s" : $typ"
}

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

package de.uni_luebeck.isp.tessla.tessladoc

import de.uni_luebeck.isp.tessla.tessladoc.DocJsonProtocol._
import de.uni_luebeck.isp.tessla.core._
import spray.json._

sealed trait Statement

sealed trait TesslaDoc extends Statement {
  def doc: String
}

/**
 * Contains the data structures for different documentation elements
 */

object TesslaDoc {

  case class Docs(items: Seq[TesslaDoc], imports: Seq[Import]) {
    override def toString: String = this.toJson.prettyPrint
  }

  case class DefDoc(
    name: String,
    src: String,
    typeParameters: Seq[String],
    parameters: Seq[Param],
    returnType: Option[Type],
    doc: String,
    loc: Location,
    isLiftable: Boolean
  ) extends TesslaDoc

  case class AnnotationDoc(
    name: String,
    parameters: Seq[Param],
    global: Boolean,
    doc: String,
    inModule: Option[String],
    loc: Location
  ) extends TesslaDoc

  case class TypeDoc(name: String, typeParameters: Seq[String], doc: String, inModule: Option[String], loc: Location)
      extends TesslaDoc

  case class ModuleDoc(name: String, doc: String, members: Seq[TesslaDoc], imports: Seq[Import], loc: Location)
      extends TesslaDoc

  case class Import(path: Seq[String]) extends Statement

  case class Param(name: String, typ: EvalType)

  sealed abstract class Type

  case class EvalType(eval: String, typ: Type)

  case class SimpleType(name: String) extends Type

  case class TypeApplication(constructor: Type, arguments: Seq[Type]) extends Type

  case class FunctionType(parameters: Seq[EvalType], result: Type) extends Type

  case class ObjectType(members: Map[String, Type]) extends Type

  case class TupleType(members: Seq[Type]) extends Type
}

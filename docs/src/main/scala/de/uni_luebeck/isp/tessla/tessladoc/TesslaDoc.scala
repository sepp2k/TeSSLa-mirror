/*
 * Copyright (c) 2020 Institute of Software Engineering and Programming Languages,
 * University of Lübeck, Germany
 *
 * Modified MIT license:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this binary (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software and the code which is
 * generated by the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

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

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import spray.json.*

/**
 * Contains functionality to transform annotations of a specification into a Json format.
 * */
object AnnotationsToJson {

  case class Annotation(name: String, args: Map[String, AnnotationValue])
  case class AnnotationValue(value: String, `type`: String)
  case class Stream(name: String, `type`: String, annotations: Seq[Annotation])

  case class Specification(
    global: Seq[Annotation],
    inputs: Seq[Stream],
    outputs: Seq[Stream]
  )

  /**
   * The Json Protocol containing implicit formats for the data structures.
   * */
  object JsonProtocol extends DefaultJsonProtocol {
    implicit val annotationFormat: JsonFormat[Annotation] = lazyFormat(jsonFormat2(Annotation.apply))
    implicit val annotationValueFormat: JsonFormat[AnnotationValue] = jsonFormat2(AnnotationValue.apply)
    implicit val streamFormat: JsonFormat[Stream] = jsonFormat3(Stream.apply)
    implicit val specificationFormat: JsonFormat[Specification] = jsonFormat3(Specification.apply)
  }

  import JsonProtocol._

  /**
   * Extract the annotations (including argument names and types) from the specification and return
   * a prettified Json String.
   * */
  def apply(spec: Core.Specification): String = {
    val globals = translateAnnotations(spec.annotations)
    val inputs = spec.in.map {
      case (name, (typ, annotations)) =>
        Stream(name.toString, typ.toString, translateAnnotations(annotations))
    }.toSeq
    val outputs = spec.out.map {
      case (ref, annotations) =>
        Stream(ref.id.toString, ref.tpe.toString, translateAnnotations(annotations))
    }
    val annotationInfo = Specification(globals, inputs, outputs)
    annotationInfo.toJson.prettyPrint
  }

  private def translateAnnotations(annotations: Core.Annotations) =
    annotations.toSeq
      .flatMap {
        case (name, entries) => entries.map((name, _))
      }
      .map {
        case (name, args: Core.RecordConstructorExpression) =>
          val stringified = args.entries.view.mapValues(_._1).mapValues(translateAnnotationValue).toMap
          Annotation(name, stringified)
        case (name, value) =>
          Annotation(name, Map("value" -> translateAnnotationValue(value)))
      }

  // In case of a string literal, take the String value directly instead of printing the literal
  // since that would result in the value being wrapped in quotation marks, which will then be
  // escaped during Json generation, like "\"foo\""
  private def translateAnnotationValue(arg: Core.ExpressionArg): AnnotationValue = arg match {
    case s: Core.StringLiteralExpression => AnnotationValue(s.value, arg.tpe.toString)
    case _                               => AnnotationValue(arg.toString, arg.tpe.toString)
  }
}

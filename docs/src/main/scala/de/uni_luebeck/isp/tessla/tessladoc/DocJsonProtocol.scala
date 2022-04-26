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

package de.uni_luebeck.isp.tessla.tessladoc

import de.uni_luebeck.isp.tessla.core.Location
import de.uni_luebeck.isp.tessla.core.Location.SourceRange
import de.uni_luebeck.isp.tessla.tessladoc.TesslaDoc._
import spray.json._

/**
 * A Json protocol containing all formats required to generate the Tessla documentation in Json format.
 */
object DocJsonProtocol extends DefaultJsonProtocol {
  implicit val docsFormat: JsonFormat[Docs] = lazyFormat(jsonFormat2(Docs.apply))

  implicit val typeDocFormat: JsonFormat[TypeDoc] = lazyFormat(jsonFormat5(TypeDoc.apply))
  implicit val annotationDocFormat: JsonFormat[AnnotationDoc] = lazyFormat(jsonFormat6(AnnotationDoc.apply))
  implicit val moduleDocFormat: JsonFormat[ModuleDoc] = lazyFormat(jsonFormat5(ModuleDoc.apply))
  implicit val defDocFormat: JsonFormat[DefDoc] = lazyFormat(jsonFormat8(DefDoc.apply))

  implicit val paramFormat: JsonFormat[Param] = lazyFormat(jsonFormat2(Param.apply))

  implicit val simpleTypeFormat: JsonFormat[SimpleType] = jsonFormat1(SimpleType.apply)
  implicit val typeApplicationFormat: JsonFormat[TypeApplication] = lazyFormat(jsonFormat2(TypeApplication.apply))
  implicit val functionTypeFormat: JsonFormat[FunctionType] = lazyFormat(jsonFormat2(FunctionType.apply))
  implicit val objectTypeFormat: JsonFormat[ObjectType] = lazyFormat(jsonFormat1(ObjectType.apply))
  implicit val tupleTypeFormat: JsonFormat[TupleType] = lazyFormat(jsonFormat1(TupleType.apply))

  implicit val evalTypeFormat: JsonFormat[EvalType] = lazyFormat(jsonFormat2(EvalType.apply))
  implicit val importDocFormat: JsonFormat[Import] = jsonFormat1(Import.apply)

  // Enrich Json Values by a 'kind' field to distinguish different case classes
  implicit class RichJsValue(json: JsValue) {
    def withKind(kind: String): JsObject = JsObject(
      json.asJsObject.fields + ("kind" -> JsString(kind))
    )
    def kind: String = json.asJsObject.fields("kind").asInstanceOf[JsString].value
  }

  // Formats for traits, delegating to the different formats for the respective case-classes
  implicit val tesslaDocFormat: JsonFormat[TesslaDoc] = new JsonFormat[TesslaDoc] {
    override def write(doc: TesslaDoc): JsValue = doc match {
      case d: AnnotationDoc => d.toJson.withKind("AnnotationDoc")
      case d: TypeDoc       => d.toJson.withKind("TypeDoc")
      case d: ModuleDoc     => d.toJson.withKind("ModuleDoc")
      case d: DefDoc        => d.toJson.withKind("DefDoc")
    }

    override def read(json: JsValue): TesslaDoc = json.kind match {
      case "AnnotationDoc" => json.convertTo[AnnotationDoc]
      case "TypeDoc"       => json.convertTo[TypeDoc]
      case "ModuleDoc"     => json.convertTo[ModuleDoc]
      case "DefDoc"        => json.convertTo[DefDoc]
    }
  }

  implicit val typeFormat: JsonFormat[Type] = new JsonFormat[Type] {
    override def write(tpe: Type): JsValue = tpe match {
      case t: SimpleType      => t.toJson.withKind("SimpleType")
      case t: TypeApplication => t.toJson.withKind("TypeApplication")
      case t: FunctionType    => t.toJson.withKind("FunctionType")
      case t: ObjectType      => t.toJson.withKind("ObjectType")
      case t: TupleType       => t.toJson.withKind("TupleType")
    }

    override def read(json: JsValue): Type = json.kind match {
      case "SimpleType"      => json.convertTo[SimpleType]
      case "TypeApplication" => json.convertTo[TypeApplication]
      case "FunctionType"    => json.convertTo[FunctionType]
      case "ObjectType"      => json.convertTo[ObjectType]
      case "TupleType"       => json.convertTo[TupleType]
    }
  }

  implicit val locationFormat: JsonFormat[Location] = new JsonFormat[Location] {
    case class Loc(path: String, range: Option[Location.SourceRange])
    implicit val sourceRangeFormat: JsonFormat[SourceRange] = jsonFormat4(Location.SourceRange.apply)
    implicit val locFormat: JsonFormat[Loc] = jsonFormat2(Loc.apply)
    private val Opt = """option '(.*)'""".r

    override def write(loc: Location): JsValue = Loc(loc.path, loc.range).toJson

    override def read(json: JsValue): Location = {
      val loc = json.convertTo[Loc]
      loc.path match {
        case p if p == Location.builtIn.path => Location.builtIn
        case p if p == Location.unknown.path => Location.unknown
        case Opt(p)                          => Location.option(p)
        case _ =>
          val r = loc.range.get
          Location(r.fromLine, r.fromColumn, r.toLine, r.toColumn, loc.path)
      }
    }
  }
}

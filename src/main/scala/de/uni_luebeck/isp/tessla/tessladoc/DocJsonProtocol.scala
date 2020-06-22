package de.uni_luebeck.isp.tessla.tessladoc

import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.Location.SourceRange
import de.uni_luebeck.isp.tessla.tessladoc.TesslaDoc._
import spray.json._

object DocJsonProtocol extends DefaultJsonProtocol {
  implicit val docsFormat: JsonFormat[Docs] = lazyFormat(jsonFormat2(Docs))

  implicit val typeDocFormat: JsonFormat[TypeDoc] = lazyFormat(jsonFormat4(TypeDoc))
  implicit val annotationDocFormat: JsonFormat[AnnotationDoc] = lazyFormat(jsonFormat4(AnnotationDoc))
  implicit val moduleDocFormat: JsonFormat[ModuleDoc] = lazyFormat(jsonFormat5(ModuleDoc))
  implicit val defDocFormat: JsonFormat[DefDoc] = lazyFormat(jsonFormat8(DefDoc))

  implicit val paramFormat: JsonFormat[Param] = lazyFormat(jsonFormat2(Param))

  implicit val simpleTypeFormat: JsonFormat[SimpleType] = jsonFormat1(SimpleType)
  implicit val typeApplicationFormat: JsonFormat[TypeApplication] = lazyFormat(jsonFormat2(TypeApplication))
  implicit val functionTypeFormat: JsonFormat[FunctionType] = lazyFormat(jsonFormat2(FunctionType))
  implicit val objectTypeFormat: JsonFormat[ObjectType] = lazyFormat(jsonFormat1(ObjectType))
  implicit val tupleTypeFormat: JsonFormat[TupleType] = lazyFormat(jsonFormat1(TupleType))

  implicit val evalTypeFormat: JsonFormat[EvalType] = lazyFormat(jsonFormat2(EvalType))
  implicit val importDocFormat: JsonFormat[Import] = jsonFormat1(Import)

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

  implicit val scopeFormat: JsonFormat[Scope] = new JsonFormat[Scope] {
    override def write(scope: Scope): JsValue = scope match {
      case Global               => JsString("global")
      case Local(scopeLocation) => scopeLocation.toJson
    }

    override def read(json: JsValue): Scope = json match {
      case JsString("global") => Global
      case _                  => Local(json.convertTo[Location])
    }
  }

  implicit val locationFormat: JsonFormat[Location] = new JsonFormat[Location] {
    private case class Loc(path: String, range: Option[Location.SourceRange])
    implicit val sourceRangeFormat: JsonFormat[SourceRange] = jsonFormat4(Location.SourceRange)
    implicit val locFormat: JsonFormat[Loc] = jsonFormat2(Loc)
    val Opt = """option '(.*)'""".r

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

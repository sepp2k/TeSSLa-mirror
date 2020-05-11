package de.uni_luebeck.isp.tessla.tessladoc

import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.Location.SourceRange
import de.uni_luebeck.isp.tessla.tessladoc.TesslaDoc.{AnnotationDoc, DefDoc, Docs, FunctionType, Global, Local, ModuleDoc, ObjectType, Param, Scope, SimpleType, TupleType, Type, TypeApplication, TypeDoc}
import spray.json._

object DocJsonProtocol extends DefaultJsonProtocol {
  implicit val docsFormat: JsonFormat[Docs] = lift(new JsonWriter[Docs]() {
    override def write(docs: Docs): JsValue = docs.items.toJson
  })

  implicit val typeDocFormat: JsonFormat[TypeDoc] = lazyFormat(jsonFormat4(TypeDoc))
  implicit val annotationDocFormat: JsonFormat[AnnotationDoc] = lazyFormat(jsonFormat4(AnnotationDoc))
  implicit val moduleDocFormat: JsonFormat[ModuleDoc] = lazyFormat(jsonFormat4(ModuleDoc))
  implicit val defDocFormat: JsonFormat[DefDoc] = lazyFormat(jsonFormat8(DefDoc))

  implicit val paramFormat: JsonFormat[Param] = lazyFormat(jsonFormat2(Param))

  implicit val simpleTypeFormat: JsonFormat[SimpleType] = jsonFormat1(SimpleType)
  implicit val typeApplicationFormat: JsonFormat[TypeApplication] = lazyFormat(jsonFormat2(TypeApplication))
  implicit val functionTypeFormat: JsonFormat[FunctionType] = lazyFormat(jsonFormat2(FunctionType))
  implicit val objectTypeFormat: JsonFormat[ObjectType] = lazyFormat(jsonFormat1(ObjectType))
  implicit val tupleTypeFormat: JsonFormat[TupleType] = lazyFormat(jsonFormat1(TupleType))

  // Formats for traits, delegating to the different formats for the respective case-classes
  implicit val tesslaDocFormat: JsonFormat[TesslaDoc] = lift(new JsonWriter[TesslaDoc] {
    override def write(doc: TesslaDoc): JsValue = doc match {
      case d: AnnotationDoc => annotationDocFormat.write(d)
      case d: TypeDoc => typeDocFormat.write(d)
      case d: ModuleDoc => moduleDocFormat.write(d)
      case d: DefDoc => defDocFormat.write(d)
    }
  })

  implicit val typeFormat: JsonFormat[Type] = lift(new JsonWriter[Type] {
    override def write(tpe: Type): JsValue = tpe match {
      case t: SimpleType => simpleTypeFormat.write(t)
      case t: TypeApplication => typeApplicationFormat.write(t)
      case t: FunctionType => functionTypeFormat.write(t)
      case t: ObjectType => objectTypeFormat.write(t)
      case t: TupleType => tupleTypeFormat.write(t)
    }
  })

  implicit val scopeFormat: JsonFormat[Scope] = lift(new JsonWriter[Scope] {
    override def write(scope: Scope): JsValue = scope match {
      case Global => StringJsonFormat.write("global")
      case Local(scopeLocation) => locationFormat.write(scopeLocation)
    }
  })

  implicit val locationFormat: JsonFormat[Location] = lift(new JsonWriter[Location] {
    implicit val sourceRangeFormat: JsonFormat[SourceRange] = jsonFormat4(Location.SourceRange)

    private case class Loc(path: String, range: Option[Location.SourceRange])

    override def write(loc: Location): JsValue = Loc(loc.path, loc.range).toJson(jsonFormat2(Loc))
  })
}

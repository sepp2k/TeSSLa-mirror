package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import cats.data.Ior
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.{Location, TranslationPhase}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

import scala.collection.mutable

//TODO: Always pull out Set/Map/... constructor calls
//TODO: Make all identifier unique

class StreamDefFlattener extends TranslationPhase[Core.Specification, Core.Specification] {

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    var maxId: Long = spec.maxIdentifier
    val definitions = spec.definitions
    var newDef: mutable.Map[Identifier, DefinitionExpression] = mutable.Map()
    var warnings = Seq()

    def getId(id: Option[Identifier]): Identifier = {
      id.getOrElse(new Identifier(Ior.right({maxId += 1; maxId})))
    }

    def flatten(id: Option[Identifier], e: ExpressionArg): ExpressionArg = {
      e match {
        case ApplicationExpression(TypeApplicationExpression(e: ExternExpression, typeArgs, location), args, location2) => {
          val nid = getId(id)
          newDef += (nid -> ApplicationExpression(TypeApplicationExpression(e, typeArgs, location), args.map { a => flatten(None, a) }, location2))
          ExpressionRef(nid, e.tpe, Location.unknown)
        }
        case ApplicationExpression(e: ExternExpression, args, location) => {
          val nid = getId(id)
          newDef += (nid -> ApplicationExpression(e, args.map { a => flatten(None, a) }, location))
          ExpressionRef(nid, e.tpe, Location.unknown)
        }
        case e: ExpressionRef => e
        case _ => e
      }
    }


    definitions.foreach { case (id, definition) => {
      definition.tpe match {
        case InstantiatedType("Events", _, _) => flatten(Some(id), definition)
        case _ => newDef += (id -> definition)
      }
    }
    }

    Success(Core.Specification(spec.in, newDef.toMap, spec.out, maxId), warnings)
  }

}

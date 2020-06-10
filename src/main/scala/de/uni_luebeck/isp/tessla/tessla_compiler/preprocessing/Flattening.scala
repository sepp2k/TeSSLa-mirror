package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import cats.data.Ior
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TesslaAST.Core.{DefinitionExpression, _}
import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

import scala.collection.mutable

class Flattening(flattenOutFuncExterns: Boolean) extends TranslationPhase[Core.Specification, Core.Specification] {

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    var maxId: Long = spec.maxIdentifier

    def freshID(): Identifier = {
      new Identifier(Ior.right({maxId += 1; maxId}))
    }

    def externExpNeedsFlattening(e: ExternExpression): Boolean = {
      e match {
        case ExternExpression("true", _, _) |
             ExternExpression("false", _, _) => false
        case _ => flattenOutFuncExterns
      }
    }

    def flattenExpressionArg(e: ExpressionArg, addDefs: mutable.Map[Identifier, DefinitionExpression]): ExpressionArg = {
      e match {
        case r: ExpressionRef => r
        case _: IntLiteralExpression |
             _: StringLiteralExpression |
             _: FloatLiteralExpression |
             _: TypeApplicationExpression =>
          flattenExpression(e.asInstanceOf[DefinitionExpression], addDefs)
        case e: ExternExpression if !externExpNeedsFlattening(e) =>
          flattenExpression(e.asInstanceOf[DefinitionExpression], addDefs)
        case RecordConstructorExpression(m, _) if m.isEmpty =>
          flattenExpression(e.asInstanceOf[DefinitionExpression], addDefs)
        case e:  Expression =>
          val newID = freshID()
          val flattenedExp = flattenExpression(e, addDefs)
          addDefs.addOne(newID -> flattenedExp)
          ExpressionRef(newID, e.tpe)
      }
    }

    def flattenExpression(e: DefinitionExpression, addDefs: mutable.Map[Identifier, DefinitionExpression]): DefinitionExpression = {
      e match {
        case FunctionExpression(typeParams, params, body, result, location) =>
          val resDef : mutable.Map[Identifier, DefinitionExpression] = mutable.Map()
          val newRes = flattenExpressionArg(result, resDef)
          val newBody = flattenDefinitions(body ++ resDef)
          FunctionExpression(typeParams, params, newBody, newRes, location)
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(flattenExpressionArg(applicable, addDefs), args.map(flattenExpressionArg(_, addDefs)), location)
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(flattenExpressionArg(applicable, addDefs), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          RecordConstructorExpression(entries.map{case (n, (e, loc)) => (n, (flattenExpressionArg(e, addDefs), loc))}, location)
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, flattenExpressionArg(target, addDefs), nameLocation, location)
        case _ => e
      }
    }

    def flattenDefinitions(defs: Map[Identifier, DefinitionExpression]) : Map[Identifier, DefinitionExpression] = {
      val addDefs : mutable.Map[Identifier, DefinitionExpression] = mutable.Map()
      defs.map{case (id, exp) =>
        (id, flattenExpression(exp, addDefs))
      } ++ addDefs.toMap
    }

    Success(Core.Specification(spec.in, flattenDefinitions(spec.definitions), spec.out, maxId), Seq())
  }

}

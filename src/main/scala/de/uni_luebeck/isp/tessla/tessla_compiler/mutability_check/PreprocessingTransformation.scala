package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import cats.data.Ior
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.{Location, TranslationPhase}


object PreprocessingTransformation extends TranslationPhase[Specification, Specification] {

  override def translate(tc: Specification): TranslationPhase.Result[Specification] = {

    var idNum = tc.maxIdentifier

    def translateMap(defs: Map[Identifier, DefinitionExpression]): Map[Identifier, DefinitionExpression] = {
      defs.map{case (id, exp) =>(id, translateDefinitionExpression(exp))}
    }

    def translateExp(exp: ExpressionArg) : ExpressionArg = {
      exp match {
        case e: Expression => translateDefinitionExpression(e)
        case ExpressionRef(id, tpe, location) => ExpressionRef(id, tpe, location)
      }
    }

    def translateDefinitionExpression(exp: DefinitionExpression) : DefinitionExpression = {
      exp match {
        case FunctionExpression(typeParams, params, body, result, location) => {

          val (newBody, newResult) = if (!result.isInstanceOf[ExpressionRef]) {
            idNum += 1
            val newID = Identifier(Ior.right(idNum))
            (body + (newID -> result.asInstanceOf[Expression]), ExpressionRef(newID, result.tpe, Location.unknown))
          } else {
            (body, result)
          }

          FunctionExpression(typeParams, params, translateMap(newBody), newResult, location)
        }
        case ApplicationExpression(applicable, args, location) => ApplicationExpression(translateExp(applicable), args.map(translateExp), location)
        case TypeApplicationExpression(applicable, typeArgs, location) => TypeApplicationExpression(translateExp(applicable), typeArgs, location)
        case RecordConstructorExpression(entries, location) => RecordConstructorExpression(entries.view.mapValues{case (e, l) => (translateExp(e), l)}.toMap, location)
        case RecordAccessorExpression(name, target, nameLocation, location) => RecordAccessorExpression(name, translateExp(target), nameLocation, location)
        case e => e
      }
    }

    Success(Specification(tc.in, translateMap(tc.definitions), tc.out, idNum), Seq())
  }


}
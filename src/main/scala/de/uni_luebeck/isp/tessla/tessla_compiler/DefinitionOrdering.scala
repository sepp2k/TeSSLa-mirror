package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core._

import scala.collection.mutable

object DefinitionOrdering {

  def order(input: Map[Identifier, DefinitionExpression]) : Seq[(Identifier, DefinitionExpression)] = {

    val ordered : mutable.ArrayBuffer[(Identifier, DefinitionExpression)] = mutable.ArrayBuffer()

    def calcExpressionDependencies(exp: ExpressionArg, ignore: Set[Identifier]) : Unit = {
      exp match {
        case ApplicationExpression(TypeApplicationExpression(ExternExpression(_,_,_,"last",_), _, _), args, _) => calcExpressionDependencies(args(1), ignore)
        case ApplicationExpression(TypeApplicationExpression(ExternExpression(_,_,_,"delay",_), _, _), args, _) => calcExpressionDependencies(args(1), ignore)
        case ApplicationExpression(app, args, _) => args.appended(app).foreach(exp => calcExpressionDependencies(exp, ignore))
        case ExpressionRef(id, _, _) if input.contains(id) => calcMissingDependencies(id, input(id), ignore)
        case TypeApplicationExpression(e, _, _) => calcExpressionDependencies(e, ignore)
        case RecordConstructorExpression(entries, _) => entries.foreach{case (_,(exp,_)) => calcExpressionDependencies(exp, ignore)}
        case RecordAccessorExpression(_, target, _, _) => calcExpressionDependencies(target, ignore)
        case _ => ()
      }
    }

    def calcMissingDependencies(id: Identifier, dExp: DefinitionExpression, ignore: Set[Identifier]) : Unit = {
      if (!ignore.contains(id) && !ordered.contains((id, dExp))) {
        calcExpressionDependencies(dExp, ignore + id)
        ordered += ((id, dExp))
      }
    }

    input.foreach{case (i, d) => calcMissingDependencies(i, d, Set())}

    ordered.toSeq
  }

}

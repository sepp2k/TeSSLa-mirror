package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core._

import scala.collection.mutable

object DefinitionOrdering {

  def order(input: Map[Identifier, DefinitionExpression]) : Seq[(Identifier, DefinitionExpression)] = {

    val ordered : mutable.ArrayBuffer[(Identifier, DefinitionExpression)] = mutable.ArrayBuffer()

    def calcExpressionDependencies(exp: ExpressionArg) : Unit = {
      exp match {
        case ApplicationExpression(TypeApplicationExpression(ExternExpression(_,_,_,"last",_), _, _), args, _) => calcExpressionDependencies(args(1))
        case ApplicationExpression(TypeApplicationExpression(ExternExpression(_,_,_,"delay",_), _, _), args, _) => calcExpressionDependencies(args(1))
        case ApplicationExpression(app, args, _) => args.appended(app).foreach(exp => calcExpressionDependencies(exp))
        case ExpressionRef(id, _, _) if input.contains(id) => calcMissingDependencies(id, input(id))
        case TypeApplicationExpression(e, _, _) => calcExpressionDependencies(e)
        case RecordConstructorExpression(entries, _) => entries.foreach(exp => calcExpressionDependencies(exp._2))
        case RecordAccesorExpression(_, target, _) => calcExpressionDependencies(target)
        case _ => ()
      }
    }

    def calcMissingDependencies(id: Identifier, dExp: DefinitionExpression) : Unit = {
      if (!ordered.contains((id, dExp))) {
        calcExpressionDependencies(dExp)
        ordered += ((id, dExp))
      }
    }

    input.foreach{case (i, d) => calcMissingDependencies(i, d)}

    ordered.toSeq
  }

}

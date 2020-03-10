package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core._

import scala.collection.mutable

object DefinitionOrdering {

  def order(input: Map[Identifier, DefinitionExpression], addOrderingConstraints: Map[Identifier, Set[Identifier]]) : Seq[(Identifier, DefinitionExpression)] = {

    val ordered : mutable.ArrayBuffer[(Identifier, DefinitionExpression)] = mutable.ArrayBuffer()

    def calcExpressionDependencies(exp: ExpressionArg) : Unit = {
      exp match {
        case ApplicationExpression(TypeApplicationExpression(ExternExpression(_,_,_,"last",_), _, _), args, _) => calcExpressionDependencies(args(1))
        case ApplicationExpression(TypeApplicationExpression(ExternExpression(_,_,_,"delay",_), _, _), args, _) => calcExpressionDependencies(args(1))
        case ApplicationExpression(app, args, _) => args.appended(app).foreach(exp => calcExpressionDependencies(exp))
        case ExpressionRef(id, _, _) if input.contains(id) => calcMissingDependencies(id, input(id))
        case TypeApplicationExpression(e, _, _) => calcExpressionDependencies(e)
        case RecordConstructorExpression(entries, _) => entries.foreach{case (_,(exp,_)) => calcExpressionDependencies(exp)}
        case RecordAccessorExpression(_, target, _, _) => calcExpressionDependencies(target)
        case _ => ()
      }
    }

    def calcMissingDependencies(id: Identifier, dExp: DefinitionExpression) : Unit = {
      addOrderingConstraints.getOrElse(id, Set()).foreach{case prev =>
        if (input.contains(prev)) calcMissingDependencies(prev, input(prev))
      }
      if (!ordered.contains((id, dExp))) {
        calcExpressionDependencies(dExp)
        ordered += ((id, dExp))
      }
    }

    input.foreach{case (i, d) => calcMissingDependencies(i, d)}

    ordered.toSeq
  }

}

package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.TesslaAST
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.tessla_compiler.Errors

final case class Activation(base: Set[Identifier], init: Set[Identifier])

class ImplicationChecker(spec: TesslaAST.Core.Specification) {

  val activationMap : collection.mutable.Map[Identifier, (Set[Activation], Boolean)] = collection.mutable.Map()
  val knownImplications : collection.mutable.Map[(Identifier, Identifier, Set[Identifier], Boolean), Boolean] = collection.mutable.Map()

  (spec.definitions.keys ++ spec.in.keys).toSet.foreach{id : Identifier =>
    activationMap += (id -> processStreamDef(id, Set()))
  }

    def processStreamDef(id: Identifier, stack: Set[Identifier]) : (Set[Activation], Boolean) = { //Activates, Always Init
      if (stack(id)) {
        (Set(), false)
      }
      else if (activationMap.contains(id)) {
        activationMap(id)
      }
      else if (spec.in.contains(id)) {
        (Set(Activation(Set(id), Set())), false)
      } else {
        val defExpr = spec.definitions(id)
        defExpr.tpe match {
          case InstantiatedType("Events", _, _) => defExpr match {
            case ApplicationExpression(TypeApplicationExpression(e: ExternExpression, _, _), args, _) => processStreamDefExp(id, e, args, stack + id)
            case ApplicationExpression(e: ExternExpression, args, _) => processStreamDefExp(id, e, args, stack + id)
            case e => throw Errors.CoreASTError("Non valid stream defining expression cannot be translated", e.location)
          }
          case _ => (Set(), false)
        }
      }
    }

    def processStreamDefExp(id: Identifier, defExp: ExternExpression, args: Seq[ExpressionArg], stack: Set[Identifier]) : (Set[Activation], Boolean) = { //Activates, Always Init
      defExp.name match {

        case "defaultFrom" => (Set(Activation(Set(id), Set())), false) //TODO: Two colorings???

        case "lift" => (Set(Activation(Set(id), Set())), false) //TODO: Only-Value-Lifts --> Tree climbing

        case "nil" => (Set(), false)

        case "time" => processStreamDef(ExpressionFlowAnalysis.getExpArgID(args(0)), stack)

        case "default" => (processStreamDef(ExpressionFlowAnalysis.getExpArgID(args(0)), stack)._1, true)

        case "delay" => (Set(Activation(Set(id), Set(ExpressionFlowAnalysis.getExpArgID(args(0))))), false)

        case "last" =>
          val v = ExpressionFlowAnalysis.getExpArgID(args(0))
          val t = ExpressionFlowAnalysis.getExpArgID(args(1))
          val colv = processStreamDef(v, stack)
          val colt = processStreamDef(t, stack)
          if (colv._2) {
            (colt._1, false)
          } else {
            (addCond(colt._1, v), false)
          }

        case "slift" => //TODO: Here lies NP-completeness
          val argIDs = args.dropRight(1).map(ExpressionFlowAnalysis.getExpArgID)
          val argIDCols = argIDs.map(i => (i -> processStreamDef(i, stack))).toMap
          val cols =
            (1 to argIDs.size).flatMap(i => argIDs.combinations(i)).flatMap{ids =>
              val combColor = ids.map(argIDCols(_)._1).reduce[Set[Activation]]{case (c,n) => c.flatMap(cc => n.map(nc => Activation(cc.base ++ nc.base, cc.init ++ nc.init)))}
              combColor.map{col => Activation(col.base, col.init ++ (argIDs.toSet -- ids))}
            }
          (cols.toSet, argIDCols.values.forall(_._2))

        case "merge" =>
          val argIDs = args.map(ExpressionFlowAnalysis.getExpArgID)
          argIDs.map(processStreamDef(_, stack)).reduce[(Set[Activation], Boolean)]{case ((s, b),(s2, b2)) => (s ++ s2, b || b2)}

        case _ => throw Errors.CommandNotSupportedError(defExp.toString)
      }
    }

    def addCond(cols: Set[Activation], id: Identifier) : Set[Activation] = {
      cols.map{case Activation(base, init) => Activation(base, init + id)}
    }

  def freqImplication(i: Identifier, j: Identifier, notInit: Set[Identifier] = Set(), jInit: Boolean = false) : Boolean = {
    if (i == j) {
      true
    } else {
        if (!knownImplications.contains((i, j, notInit, jInit))) {
          val coli = activationMap(i)
          val colj = activationMap(j)

          val res = coli._1.forall { mandatoryColor =>
            if (mandatoryColor.init.intersect(notInit).nonEmpty || (jInit && mandatoryColor.init.exists(c => freqImplication(c, j, notInit + c, true)))) {
              true
            } else {
              colj._1.exists { possColor =>
                if (possColor.base.subsetOf(mandatoryColor.base)) {
                  possColor.init.forall(possImp => mandatoryColor.init.exists(mandImp => freqImplication(mandImp, possImp, notInit + possImp, true)))
                } else {
                  false
                }
              }
            }
          }

          knownImplications += ((i, j, notInit, jInit) -> res)
        }
      knownImplications((i, j, notInit, jInit))
    }
  }

}

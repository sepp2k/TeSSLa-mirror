package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.{Location, TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase.Success

import scala.collection.immutable.ArraySeq

object ASTTransformation extends TranslationPhase[TesslaCoreWithMutabilityInfo, TesslaCoreWithMutabilityInfo] {

  //TODO: May be extended
  //TODO: Record handling

  @scala.annotation.tailrec
  def isInlineExpression(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]) : Boolean = {
    e match {
      case ApplicationExpression(TypeApplicationExpression(ExternExpression(_, _, _, name, _), _, _), _, _) =>
        Set("Set_empty", "Map_empty", "List_empty").contains(name)
      case ExpressionRef(id, _, _) =>
        scope.contains(id) /*otherwiese inputs*/ && isInlineExpression(scope(id), scope)
      case _ =>
        false
    }
  }

  def getInlineExpression(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]) : ExpressionArg = {
    if (isInlineExpression(e, scope)) {
      @scala.annotation.tailrec
      def getExp(e: ExpressionArg) : ExpressionArg = {
        e match {
          case _: Expression => e
          case ExpressionRef(id, _, _) => getExp(scope(id))
        }
      }
      getExp(e)
    } else {
      e
    }
  }

  def inlineMutableConstructors(defs: Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]) : Map[Identifier, DefinitionExpression] = {

    def recInlineExpression(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]) : ExpressionArg = {
      e match {
        case e: Expression => recInlineDefExpr(e, scope)
        case _: ExpressionRef => {
          getInlineExpression(e, scope)
        }
      }
    }

    def recInlineDefExpr(e: DefinitionExpression, scope: Map[Identifier, DefinitionExpression]) : DefinitionExpression = {
      e match {
        case FunctionExpression(typeParams, params, body, result, location) =>
          val newScope = scope ++ body
          FunctionExpression(typeParams, params, inlineMutableConstructors(body, newScope), recInlineExpression(result, newScope), location)
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(recInlineExpression(applicable, scope), args.map(recInlineExpression(_, scope)), location)
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(recInlineExpression(applicable, scope), typeArgs, location)
        case _ => e
      }
    }

    defs.view.mapValues(recInlineDefExpr(_, scope)).toMap
  }

  override def translate(tcMut: TesslaCoreWithMutabilityInfo): TranslationPhase.Result[TesslaCoreWithMutabilityInfo] = {

    val spec = tcMut.spec
    val idTypes = tcMut.idTypes

    //TODO: Inlining of mutable vars

    def barkEvents(t: Type) : Type = {
      t match {
        case InstantiatedType("Events", Seq(t), _) => t
        case _ => t
      }
    }

    def typeArgInference(fType: FunctionType, argTypes: Seq[Type]): List[Type] = {
      val types: collection.mutable.Map[Identifier, Type] = collection.mutable.Map()

      def calcTypeParams(t1: Type, t2: Type) : Unit = {
        //TODO: Better solution
        val t1n = barkEvents(t1)
        val t2n = barkEvents(t2)

        t1n match {
          case FunctionType(_, paramTypes, resultType, _) =>
            paramTypes.zip(t2n.asInstanceOf[FunctionType].paramTypes).foreach { case (e1, e2) => calcTypeParams(e1._2, e2._2) }
            calcTypeParams(resultType, t2n.asInstanceOf[FunctionType].resultType)
          case InstantiatedType(name, typeArgs, _) if Set(name, s"Mut$name").contains(t2n.asInstanceOf[InstantiatedType].name) =>
            typeArgs.zip(t2n.asInstanceOf[InstantiatedType].typeArgs).foreach { case (e1, e2) => calcTypeParams(e1, e2) }
          case RecordType(entries, _) =>
            entries.zip(t2n.asInstanceOf[RecordType].entries).foreach { case (e1, e2) => calcTypeParams(e1._2._1, e2._2._1) }
          case TypeParam(id, _) =>
            types += (id -> t2n)
        }
      }

      fType.paramTypes.zip(argTypes).foreach{case ((_, t1), t2) => calcTypeParams(t1, t2)}

      fType.typeParams.map(types(_))
    }

    def getApplicable(app: ExpressionArg, args: ArraySeq[ExpressionArg], scope: Map[Identifier, DefinitionExpression], resType: Type, loc: Location = Location.unknown): (ExpressionArg, ArraySeq[ExpressionArg]) = {

      def newArgsAndTypes: (ArraySeq[TesslaAST.Core.ExpressionArg], ArraySeq[TesslaAST.Core.Type]) = app match {
        case ExternExpression(_, _, _, "lift", _)
             | ExternExpression(_, _, _, "slift", _) =>
          val lArg = getApplicable(args.last, args.dropRight(1), scope, resType)

          (lArg._2 :+ lArg._1, lArg._2.map(ExpressionFlowAnalysis.getExpArgID).map(idTypes(_, scope)) :+ lArg._1.tpe)
        case _ =>
          (args, args.map(ExpressionFlowAnalysis.getExpArgID).map(idTypes(_, scope)))
      }

      app match {
        case TypeApplicationExpression(applicable, _, location) =>
          getApplicable(applicable, args, scope, resType, location)
        case ExternExpression(_, params, _, name, location) =>
          //TypeApplication is erased here since types are fully known
          val newExtExp = ExternExpression(List(), newArgsAndTypes._2.zip(params).map{ case (typ, (ev, _)) => (ev, typ)}.toList, resType, name, location)
          (TypeApplicationExpression(newExtExp, List(), loc), newArgsAndTypes._1)
        case _ =>
          (TypeApplicationExpression(app, typeArgInference(app.tpe.asInstanceOf[FunctionType], newArgsAndTypes._2), loc), args)
      }
    }

    def transformDefs(defs: Map[Identifier, DefinitionExpression], scope: Map[Identifier, DefinitionExpression]): Map[Identifier, DefinitionExpression] = {
      defs.map{ case (id, exp) =>
        val newExp = exp match {
          case FunctionExpression(typeParams, params, body, result, location) =>
            val resID = ExpressionFlowAnalysis.getExpArgID(result)
            val newRes = ExpressionRef(resID, idTypes(resID, scope ++ body), result.location)
            FunctionExpression(typeParams, params.map{ case (id, ev, _) => (id, ev, idTypes(id, scope))}, transformDefs(body, scope ++ body), newRes, location)
          case ApplicationExpression(app, args, location) =>
            val (newApp, newArgs) = getApplicable(app, args, scope, idTypes(id, scope))
            ApplicationExpression(newApp, newArgs, location)
          case _ =>
            exp
        }
        (id, newExp)
      }
    }

    val in = spec.in.map{case (id, (_, anno)) => (id, (idTypes(id, spec.definitions), anno))}
    val defs = transformDefs(spec.definitions, spec.definitions)

    //TODO: Plain specification sufficient?
    Success(new TesslaCoreWithMutabilityInfo(TesslaAST.Core.Specification(in, inlineMutableConstructors(defs, defs), spec.out, spec.maxIdentifier), tcMut.idTypes, tcMut.addDeps, tcMut.impCheck), Seq())
  }

}

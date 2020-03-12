package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.{Location, TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase.Success
import de.uni_luebeck.isp.tessla.tessla_compiler.Errors

import scala.collection.immutable.ArraySeq

object ASTTransformation extends TranslationPhase[TesslaCoreWithMutabilityInfo, TesslaCoreWithMutabilityInfo] {

  override def translate(tcMut: TesslaCoreWithMutabilityInfo): TranslationPhase.Result[TesslaCoreWithMutabilityInfo] = {

    val spec = tcMut.spec
    val idTypes = tcMut.idTypes

    //TODO: Inlining of mutable vars

    def transformInitiatingExpression(app: ApplicationExpression, desiredType: Type, translatedArgs: ArraySeq[ExpressionArg]) : ApplicationExpression = {
      app match {
        case ApplicationExpression(TypeApplicationExpression(ExternExpression(typePars, pars, _, name, l0), targs, l1), _, l2) =>
          ApplicationExpression(TypeApplicationExpression(ExternExpression(typePars, pars, desiredType, name, l0), targs, l1), translatedArgs, l2)
        case _ => throw Errors.CoreASTError(s"No valid initiating expression $app", app.location)
      }
    }

    def typeArgInference(fType: FunctionType, argTypes: ArraySeq[Type]): List[Type] = {
      val types: collection.mutable.Map[Identifier, Type] = collection.mutable.Map()

      def calcTypeParams(t1: Type, t2: Type) : Unit = {

        t1 match {
          case FunctionType(_, paramTypes, resultType, _) =>
            paramTypes.zip(t2.asInstanceOf[FunctionType].paramTypes).foreach { case (e1, e2) => calcTypeParams(e1._2, e2._2) }
            calcTypeParams(resultType, t2.asInstanceOf[FunctionType].resultType)
          case InstatiatedType(name, typeArgs, _) if Set(name, s"Mut$name").contains(t2.asInstanceOf[InstatiatedType].name) =>
            typeArgs.zip(t2.asInstanceOf[InstatiatedType].typeArgs).foreach { case (e1, e2) => calcTypeParams(e1, e2) }
          case RecordType(entries, _) =>
            entries.zip(t2.asInstanceOf[RecordType].entries).foreach { case (e1, e2) => calcTypeParams(e1._2._1, e2._2._1) }
          case TypeParam(id, _) =>
            types += (id -> t2)
        }
      }

      fType.paramTypes.zip(argTypes).foreach{case ((_, t1), t2) => calcTypeParams(t1, t2)}

      fType.typeParams.map(types(_))
    }

    def transformDefinitionExpression(defExp: DefinitionExpression, scope: Map[Identifier, DefinitionExpression]): DefinitionExpression = {
      defExp match {
            case FunctionExpression(typeParams, params, body, result, location) => FunctionExpression(typeParams, params, body.map{case (id, defExp) => (id, transformDefinitionExpression(defExp, scope ++ body))}, transformExpressionArg(result, scope), location)
            case a: ApplicationExpression => transformApp(a, ArraySeq(), scope)._1.asInstanceOf[ApplicationExpression]
            case RecordConstructorExpression(entries, location) => RecordConstructorExpression(entries.view.mapValues {case (e, l) => (transformExpressionArg(e, scope), l) }.toMap, location)
            case RecordAccesorExpression(name, target, nameLocation, location) => RecordAccesorExpression(name, transformExpressionArg(target, scope), nameLocation, location)
            case e => e
      }
    }

    def barkEvents(t: Type) : Type = {
      t match {
        case InstatiatedType("Events", Seq(t), _) => t
        case _ => ??? //TODO: Error
      }
    }

    def transformApp(applicable: ExpressionArg, args : ArraySeq[ExpressionArg], scope: Map[Identifier, DefinitionExpression],
                     inApp : Boolean = false, slift : Boolean = false) : (ExpressionArg, List[Type], ArraySeq[ExpressionArg]) = {
      val argTypes = if (slift) args.map(a => barkEvents(a.tpe)) else args.map(_.tpe)

      applicable match {
        case ExternExpression(typePars, pars, _, name, location) =>
          val (newArgs, newTypeArgs) =  if (name == "lift" || name == "slift") {
            val (newExp, typeArgs, newArgs) = transformApp(args.last, args.dropRight(1), scope, true, true)
            (newArgs :+ newExp, typeArgs)
          } else {
            (args, typeArgInference(applicable.tpe.asInstanceOf[FunctionType], argTypes))
          }
          val newParamTypes = if (slift) newArgs.map(a => barkEvents(a.tpe)) else newArgs.map(_.tpe)
          val newResType = ExpressionFlowAnalysis.getOutputTypeForExternExpression(None,  applicable.asInstanceOf[ExternExpression], newArgs, idTypes, tcMut.impCheck, scope)
          (ExternExpression(typePars, pars.zip(newParamTypes).map{case ((ev, _), nt) => (ev, nt)}, newResType, name, location), newTypeArgs, newArgs)

        case TypeApplicationExpression(applicable, _, location) => {
          val (exp, tArgs, newArgs) = transformApp(applicable, args, scope,true, slift)
          (TypeApplicationExpression(exp, tArgs, location), List(), newArgs)
        }
        case ApplicationExpression(app, appArgs, _) => {
          val newArgs = appArgs.map(transformExpressionArg(_, scope))
          val targs = applicable.tpe match {
            case _: FunctionType if inApp => typeArgInference(applicable.tpe.asInstanceOf[FunctionType], argTypes)
            case _ => List()
          }
          val (newAppExp, _, finArgs) = transformApp(app, newArgs, scope)
          (ApplicationExpression(newAppExp, finArgs), targs, args)
        }
        case _: FunctionExpression |
             _: ExpressionRef => (transformExpressionArg(applicable, scope), typeArgInference(applicable.tpe.asInstanceOf[FunctionType], argTypes), args)
        case _ => ???
      }
    }

    def transformExpressionArg(ea: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): ExpressionArg = {
      ea match {
        case e: Expression => transformDefinitionExpression(e, scope)
        case ExpressionRef(id, _, loc) => ExpressionRef(id, idTypes(id), loc)
      }
    }

    val in = spec.in.map{case (id, (_, annos)) =>
      (id, (idTypes(id), annos))
    }

    val definitions = spec.definitions.map{case (id, defExp) =>
      defExp match {
        // TODO: Real  detection of this case
        // TODO: Put recognition of expressions creating new Objects to central position.
        // Note: Such expressions are always flattened out at this place and hence cannot occur inside the second case
        case ApplicationExpression(TypeApplicationExpression(_: ExternExpression, _, _), ArraySeq(), _) => (id, transformInitiatingExpression(defExp.asInstanceOf[ApplicationExpression], idTypes(id), ArraySeq()))
        case _ => (id, transformDefinitionExpression(defExp, spec.definitions))
      }
    }

    //TODO: Plain specification sufficient?
    Success(new TesslaCoreWithMutabilityInfo(TesslaAST.Core.Specification(in, definitions, spec.out, spec.maxIdentifier), tcMut.idTypes, tcMut.addDeps, tcMut.impCheck), Seq())
  }

}

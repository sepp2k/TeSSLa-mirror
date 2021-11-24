/*
 * Copyright 2021 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler.mutability_check

import de.uni_luebeck.isp.tessla.core.{Location, TesslaAST, TranslationPhase}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

import scala.collection.immutable.ArraySeq

/**
 * Changes types e.g. Map to MutMap etc. to indicate translation as mutable data structure
 */
object ASTTransformation extends TranslationPhase[TesslaCoreWithMutabilityInfo, TesslaCoreWithMutabilityInfo] {

  @scala.annotation.tailrec
  def isInlineExpression(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): Boolean = {
    e match {
      case ApplicationExpression(TypeApplicationExpression(ExternExpression(name, _, _), _, _), _, _) =>
        Set("Set_empty", "Map_empty", "List_empty", "Queue_empty").contains(name)
      case ExpressionRef(id, _, _) =>
        scope.contains(id) /*otherwiese inputs*/ && isInlineExpression(scope(id), scope)
      case _ =>
        false
    }
  }

  def getInlineExpression(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): ExpressionArg = {
    if (isInlineExpression(e, scope)) {
      @scala.annotation.tailrec
      def getExp(e: ExpressionArg): ExpressionArg = {
        e match {
          case _: Expression           => e
          case ExpressionRef(id, _, _) => getExp(scope(id))
        }
      }
      getExp(e)
    } else {
      e
    }
  }

  def inlineMutableConstructors(
    defs: Map[Identifier, DefinitionExpression],
    scope: Map[Identifier, DefinitionExpression]
  ): Map[Identifier, DefinitionExpression] = {

    def recInlineExpression(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): ExpressionArg = {
      e match {
        case e: Expression => recInlineDefExpr(e, scope)
        case _: ExpressionRef => {
          getInlineExpression(e, scope)
        }
      }
    }

    def recInlineDefExpr(
      e: DefinitionExpression,
      scope: Map[Identifier, DefinitionExpression]
    ): DefinitionExpression = {
      e match {
        case FunctionExpression(typeParams, params, body, result, location) =>
          val newScope = scope ++ body
          FunctionExpression(
            typeParams,
            params,
            inlineMutableConstructors(body, newScope),
            recInlineExpression(result, newScope),
            location
          )
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(
            recInlineExpression(applicable, scope),
            args.map(recInlineExpression(_, scope)),
            location
          )
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

    def barkEvents(t: Type): Type = {
      t match {
        case InstantiatedType("Events", Seq(t), _) => t
        case _                                     => t
      }
    }

    def barkOption(t: Type): Type = {
      t match {
        case InstantiatedType("Option", Seq(t), _) => t
      }
    }

    def typeArgInference(fType: FunctionType, argTypes: Seq[Type], wrappedInOption: Boolean): List[Type] = {
      val types: collection.mutable.Map[Identifier, Type] = collection.mutable.Map()

      def calcTypeParams(t1: Type, t2: Type, wrappedInOption: Boolean = false): Unit = {
        val t1n = if (wrappedInOption) barkOption(barkEvents(t1)) else barkEvents(t1)
        val t2n = barkEvents(t2)

        t1n match {
          case FunctionType(_, paramTypes, resultType, _) =>
            paramTypes.zip(t2n.asInstanceOf[FunctionType].paramTypes).foreach {
              case (e1, e2) => calcTypeParams(e1._2, e2._2)
            }
            calcTypeParams(resultType, t2n.asInstanceOf[FunctionType].resultType)
          case InstantiatedType(name, typeArgs, _)
              if Set(name, s"Mut$name").contains(t2n.asInstanceOf[InstantiatedType].name) =>
            typeArgs.zip(t2n.asInstanceOf[InstantiatedType].typeArgs).foreach {
              case (e1, e2) => calcTypeParams(e1, e2)
            }
          case RecordType(entries, _) =>
            entries.zip(t2n.asInstanceOf[RecordType].entries).foreach {
              case (e1, e2) => calcTypeParams(e1._2._1, e2._2._1)
            }
          case TypeParam(id, _) =>
            types += (id -> t2n)
        }
      }

      fType.paramTypes.zip(argTypes).foreach { case ((_, t1), t2) => calcTypeParams(t1, t2, wrappedInOption) }

      fType.typeParams.map(types(_))
    }

    def actualizeRef(e: ExpressionArg, scope: Map[Identifier, DefinitionExpression]): ExpressionArg = {
      e match {
        case ExpressionRef(id, _, location) =>
          ExpressionRef(id, idTypes(id, scope), location)
        case RecordConstructorExpression(entreis, loc) =>
          RecordConstructorExpression(entreis.map { case (n, (e, l)) => (n, (actualizeRef(e, scope), l)) })
        case RecordAccessorExpression(name, target, nameLoc, loc) =>
          RecordAccessorExpression(name, actualizeRef(target, scope), nameLoc, loc)
        //We're flat. Everything with subexpression actually can't happen
        case _ => e
      }
    }

    def getApplicable(
      app: ExpressionArg,
      args: ArraySeq[ExpressionArg],
      scope: Map[Identifier, DefinitionExpression],
      resType: Type,
      loc: Location = Location.unknown,
      wrappedInOption: Boolean = false
    ): (ExpressionArg, ArraySeq[ExpressionArg]) = {

      def newArgsAndTypes: (ArraySeq[TesslaAST.Core.ExpressionArg], ArraySeq[TesslaAST.Core.Type]) = app match {
        case ExternExpression("lift", _, _) | ExternExpression("slift", _, _) =>
          val lArg = getApplicable(
            args.last,
            args.dropRight(1),
            scope,
            resType,
            Location.unknown,
            app.asInstanceOf[ExternExpression].name == "lift"
          )

          (lArg._2 :+ lArg._1, lArg._2.map(ExpressionFlowAnalysis.getExpArgID).map(idTypes(_, scope)) :+ lArg._1.tpe)
        case _ =>
          (args.map(actualizeRef(_, scope)), args.map(ExpressionFlowAnalysis.getExpArgID).map(idTypes(_, scope)))
      }

      app match {
        case TypeApplicationExpression(applicable, _, location) =>
          getApplicable(applicable, args, scope, resType, location)
        case ExternExpression(name, _, location) =>
          //TypeApplication is erased here since types are fully known
          val newExtExp = ExternExpression(
            name,
            resType,
            location
          )
          (TypeApplicationExpression(newExtExp, List(), loc), newArgsAndTypes._1)
        case _ =>
          (
            TypeApplicationExpression(
              actualizeRef(app, scope),
              typeArgInference(app.tpe.asInstanceOf[FunctionType], newArgsAndTypes._2, wrappedInOption),
              loc
            ),
            newArgsAndTypes._1
          )
      }
    }

    def transformDefs(
      defs: Map[Identifier, DefinitionExpression],
      scope: Map[Identifier, DefinitionExpression]
    ): Map[Identifier, DefinitionExpression] = {
      defs.map {
        case (id, exp) =>
          val newExp = exp match {
            case FunctionExpression(typeParams, params, body, result, location) =>
              val resID = ExpressionFlowAnalysis.getExpArgID(result)
              val newRes = ExpressionRef(resID, idTypes(resID, scope ++ body), result.location)
              FunctionExpression(
                typeParams,
                params.map { case (id, ev, _) => (id, ev, idTypes(id, scope)) },
                transformDefs(body, scope ++ body),
                newRes,
                location
              )
            case ApplicationExpression(app, args, location) =>
              val (newApp, newArgs) = getApplicable(app, args, scope, idTypes(id, scope))
              ApplicationExpression(newApp, newArgs, location)
            case _ =>
              actualizeRef(exp, scope).asInstanceOf[DefinitionExpression]
          }
          (id, newExp)
      }
    }

    val in = spec.spec.in.map { case (id, (_, anno)) => (id, (idTypes(id, spec.spec.definitions), anno)) }
    val defs = transformDefs(spec.spec.definitions, spec.spec.definitions)

    Success(
      tcMut.copy(spec =
        tcMut.spec.copy(spec = tcMut.spec.spec.copy(in = in, definitions = inlineMutableConstructors(defs, defs)))
      ),
      Seq()
    )
  }

}

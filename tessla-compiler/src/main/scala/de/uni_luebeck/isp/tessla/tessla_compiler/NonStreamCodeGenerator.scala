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

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils._

import scala.language.{implicitConversions, postfixOps}

/**
 * Class for the translation of TeSSLa expressions of non-stream type
 */

class NonStreamCodeGenerator(extSpec: ExtendedSpecification) {

  /**
   * Set of identifiers which are assigned with final assignments.
   * If an assignment uses exclusively existing assignments it can also be finally assigned
   * Otherwise (in case of recursion) it must be assigned with a dummy default value first
   */
  private var definedIdentifiers: Map[Identifier, DeclarationType] = Map()

  /**
   * Translates a function application to ImpLan
   * @param e The function expression which is applied
   * @param args The argument expressions of the application
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function application
   */
  def translateFunctionCall(
    e: ExpressionArg,
    args: Seq[ImpLanExpr],
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): ImpLanExpr = {
    e match {
      case TypeApplicationExpression(app, types, _) =>
        translateFunctionCall(app, args, tm.typeApp(types), defContext)
      case ExternExpression(name, typ: Core.FunctionType, _) =>
        val resultType = typ.resultType
        val params = typ.paramTypes
        val newTm = tm.parsKnown(typ.typeParams)
        val ftype = FunctionType(
          params.map { case (_, t) => IntermediateCodeUtils.typeConversion(t.resolve(newTm.resMap)) },
          IntermediateCodeUtils.typeConversion(resultType.resolve(newTm.resMap))
        )
        FunctionCall(s"__${name}__", args, ftype)
      case e: ExternExpression => translateExtern(e, tm, defContext)
      case _: FunctionExpression | _: ExpressionRef | _: ApplicationExpression | _: RecordAccessorExpression =>
        LambdaApplication(translateExpressionArg(e, tm, defContext), args)
      case e =>
        throw Diagnostics.CoreASTError("Function call to expression of wrong type cannot be translated", e.location)
    }
  }

  /**
   * Translates an assignment from TeSSLa to ImpLan
   * @param id The id which is assigned
   * @param e The expression assigned to id
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated assignment
   */
  def translateAssignment(
    id: Identifier,
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): ImpLanStmt = {
    val resTpe = e.tpe.resolve(tm.resMap)
    val lazyVar = extSpec.lazyVars.get.contains(id)
    if (lazyVar || finalAssignmentPossible(e)) {
      definedIdentifiers += (id -> (if (lazyVar) FinalLazyDeclaration else FinalDeclaration))
      val inlinedExp = if (lazyVar) inlineVars(e, defContext) else e
      FinalAssignment(s"var_$id", Some(translateExpressionArg(inlinedExp, tm, defContext)), resTpe, lazyVar)
    } else {
      definedIdentifiers += (id -> VariableDeclaration)
      Assignment(
        s"var_$id",
        translateExpressionArg(e, tm, defContext),
        defaultValueForType(resTpe),
        resTpe,
        false
      )
    }
  }

  /**
   * Translates a TeSSLa Core FunctionExpression to an ImpLan [[IntermediateCode.LambdaExpression]]
   * @param e The function to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function expression
   */
  def translateFunction(
    e: FunctionExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): ImpLanExpr = {
    definedIdentifiers ++= e.params.map(_._1 -> FinalDeclaration)
    val newTm = tm.parsKnown(e.typeParams)
    LambdaExpression(
      e.params.map { case (id, _, _) => if (id.fullName == "_") "_" else s"var_$id" },
      e.params.map {
        case (_, StrictEvaluation, tpe) => tpe.resolve(newTm.resMap)
        case (_, LazyEvaluation, tpe)   => LazyContainer(tpe.resolve(newTm.resMap))
      },
      e.result.tpe.resolve(newTm.resMap),
      translateBody(e.body, e.result, newTm, defContext)
    )
  }

  /**
   * Translates a block of statements with return expression (i.e. the body of a lambda) to [[IntermediateCode.ImpLanStmt]]s
   * @param body The sequence of statements to be translated
   * @param ret The return expression of the block
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated block. Contains a [[IntermediateCode.ReturnStatement]] at the end
   */
  def translateBody(
    body: Map[Identifier, DefinitionExpression],
    ret: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): Seq[ImpLanStmt] = {
    val newDefContext = defContext ++ body

    val translatedBody = DefinitionOrdering.order(body).foldLeft[Seq[ImpLanStmt]](Seq()) {
      case (curr, (id, exp)) => curr :+ translateAssignment(id, exp, tm, newDefContext)
    }
    translatedBody :+ ReturnStatement(translateExpressionArg(ret, tm, newDefContext))
  }

  /**
   *
   * @param e          Translates an ExternExpression. If the extern is of function type a lambda expression is wrapped around it.
   *                   If the extern is directly applied this lambda is most likely unnecessary and this function should not be
   *                   used for translation of the called extern.
   * @param tm         The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated expression
   */
  def translateExtern(
    e: ExternExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): ImpLanExpr = {
    e match {
      case ExternExpression("true", _, _) =>
        BoolValue(true)
      case ExternExpression("false", _, _) =>
        BoolValue(false)
      case ExternExpression("None", InstantiatedType("Option", Seq(t), _), _) =>
        NoneValue(t.resolve(tm.resMap))
      case ExternExpression(_, typ: Core.FunctionType, _) =>
        val newTm = tm.parsKnown(typ.typeParams)
        val argNames = typ.paramTypes.indices.map(i => s"tLPar_$i")
        val argTypes = typ.paramTypes
          .map(_._2.resolve(newTm.resMap))
          .map(IntermediateCodeUtils.typeConversion)
          .zip(typ.paramTypes.map(_._1))
          .map {
            case (t, StrictEvaluation) => t
            case (t, LazyEvaluation)   => LazyContainer(t)
          }
        val ret = translateFunctionCall(e, argNames.map(Variable), newTm, defContext)
        LambdaExpression(argNames, argTypes, typ.resultType.resolve(newTm.resMap), Seq(ReturnStatement(ret)))
    }
  }

  /**
   * Translates an ExpressionArg to a corresponding [[IntermediateCode.ImpLanExpr]]
   * @param e The expression to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated expression
   */
  def translateExpressionArg(
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): ImpLanExpr = {
    e match {
      case TypeApplicationExpression(e, tArgs, _) =>
        translateExpressionArg(e, tm.typeApp(tArgs), defContext)
      case f: FunctionExpression =>
        translateFunction(f, tm, defContext)
      case ApplicationExpression(e, args, _) =>
        translateFunctionCall(
          e,
          getInlinedArgs(args, defContext).map(translateExpressionArg(_, tm, defContext)),
          tm,
          defContext
        )
      case StringLiteralExpression(value, _) =>
        StringValue(value)
      case IntLiteralExpression(value, _) =>
        LongValue(value.toLong)
      case FloatLiteralExpression(value, _) =>
        DoubleValue(value)
      case ExpressionRef(id, _, _) =>
        s"var_${id.fullName}"
      case x: ExternExpression =>
        translateExtern(x, tm, defContext)
      case RecordConstructorExpression(entries, _) if entries.isEmpty =>
        UnitValue
      case RecordConstructorExpression(entries, _) =>
        MkStruct(entries.toSeq.map { case (n, (ea, _)) => (n.name, translateExpressionArg(ea, tm, defContext)) }, e.tpe)
      case RecordAccessorExpression(name, target, _, _) =>
        GetStruct(translateExpressionArg(target, tm, defContext), name.name, target.tpe)
      case _ =>
        throw Diagnostics.CoreASTError("Unexpected ExpressionArg cannot be translated", e.location)
    }
  }

  /**
   * Inlines all references in a function application's parameters according to inlining information from [[ExtendedSpecification]]
   * @param args The arguments applied to e
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The modified parameter expressions
   */
  private def getInlinedArgs(
    args: Seq[ExpressionArg],
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): Seq[ExpressionArg] = {
    args.map(e => inlineVars(e, defContext))
  }

  /**
   * Inlines all [[ExpressionRef]] (variable references) to variables in the inlining set of the [[ExtendedSpecification]] in an
   * [[ExpressionArg]] except those of function type
   * @param e The expression where variable references should be inlined
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The modified expression
   */
  private def inlineVars(e: ExpressionArg, defContext: Map[Identifier, DefinitionExpression]): ExpressionArg = {
    e match {
      case e: Expression => inlineVars(e, defContext)
      case ExpressionRef(id, tpe, _)
          if defContext.contains(id) && !tpe.isInstanceOf[Core.FunctionType] && extSpec.inlining.get.contains(id) =>
        inlineVars(defContext(id), defContext)
      case _ => e
    }
  }

  /**
   * Inlines all [[ExpressionRef]] (variable references) to variables in the inlining set of the [[ExtendedSpecification]] in a
   * [[DefinitionExpression]] except those of function type
   * @param e The expression where variable references should be inlined
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The modified expression
   */
  private def inlineVars(
    e: DefinitionExpression,
    defContext: Map[Identifier, DefinitionExpression]
  ): DefinitionExpression = {
    e match {
      case e: Expression =>
        e match {
          case FunctionExpression(typeParams, params, body, result, location) =>
            FunctionExpression(
              typeParams,
              params,
              inlineVarsBody(body, defContext),
              inlineVars(result, defContext),
              location
            )
          case ApplicationExpression(applicable, args, location) =>
            ApplicationExpression(
              inlineVars(applicable, defContext),
              args.map { a => inlineVars(a, defContext) },
              location
            )
          case TypeApplicationExpression(applicable, typeArgs, location) =>
            TypeApplicationExpression(inlineVars(applicable, defContext), typeArgs, location)
          case RecordConstructorExpression(entries, location) =>
            RecordConstructorExpression(
              entries.map { case (n, (e, l)) => (n, (inlineVars(e, defContext), l)) },
              location
            )
          case RecordAccessorExpression(name, target, nameLoc, location) =>
            RecordAccessorExpression(name, inlineVars(target, defContext), nameLoc, location)
          case _ => e
        }
      case _ => e
    }
  }

  /**
   * Checks whether a given expression only uses identifiers from [[definedIdentifiers]], i.e. those already defined
   * @param e Expression to be examined
   * @return  Whether all subexpressions are already defined and and the expression can hence be evaluated
   */
  private def finalAssignmentPossible(e: ExpressionArg): Boolean = {
    //Boolean parameter indicates whether variable already has to be evaluateable
    def getSubIDs(e: ExpressionArg, ignore: Set[Identifier] = Set()): Set[(Identifier, Boolean)] = e match {
      case FunctionExpression(_, params, body, result, _) =>
        (body.values.toSeq :+ result)
          .map(getSubIDs(_, ignore ++ params.map(_._1) ++ body.keys))
          .reduce(_ ++ _)
          .map(e => (e._1, false))
      case ApplicationExpression(applicable, args, _) =>
        (args :+ applicable).map(getSubIDs(_, ignore)).reduce(_ ++ _)
      case TypeApplicationExpression(applicable, _, _) => getSubIDs(applicable, ignore)
      case RecordConstructorExpression(entries, _) =>
        entries.map(e => getSubIDs(e._2._1, ignore)).fold(Set()) { case (x, y) => x ++ y }
      case RecordAccessorExpression(_, target, _, _)       => getSubIDs(target, ignore)
      case ExpressionRef(id, _, _) if !ignore.contains(id) => Set((id, true))
      case _                                               => Set()
    }

    getSubIDs(e).forall(d =>
      definedIdentifiers.contains(d._1) && (!d._2 || definedIdentifiers(d._1) != VariableDeclaration)
    )
  }

  /**
   * Applies [[inlineVars]] on all statements in body
   * @param body Sequence of statements where [[inlineVars]] is applied on
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The modified body
   */
  private def inlineVarsBody(
    body: Map[Identifier, DefinitionExpression],
    defContext: Map[Identifier, DefinitionExpression]
  ): Map[Identifier, DefinitionExpression] = {
    body.map { case (id, de) => (id, inlineVars(de, defContext)) }
  }

  /**
   * Class for managing type arguments
   * @param resMap Map representing depiction from type arg names to types
   * @param unappliedArgs Types where type application has already been processed but not the expression
   *                      where the types are applied to
   */
  case class TypeArgManagement(resMap: Map[Identifier, Type], unappliedArgs: Seq[Type]) {

    /**
     * Process a type application. The new types are stored in [[unappliedArgs]] until the
     * expression they are applied on is hit. If [[unappliedArgs]] already contains types they
     * are overwritten.
     * @param types The types which are applied to the subexpression
     * @return An updated [[TypeArgManagement]]
     */
    def typeApp(types: Seq[Type]): TypeArgManagement = {
      TypeArgManagement(resMap, types)
    }

    /**
     * Processes the sub-expression after a type application and adds the type arg -> type relation to the
     * [[resMap]]. [[unappliedArgs]] is cleared. The types are connected to the arguments in the order
     * they appeared in the type application.
     * @param pars The type params of the sub-expression. If number does not match previous [[typeApp]] call
     *             as much type args as possible are connected to types.
     * @return An updated [[TypeArgManagement]]
     */
    def parsKnown(pars: Seq[Identifier]): TypeArgManagement = {
      TypeArgManagement(resMap.removedAll(pars) ++ pars.zip(unappliedArgs).toMap, Seq())
    }
  }

  object TypeArgManagement {

    /**
     * Produces a fresh [[TypeArgManagement]] object
     * @return Empty[[TypeArgManagement]]
     */
    def empty: TypeArgManagement = TypeArgManagement(Map(), Seq())
  }

}

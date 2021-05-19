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

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}

/**
 * Class implementing [[TranslationPhase]] for the reduction of a TeSSLa Core AST.
 * During this phase all identifiers which have equivalent expressions assigned are reduced to a single one and
 * all their references are replaced.
 */
object ConstantRecycler extends TranslationPhase[Specification, Specification] {

  /**
   * Function triggering the reduction of a TeSSLa Core AST
   *
   * @param spec The TeSSLa Core AST to be reduced
   * @return A reduced but equivalent TeSSLa Core AST
   */
  override def translate(spec: Specification): Result[Specification] = {

    /**
     * This function checks if two expressions are exactly equivalent.
     * The procedure is recursive. If one of the expressions is/contains an [[ExpressionRef]] the expression that is
     * bind to this reference is used for the equivalence check. Therfore it is necessary to pass the scopes of e1, e2
     * to this function.
     * If identifiers are not contained in a scope map they must be parameters of a surrounding function expression.
     *
     * When function expressions are checked for equivalance their result expression is compared. Therfore paramMapping
     * stores pairs of corresponding parameters from surrounding function expressions.
     *
     * @param e1 First expression to be compared
     * @param e2 Second expression to be compared
     * @param scope1 Scope of the first expression (Identifier->Expression map containing all identifiers and belonging
     *               expressions that can be referenced from e1)
     * @param scope2 Scope of the second expression (Identifier->Expression map containing all identifiers and belonging
     *               expressions that can be referenced from e2)
     * @param paramMapping Mapping indicating which parameters that can be referenced from e1 belong to which parameters
     *                     that can be referenced from e2
     * @param stack Stack containing all ids in scope1 for which this function was called recursively to avoid infinite
     *              recursion. Should be empty if function is initially called from outside.
     * @return Boolean indicating whether e1 and e2 are equivalent
     */
    def checkEquivalence(
      e1: ExpressionArg,
      e2: ExpressionArg,
      scope1: Map[Identifier, Expression],
      scope2: Map[Identifier, Expression],
      paramMapping: Map[Identifier, Identifier] = Map(),
      stack: Set[Identifier] = Set()
    ): Boolean = {
      e1 match {
        case FunctionExpression(typeParams, params, body, result, _) if e2.isInstanceOf[FunctionExpression] =>
          val fe2 = e2.asInstanceOf[FunctionExpression]
          val newParamMapping = paramMapping ++ params.map(_._1).zip(fe2.params.map(_._1)).toMap

          typeParams.size == fe2.typeParams.size &&
          params.size == fe2.params.size &&
          params.zip(fe2.params).forall { case (t1, t2) => t1._2 == t2._2 && checkTypeEquivalence(t1._3, t2._3) } &&
          checkEquivalence(result, fe2.result, scope1 ++ body, scope2 ++ fe2.body, newParamMapping, stack)
        //Bodies do not have to be checked for equivalence. The result is enough.

        case ApplicationExpression(applicable, args, _) if e2.isInstanceOf[ApplicationExpression] =>
          val ae2 = e2.asInstanceOf[ApplicationExpression]

          args.size == ae2.args.size &&
          args.zip(ae2.args).forall(et => checkEquivalence(et._1, et._2, scope1, scope2, paramMapping, stack)) &&
          checkEquivalence(ae2.applicable, applicable, scope1, scope2, paramMapping, stack)

        case TypeApplicationExpression(applicable, typeArgs, _) if e2.isInstanceOf[TypeApplicationExpression] =>
          val ta2 = e2.asInstanceOf[TypeApplicationExpression]

          ta2.typeArgs.size == typeArgs.size &&
          typeArgs.zip(ta2.typeArgs).forall(tt => checkTypeEquivalence(tt._1, tt._2)) &&
          checkEquivalence(applicable, ta2.applicable, scope1, scope2, paramMapping, stack)

        case RecordConstructorExpression(entries, _) if e2.isInstanceOf[RecordConstructorExpression] =>
          entries.forall {
            case (name, (exp, _)) =>
              val e2Entries = e2.asInstanceOf[RecordConstructorExpression].entries
              e2Entries.contains(name) && checkEquivalence(e2Entries(name)._1, exp, scope1, scope2, paramMapping, stack)
          }

        case RecordAccessorExpression(name, target, _, _) if e2.isInstanceOf[RecordAccessorExpression] =>
          val ra2 = e2.asInstanceOf[RecordAccessorExpression]

          ra2.name == name && checkEquivalence(target, ra2.target, scope1, scope2, paramMapping, stack)

        case StringLiteralExpression(value, _) if e2.isInstanceOf[StringLiteralExpression] =>
          value == e2.asInstanceOf[StringLiteralExpression].value
        case IntLiteralExpression(value, _) if e2.isInstanceOf[IntLiteralExpression] =>
          value == e2.asInstanceOf[IntLiteralExpression].value
        case FloatLiteralExpression(value, _) if e2.isInstanceOf[FloatLiteralExpression] =>
          value == e2.asInstanceOf[FloatLiteralExpression].value
        case ExternExpression(name, _, _) if e2.isInstanceOf[ExternExpression] =>
          name == e2.asInstanceOf[ExternExpression].name

        case ExpressionRef(id, _, _) if stack.contains(id) => true
        case ExpressionRef(id, _, _) if e2.isInstanceOf[ExpressionRef] =>
          val er2 = e2.asInstanceOf[ExpressionRef]
          (paramMapping.contains(id) && paramMapping(id) == er2.id) ||
          (!paramMapping.contains(id) && id == er2.id) ||
          (scope1.contains(id) &&
          scope2.contains(er2.id) &&
          checkEquivalence(scope1(id), scope2(er2.id), scope1, scope2, paramMapping, stack + id))

        case _ => false
      }
    }

    /**
     * This function checks if two types are equivalent.
     * There is no check for typeargs performed here. Typeargs are always assumed to be different since their scope is
     * not known.
     *
     * @param t1 First type to be compared
     * @param t2 Second type to be comparec
     * @return Boolean indicating whether t1 and t2 are equivalent
     */
    //TODO: Also check typeargs for equivalence
    def checkTypeEquivalence(t1: Type, t2: Type): Boolean = {
      t1 match {
        case InstantiatedType(name, typeArgs, _) if t2.isInstanceOf[InstantiatedType] =>
          val it2 = t2.asInstanceOf[InstantiatedType]
          name == it2.name &&
          typeArgs.size == it2.typeArgs.size &&
          typeArgs.zip(it2.typeArgs).forall(tt => checkTypeEquivalence(tt._1, tt._2))

        case FunctionType(typeParams, paramTypes, resultType, _) if t2.isInstanceOf[FunctionType] =>
          val ft2 = t2.asInstanceOf[FunctionType]
          paramTypes.size == ft2.paramTypes.size &&
          typeParams.size == ft2.typeParams.size &&
          checkTypeEquivalence(resultType, ft2.resultType) &&
          paramTypes.zip(ft2.paramTypes).forall {
            case (t1, t2) => t1._1 == t2._1 && checkTypeEquivalence(t1._2, t2._2)
          }

        case RecordType(entries, _) if t2.isInstanceOf[RecordType] =>
          val rt2 = t2.asInstanceOf[RecordType]
          entries.size == rt2.entries.size &&
          entries.zip(rt2.entries).forall {
            case (t1, t2) => t1._1 == t2._1 && checkTypeEquivalence(t1._2._1, t2._2._1)
          }

        case _ => false
      }
    }

    /**
     * Returns a map indicating which identifiers in a set of statements can be replaced by other identifiers.
     * One identifier can be replaced by another one if they are bind to equal expressions.
     *
     * @param stmts The set of statements (identifier and assigned expression) to be examined
     * @param scope Scope of the statements (Identifier->Expression map containing all identifiers and belonging
     *               expressions that can be referenced from the expressions in stmts)
     * @return Map of possible replacements. If an identifier cannot be replaced, its belonging key in the map is the
     *         identifier itself. The map contains every identifier which is key in the stmts map.
     */
    def getRecyclePlan(
      stmts: Map[Identifier, Expression],
      scope: Map[Identifier, Expression]
    ): Map[Identifier, Identifier] = {
      val includedStmts: collection.mutable.Set[Identifier] = collection.mutable.Set()

      stmts.foldLeft[Map[Identifier, Identifier]](Map()) {
        case (currentRPlan, (id, exp)) =>
          val equiv = includedStmts.find(i => checkEquivalence(exp, scope(i), scope, scope))
          if (equiv.isDefined) {
            currentRPlan + (id -> equiv.get)
          } else {
            includedStmts += id
            currentRPlan + (id -> id)
          }
      }
    }

    /**
     * Replaces an [[ExpressionArg]] by an equivalent one where all identifiers are replaced by equivalent ones
     * according to the plan which is passed.
     *
     * @param exp The expression to be processed
     * @param plan The replacement plan for identifiers. Every identifier in exp which is a key in this map is replaced
     *             by the value from this map. Can be retrieved from [[getRecyclePlan]].
     * @param scope Scope of the [[ExpressionArg]] (Identifier->Expression map containing all identifiers and belonging
     *               expressions that can be referenced from exp)
     * @return An equivalend [[ExpressionArg]]
     */
    def recycleExpressionArg(
      exp: ExpressionArg,
      plan: Map[Identifier, Identifier],
      scope: Map[Identifier, Expression]
    ): ExpressionArg = {
      exp match {
        case e: Expression                                         => recycleExpression(e, plan, scope)
        case ExpressionRef(id, tpe, location) if plan.contains(id) => ExpressionRef(plan(id), tpe, location)
        case _                                                     => exp
      }
    }

    /**
     * Replaces an [[Expression]] by an equivalent one where all identifiers are replaced by equivalent ones
     * according to the plan which is passed.
     * For functions the bodies further possible replacements(which are not contained in [[plan]]) are searched and
     * performed recursively.
     *
     * @param exp The expression to be processed
     * @param plan The replacement plan for identifiers. Every identifier in exp which is a key in this map is replaced
     *             by the value from this map. Can be retrieved from [[getRecyclePlan]].
     * @param scope Scope of the [[Expression]] (Identifier->Expression map containing all identifiers and belonging
     *               expressions that can be referenced from exp)
     * @return An equivalend [[Expression]]
     */
    def recycleExpression(
      exp: Expression,
      plan: Map[Identifier, Identifier],
      scope: Map[Identifier, Expression]
    ): Expression = {
      exp match {
        case FunctionExpression(typeParams, params, body, result, location) =>
          val rp = plan ++ getRecyclePlan(body, scope ++ body)
          FunctionExpression(
            typeParams,
            params,
            recycle(body, rp, scope ++ body),
            recycleExpressionArg(result, rp, scope ++ body),
            location
          )
        case ApplicationExpression(applicable, args, location) =>
          ApplicationExpression(
            recycleExpressionArg(applicable, plan, scope),
            args.map(recycleExpressionArg(_, plan, scope)),
            location
          )
        case TypeApplicationExpression(applicable, typeArgs, location) =>
          TypeApplicationExpression(recycleExpressionArg(applicable, plan, scope), typeArgs, location)
        case RecordConstructorExpression(entries, location) =>
          RecordConstructorExpression(
            entries.view.mapValues { case (ea, l) => (recycleExpressionArg(ea, plan, scope), l) }.toMap,
            location
          )
        case RecordAccessorExpression(name, target, nameLocation, location) =>
          RecordAccessorExpression(name, recycleExpressionArg(target, plan, scope), nameLocation, location)
        case _ => exp
      }
    }

    /**
     * Replaces all expressions in a list of statements by equivalent ones where all contained identifiers are replaced by
     * equivalent ones according to the plan which is passed.
     *
     * @param stmts The list of statements to be processed
     * @param plan The replacement plan for identifiers. Every identifier in exp which is a key in this map is replaced
     *             by the value from this map. Can be retrieved from [[getRecyclePlan]].
     * @param scope Scope of the expressions from the statement list (Identifier->Expression map containing all
     *              identifiers and belonging expressions that can be referenced from the expressions)
     * @return An equivalend list of statements
     */
    def recycle(
      stmts: Map[Identifier, Expression],
      plan: Map[Identifier, Identifier],
      scope: Map[Identifier, Expression]
    ): Map[Identifier, Expression] = {
      stmts.flatMap {
        case (id, _) if plan(id) != id => None
        case (id, exp)                 => Some((id, recycleExpression(exp, plan, scope)))
      }
    }

    val plan = getRecyclePlan(spec.definitions, spec.definitions)

    val newDef = recycle(spec.definitions, plan, spec.definitions)
    val newOut = spec.out.map {
      case (er, annotations) =>
        (recycleExpressionArg(er, plan, spec.definitions).asInstanceOf[ExpressionRef], annotations)
    }

    Success(spec.copy(definitions = newDef, out = newOut), Seq())

  }
}

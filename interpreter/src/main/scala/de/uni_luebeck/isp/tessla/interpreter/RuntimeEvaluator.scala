/*
 * Copyright 2020 The TeSSLa Community
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

package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.core.ConstantEvaluator.{Record, RuntimeError}
import de.uni_luebeck.isp.tessla.core.Errors.InternalError
import de.uni_luebeck.isp.tessla.core.{ConstantEvaluator, TesslaAST}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core
import de.uni_luebeck.isp.tessla.core.util.Lazy
import de.uni_luebeck.isp.tessla.interpreter.RuntimeEvaluator.Env

import scala.collection.immutable.ArraySeq

object RuntimeEvaluator {

  type Env = Map[String, Lazy[Any]]

}

/**
 * An evaluator for core expressions, given implementations for externs.
 *
  * @param externs definitions of externs, as mapping from their name to their definition
 */

// TODO: All arguments are currently wrapped in Lazy, even if they are strictly evaluated
// TODO: Replace argument lists by tuples?
class RuntimeEvaluator(externs: Map[String, Any]) {

  /**
   * Evaluate an expression argument.
   *
    * In case of a reference, the identifier is resolved using the provided environment,
   * otherwise the expression is evaluated in [[evalExpression]].
   *
    * @param arg the argument to evaluate
   * @param env the environment
   * @return
   */
  def evalExpressionArg(arg: Core.ExpressionArg, env: => Env): Lazy[Any] = arg match {
    case Core.ExpressionRef(id, _, _) =>
      Lazy {
        env(id.fullName).get
      }
    case e: Core.Expression => Lazy(evalExpression(e, env))
  }

  /**
   * Evaluate the expression with the provided environment and externs.
   *
    * @param exp the expression to evaluate
   * @param env the environment
   * @return the resulting value
   */
  def evalExpression(exp: Core.Expression, env: Env): Any = exp match {
    case Core.FunctionExpression(_, params, body, result, location) =>
      (args: ArraySeq[Lazy[Any]]) => {
        if (params.size != args.size) {
          throw InternalError(s"Called with wrong number of arguments.", location)
        }
        lazy val newEnv: Env =
          env ++ params.map(_._1.fullName).zip(args) ++ body.map(e => (e._1.fullName, evalExpressionArg(e._2, newEnv)))
        evalExpressionArg(result, newEnv)
      }
    case e: Core.ExternExpression =>
      externs.get(e.name) match {
        case Some(f) => f
        case None    => throw InternalError(s"Extern ${e.name} not defined.", e.location)
      }
    case Core.ApplicationExpression(applicable, args, location) =>
      val f = evalExpressionArg(applicable, env).get
      val Core.FunctionType(_, paramTypes, _, _) = applicable.tpe
      if (paramTypes.size != args.size) {
        throw InternalError(s"Wrong number of arguments.", location)
      }
      val newArgs = args.map(evalExpressionArg(_, env)).zip(paramTypes).map {
        case (v, (TesslaAST.StrictEvaluation, _)) => {
          v.get
          v // NOTE: All environment entries are currently wrapped in lazy, even if strictly evaluated.
        }
        case (v, (TesslaAST.LazyEvaluation, _)) => v
      }
      propagateInternal(f)(_.asInstanceOf[ArraySeq[Lazy[Any]] => Lazy[Any]](newArgs).get)
    case Core.TypeApplicationExpression(applicable, _, _) =>
      evalExpressionArg(applicable, env).get
    case Core.RecordConstructorExpression(entries, _) =>
      Record(
        entries.map(e => (e._1, evalExpressionArg(e._2._1, env).get))
      ) // TODO: entries are now strictly evaluated, should they be lazy?
    case Core.RecordAccessorExpression(name, target, _, _) =>
      propagateInternal(evalExpressionArg(target, env).get)(_.asInstanceOf[ConstantEvaluator.Record].entries(name))
    case Core.StringLiteralExpression(value, _) => value
    case Core.IntLiteralExpression(value, _)    => value
    case Core.FloatLiteralExpression(value, _)  => value
  }

  private def propagateInternal(arg: Any)(f: Any => Any) =
    if (arg.isInstanceOf[RuntimeError]) arg else f(arg)

}

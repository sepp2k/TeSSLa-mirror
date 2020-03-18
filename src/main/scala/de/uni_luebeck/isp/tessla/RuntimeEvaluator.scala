package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.RuntimeEvaluator.{RuntimeError, _}
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.util.Lazy

import scala.collection.immutable.ArraySeq
import de.uni_luebeck.isp.tessla.RuntimeExterns.Extern

object RuntimeEvaluator {

  type Env = Map[String, Lazy[Any]]

  case class Record(entries: Map[String, Any]) {
    override def toString = TesslaAST.printRecord(entries, " = ", (x: Any) => x.toString, "()")
  }

  case class RuntimeError(msg: String) // TODO: support location information etc


}

// TODO: All arguments are currently wrapped in Lazy, even if they are strictly evaluated
// TODO: Replace argument lists by tuples?
class RuntimeEvaluator(externs: Map[String, Extern[Lazy]]) {

  def evalExpressionArg(arg: Core.ExpressionArg, env: => Env): Lazy[Any] = arg match {
    case Core.ExpressionRef(id, _, _) => Lazy {
      env(id.fullName).get
    }
    case e: Core.Expression => Lazy(evalExpression(e, env))
  }

  def propagateInternal(arg: Any)(f: Any => Any) =
    if (arg.isInstanceOf[RuntimeError]) arg else f(arg)

  def evalExpression(exp: Core.Expression, env: Env): Any = exp match {
    case Core.FunctionExpression(_, params, body, result, location) =>
      (args: ArraySeq[Lazy[Any]]) => {
        if (params.size != args.size) {
          throw InternalError(s"Called with wrong number of arguments.", location)
        }
        lazy val newEnv: Env = env ++ params.map(_._1.fullName).zip(args) ++ body.map(e => (e._1.fullName, evalExpressionArg(e._2, newEnv)))
        evalExpressionArg(result, newEnv)
      }
    case e: Core.ExternExpression =>
      externs.get(e.name) match {
        case Some(f) => f
        case None => throw InternalError(s"Extern ${e.name} not defined.", e.location)
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
      Record(entries.map(e => (e._1, evalExpressionArg(e._2._1, env).get))) // TODO: entries are now strictly evaluated, should they be lazy?
    case Core.RecordAccessorExpression(name, target, _, _) =>
      propagateInternal(evalExpressionArg(target, env).get)(_.asInstanceOf[Record].entries(name))
    case Core.StringLiteralExpression(value, _) => value
    case Core.IntLiteralExpression(value, _) => value
    case Core.FloatLiteralExpression(value, _) => value
  }

}


package de.uni_luebeck.isp.tessla

import java.util.Locale

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.RuntimeEvaluator._
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.util.Lazy
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition

object RuntimeEvaluator {

  type Env = Map[String, Lazy[Any]]

  case class Record(entries: Map[String, Any]) {
    override def toString = {
      val isTuple = entries.keys.forall(_.matches("_\\d+"))
      if (isTuple) {
        val sorted = entries.toList.map(x => (x._1.substring(1).toInt, x._2)).sortBy(_._1).map(_._2)
        s"(${sorted.mkString(", ")})"
      } else {
        val sorted = entries.toList.sortBy(_._1).map(x => x._1 + " = " + x._2)
        s"{${sorted.mkString(", ")}}"
      }
    }
  }

  def strict(f: List[Any] => Any) = (list: List[Lazy[Any]]) => f(list.map(_.get))

  def binIntOp(f: (BigInt, BigInt) => Any) = strict(l =>
    f(l(0).asInstanceOf[BigInt], l(1).asInstanceOf[BigInt]))

  def unaryIntOp(f: BigInt => Any) = strict(l => f(l(0).asInstanceOf[BigInt]))

  def binBoolOp(f: (Boolean, Boolean) => Any) = strict(l => f(l(0).asInstanceOf[Boolean], l(1).asInstanceOf[Boolean]))

  def binFloatOp(f: (Double, Double) => Any) = strict(l => f(l(0).asInstanceOf[Double], l(1).asInstanceOf[Double]))

  def binAnyOp(f: (Any, Any) => Any) = strict(l => f(l(0), l(1)))

  def unaryFloatOp(f: Double => Any) = strict(l => f(l(0).asInstanceOf[Double]))

  val commonExterns: Map[String, List[Lazy[Any]] => Any] = Map(
    "true" -> ((arguments: List[Lazy[Any]]) => true),
    "false" -> ((arguments: List[Lazy[Any]]) => false),
    "add" -> binIntOp(_ + _),
    "sub" -> binIntOp(_ - _),
    "mul" -> binIntOp(_ * _),
    "div" -> binIntOp(_ / _),
    "mod" -> binIntOp(_ % _),
    "fadd" -> binFloatOp(_ + _),
    "fsub" -> binFloatOp(_ - _),
    "fmul" -> binFloatOp(_ * _),
    "fdiv" -> binFloatOp(_ / _),
    "leftshift" -> binIntOp(_ << _.toInt),
    "rightshift" -> binIntOp(_ >> _.toInt),
    "bitand" -> binIntOp(_ & _),
    "bitor" -> binIntOp(_ | _),
    "bitxor" -> binIntOp(_ ^ _),
    "bitflip" -> unaryIntOp(~_),
    "negate" -> unaryIntOp(-_),
    "fnegate" -> unaryFloatOp(-_),
    "eq" -> binAnyOp(_ == _),
    "neq" -> binAnyOp(_ != _),
    "lt" -> binIntOp(_ < _),
    "leq" -> binIntOp(_ <= _),
    "gt" -> binIntOp(_ > _),
    "geq" -> binIntOp(_ >= _),
    "flt" -> binFloatOp(_ < _),
    "fleq" -> binFloatOp(_ <= _),
    "fgt" -> binFloatOp(_ > _),
    "fgeq" -> binFloatOp(_ >= _),
    "and" -> ((arguments: List[Lazy[Any]]) => arguments(0).get.asInstanceOf[Boolean] && arguments(1).get.asInstanceOf[Boolean]),
    "or" -> ((arguments: List[Lazy[Any]]) => arguments(0).get.asInstanceOf[Boolean] || arguments(1).get.asInstanceOf[Boolean]),
    "not" -> strict { arguments => !arguments(0).asInstanceOf[Boolean] },
    "ite" -> ((arguments: List[Lazy[Any]]) => if (arguments(0).get.asInstanceOf[Boolean]) arguments(1).get else arguments(2).get), // TODO: should possibly be strict, but them functions have to use static ite
    "staticite" -> ((arguments: List[Lazy[Any]]) => if (arguments(0).get.asInstanceOf[Boolean]) arguments(1).get else arguments(2).get),
    "pow" -> binFloatOp(math.pow),
    "log" -> binFloatOp((x, base) => math.log(x) / math.log(base)),
    "sin" -> unaryFloatOp(Math.sin),
    "cos" -> unaryFloatOp(Math.cos),
    "tan" -> unaryFloatOp(Math.tan),
    "atan" -> unaryFloatOp(Math.atan),
    "intToFloat" -> unaryIntOp(_.toDouble),
    "floatToInt" -> unaryFloatOp(x => BigDecimal(x).toBigInt()),
    "None" -> ((arguments: List[Any]) => None),
    "Some" -> strict(arguments => Some(arguments(0))),
    "isNone" -> strict(arguments => arguments(0).asInstanceOf[Option[Any]].isEmpty),
    "getSome" -> strict(arguments =>
      arguments(0).asInstanceOf[Option[Any]].get),
    "Map_empty" -> ((arguments: List[Any]) => Map()),
    "Map_add" -> strict(arguments => arguments(0).asInstanceOf[Map[Any, Any]] + (arguments(1) -> arguments(2))),
    "Map_get" -> strict(arguments => arguments(0).asInstanceOf[Map[Any, Any]](arguments(1))),
    "Map_contains" -> strict(arguments => arguments(0).asInstanceOf[Map[Any, Any]].contains(arguments(1))),
    "Map_remove" -> strict(arguments => arguments(0).asInstanceOf[Map[Any, Any]] - arguments(1)),
    "Map_size" -> strict(arguments => BigInt(arguments(0).asInstanceOf[Map[Any, Any]].size)),
    "Map_keys" -> strict(arguments => arguments(0).asInstanceOf[Map[Any, Any]].keys.toList),
    "Map_fold" -> strict(arguments => arguments(0).asInstanceOf[Map[Any, Any]].foldLeft(arguments(1))(
      (x, y) => arguments(2).asInstanceOf[(List[Any] => Any)](List(Lazy(x), Lazy(y._1), Lazy(y._2))))),
    "Set_empty" -> ((arguments: List[Any]) => Set()),
    "Set_add" -> strict(arguments => arguments(0).asInstanceOf[Set[Any]] + arguments(1)),
    "Set_contains" -> strict(arguments => arguments(0).asInstanceOf[Set[Any]].contains(arguments(1))),
    "Set_remove" -> strict(arguments => arguments(0).asInstanceOf[Set[Any]] - arguments(1)),
    "Set_size" -> strict(arguments => BigInt(arguments(0).asInstanceOf[Set[Any]].size)),
    "Set_union" -> strict(arguments => arguments(0).asInstanceOf[Set[Any]] | arguments(1).asInstanceOf[Set[Any]]),
    "Set_intersection" -> strict(arguments => arguments(0).asInstanceOf[Set[Any]] & arguments(1).asInstanceOf[Set[Any]]),
    "Set_minus" -> strict(arguments => arguments(0).asInstanceOf[Set[Any]] -- arguments(1).asInstanceOf[Set[Any]]),
    "Set_fold" -> strict(arguments => arguments(0).asInstanceOf[Set[Any]].fold(arguments(1))(
      (x, y) => arguments(2).asInstanceOf[(List[Any] => Any)](List(Lazy(x), Lazy(y))))),
    "List_empty" -> ((arguments: List[Any]) => Nil),
    "List_size" -> strict(arguments => BigInt(arguments(0).asInstanceOf[List[Any]].size)),
    "List_append" -> strict(arguments => arguments(0).asInstanceOf[List[Any]] :+ arguments(1)),
    "List_prepend" -> strict(arguments => arguments(0) +: arguments(1).asInstanceOf[List[Any]]),
    "List_tail" -> strict(arguments => arguments(0).asInstanceOf[List[Any]].tail),
    "List_init" -> strict(arguments => arguments(0).asInstanceOf[List[Any]].init),
    "List_fold" -> strict(arguments => arguments(0).asInstanceOf[List[Any]].fold(arguments(1))(
      (x, y) => arguments(2).asInstanceOf[(List[Any] => Any)](List(Lazy(x), Lazy(y))))),
    "List_get" -> strict(arguments => arguments(0).asInstanceOf[List[Any]](arguments(1).asInstanceOf[BigInt].toInt)),
    "List_set" -> strict(arguments => arguments(0).asInstanceOf[List[Any]].updated(arguments(1).asInstanceOf[BigInt].toInt, arguments(2))),
    "String_concat" -> strict(arguments => arguments(0).asInstanceOf[String] + arguments(1).asInstanceOf[String]),
    "toString" -> strict(arguments => arguments(0).toString),
    "String_format" -> strict(arguments =>
      arguments(0).asInstanceOf[String].formatLocal(Locale.ROOT, arguments(1))),
    "CTF_getInt" -> strict(arguments => Ctf.getInt(arguments(0).asInstanceOf[ICompositeDefinition], arguments(1).asInstanceOf[String])),
    "CTF_getString" -> strict(arguments => Ctf.getString(arguments(0).asInstanceOf[ICompositeDefinition], arguments(1).asInstanceOf[String]))
  )
}

// TODO: All arguments are currently wrapped in Lazy, even if they are strictly evaluated
// TODO: Replace argument lists by tuples?
class RuntimeEvaluator(externs: Map[String, List[Lazy[Any]] => Any]) {

  def evalExpressionArg(arg: Core.ExpressionArg, env: Env): Lazy[Any] = arg match {
    case Core.ExpressionRef(id, _, _) => env(id.id)
    case e: Core.Expression => Lazy(evalExpression(e, env))
  }

  def evalExpression(exp: Core.Expression, env: Env): Any = exp match {
    case Core.FunctionExpression(_, params, body, result, location) =>
      (args: Seq[Lazy[Any]]) => {
        if (params.size != args.size) {
          throw InternalError(s"Called with wrong number of arguments.", location)
        }
        lazy val newEnv: Env = env ++ params.map(_._1.id).zip(args) ++ body.map(e => (e._1.id, Lazy(evalExpressionArg(e._2, newEnv))))
        evalExpressionArg(result, newEnv).get
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
      f.asInstanceOf[List[Lazy[Any]] => Any](newArgs)
    case Core.TypeApplicationExpression(applicable, _, _) =>
      evalExpressionArg(applicable, env).get
    case Core.RecordConstructorExpression(entries, _) =>
      Record(entries.map(e => (e._1.id, evalExpressionArg(e._2, env).get))) // TODO: entries are now strictly evaluated, should they be lazy?
    case Core.RecordAccesorExpression(name, target, _) =>
      evalExpressionArg(target, env).get.asInstanceOf[Record].entries(name.id)
    case Core.StringLiteralExpression(value, _) => value
    case Core.IntLiteralExpression(value, _) => value
    case Core.FloatLiteralExpression(value, _) => value
  }

}


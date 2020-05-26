package de.uni_luebeck.isp.tessla

import java.util.Locale

import cats.Monad
import de.uni_luebeck.isp.tessla.RuntimeEvaluator.RuntimeError
import de.uni_luebeck.isp.tessla.util.Lazy
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition

import scala.collection.immutable.ArraySeq
import cats._
import cats.implicits._
import de.uni_luebeck.isp.tessla.util.ArraySeqMonad._
import org.eclipse.tracecompass.ctf.core.event.IEventDefinition

object RuntimeExterns {

  def propagate[A[+_]: Monad](args: ArraySeq[Any])(f: ArraySeq[Any] => A[Any]): A[Any] =
    args.find(_.isInstanceOf[RuntimeError]).map(_.pure[A]).getOrElse(f(args))

  def propagateSingle[A[+_]: Monad](arg: Any)(f: Any => A[Any]): A[Any] =
    if (arg.isInstanceOf[RuntimeError]) arg.pure[A] else f(arg)

  def strict[A[+_]: Monad](f: ArraySeq[Any] => A[Any]): Extern[A] = (list: ArraySeq[A[Any]]) =>
    list.sequence.flatMap(propagate(_)(f))

  def binIntOp[A[+_]: Monad](f: (BigInt, BigInt) => Any) =
    strict(l => f(l(0).asInstanceOf[BigInt], l(1).asInstanceOf[BigInt]).pure[A])

  def unaryIntOp[A[+_]: Monad](f: BigInt => Any) = strict(l => f(l(0).asInstanceOf[BigInt]).pure[A])

  def binBoolOp[A[+_]: Monad](f: (Boolean, Boolean) => Any) =
    strict(l => f(l(0).asInstanceOf[Boolean], l(1).asInstanceOf[Boolean]).pure[A])

  def binFloatOp[A[+_]: Monad](f: (Double, Double) => Any) =
    strict(l => f(l(0).asInstanceOf[Double], l(1).asInstanceOf[Double]).pure[A])

  def binAnyOp[A[+_]: Monad](f: (Any, Any) => Any) = strict(l => f(l(0), l(1)).pure[A])

  def unaryFloatOp[A[+_]: Monad](f: Double => Any) =
    strict(l => f(l(0).asInstanceOf[Double]).pure[A])

  type Extern[A[+_]] = ArraySeq[A[Any]] => A[Any]

  val runtimeCommonExterns = commonExterns[Lazy]

  def commonExterns[A[+_]: Monad]: Map[String, Extern[A]] = Map(
    "true" -> ((_: ArraySeq[A[Any]]) => true.pure[A]),
    "false" -> ((_: ArraySeq[A[Any]]) => false.pure[A]),
    "add" -> binIntOp(_ + _),
    "sub" -> binIntOp(_ - _),
    "mul" -> binIntOp(_ * _),
    "div" -> binIntOp((a, b) => if (b == 0) RuntimeError("Division by zero") else a / b),
    "mod" -> binIntOp((a, b) => if (b == 0) RuntimeError("Division by zero") else a % b),
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
    "and" -> ((arguments: ArraySeq[A[Any]]) =>
      arguments(0).flatMap { cond =>
        propagateSingle(cond) { c =>
          if (c.asInstanceOf[Boolean]) arguments(1) else false.pure[A]
        }
      }
    ),
    "or" -> ((arguments: ArraySeq[A[Any]]) =>
      arguments(0).flatMap { cond =>
        propagateSingle(cond) { c =>
          if (c.asInstanceOf[Boolean]) true.pure[A] else arguments(1)
        }
      }
    ),
    "not" -> strict { arguments => (!arguments(0).asInstanceOf[Boolean]).pure[A] },
    "ite" -> ((arguments: ArraySeq[A[Any]]) =>
      arguments(0).flatMap { cond =>
        propagateSingle(cond) { c =>
          if (c.asInstanceOf[Boolean]) arguments(1) else arguments(2)
        }
      }
    ),
    "staticite" -> ((arguments: ArraySeq[A[Any]]) =>
      arguments(0).flatMap { cond =>
        propagateSingle(cond) { c =>
          if (c.asInstanceOf[Boolean]) arguments(1) else arguments(2)
        }
      }
    ),
    "pow" -> binFloatOp(math.pow),
    "log" -> binFloatOp((x, base) => math.log(x) / math.log(base)),
    "sin" -> unaryFloatOp(Math.sin),
    "cos" -> unaryFloatOp(Math.cos),
    "tan" -> unaryFloatOp(Math.tan),
    "atan" -> unaryFloatOp(Math.atan),
    "intToFloat" -> unaryIntOp(_.toDouble),
    "floatToInt" -> unaryFloatOp(x =>
      try {
        BigDecimal(x).toBigInt
      } catch {
        case e: NumberFormatException => RuntimeError(e.getMessage)
      }
    ),
    "None" -> ((_: ArraySeq[A[Any]]) => None.pure[A]),
    "Some" -> strict(arguments => Some(arguments(0)).pure[A]),
    "isNone" -> strict(arguments =>
      propagateSingle(arguments(0))(_.asInstanceOf[Option[Any]].isEmpty.pure[A])
    ),
    "getSome" -> strict(arguments =>
      propagateSingle(arguments(0))(
        _.asInstanceOf[Option[Any]].getOrElse(RuntimeError("Tried get on None.")).pure[A]
      )
    ),
    "Map_empty" -> ((_: ArraySeq[A[Any]]) => Map().pure[A]),
    "Map_add" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[Map[Any, Any]] + (arguments(1) -> arguments(2))).pure[A]
    }),
    "Map_get" -> strict(propagate(_) { arguments =>
      arguments(0)
        .asInstanceOf[Map[Any, Any]]
        .getOrElse(arguments(1), RuntimeError("No such element."))
        .pure[A]
    }),
    "Map_contains" -> strict(propagate(_) { arguments =>
      arguments(0).asInstanceOf[Map[Any, A[Any]]].contains(arguments(1)).pure[A]
    }),
    "Map_remove" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[Map[Any, A[Any]]] - arguments(1)).pure[A]
    }),
    "Map_size" -> strict(propagate(_) { arguments =>
      BigInt(arguments(0).asInstanceOf[Map[Any, A[Any]]].size).pure[A]
    }),
    "Map_keys" -> strict(propagate(_) { arguments =>
      arguments(0).asInstanceOf[Map[Any, Any]].keys.toList.pure[A]
    }),
    "Map_fold" -> ((arguments: ArraySeq[A[Any]]) =>
      arguments(0).flatMap(propagateSingle(_) { map =>
        arguments(2).flatMap(propagateSingle(_) { f =>
          map.asInstanceOf[Map[Any, Any]].foldLeft(arguments(1)) { (a, b) =>
            f.asInstanceOf[Extern[A]](ArraySeq.untagged(a, b._1.pure[A], b._2.pure[A]))
          }
        })
      })
    ),
    "Set_empty" -> ((_: ArraySeq[A[Any]]) => Set().pure[A]),
    "Set_add" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[Set[Any]] + arguments(1)).pure[A]
    }),
    "Set_contains" -> strict(propagate(_) { arguments =>
      arguments(0).asInstanceOf[Set[Any]].contains(arguments(1)).pure[A]
    }),
    "Set_remove" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[Set[Any]] - arguments(1)).pure[A]
    }),
    "Set_size" -> strict(propagate(_) { arguments =>
      BigInt(arguments(0).asInstanceOf[Set[Any]].size).pure[A]
    }),
    "Set_union" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[Set[Any]] | arguments(1).asInstanceOf[Set[Any]]).pure[A]
    }),
    "Set_intersection" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[Set[Any]] & arguments(1).asInstanceOf[Set[Any]]).pure[A]
    }),
    "Set_minus" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[Set[Any]] -- arguments(1).asInstanceOf[Set[Any]]).pure[A]
    }),
    "Set_fold" -> ((arguments: ArraySeq[A[Any]]) =>
      arguments(0).flatMap(propagateSingle(_) { map =>
        arguments(2).flatMap(propagateSingle(_) { f =>
          map.asInstanceOf[Set[Any]].foldLeft(arguments(1)) { (a, b) =>
            f.asInstanceOf[Extern[A]](ArraySeq.untagged(a, b.pure[A]))
          }
        })
      })
    ),
    "List_empty" -> ((_: ArraySeq[Any]) => Nil.pure[A]),
    "List_size" -> strict(propagate(_) { arguments =>
      BigInt(arguments(0).asInstanceOf[List[Any]].size).pure[A]
    }),
    "List_append" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[List[Any]] :+ arguments(1)).pure[A]
    }),
    "List_prepend" -> strict(propagate(_) { arguments =>
      (arguments(0) +: arguments(1).asInstanceOf[List[Any]]).pure[A]
    }),
    "List_tail" -> strict(propagate(_) { arguments =>
      val list = arguments(0).asInstanceOf[List[Any]]
      (if (list.isEmpty) RuntimeError("Tail unsupported for empty list.") else list.tail).pure[A]
    }),
    "List_init" -> strict(propagate(_) { arguments =>
      val list = arguments(0).asInstanceOf[List[Any]]
      (if (list.isEmpty) RuntimeError("Init unsupported for empty list.") else list.init).pure[A]
    }),
    "List_fold" -> ((arguments: ArraySeq[A[Any]]) =>
      arguments(0).flatMap(propagateSingle(_) { map =>
        arguments(2).flatMap(propagateSingle(_) { f =>
          map.asInstanceOf[List[Any]].foldLeft(arguments(1)) { (a, b) =>
            f.asInstanceOf[Extern[A]](ArraySeq.untagged(a, b.pure[A]))
          }
        })
      })
    ),
    "List_get" -> strict(propagate(_) { arguments =>
      try {
        arguments(0).asInstanceOf[List[Any]](arguments(1).asInstanceOf[BigInt].toInt).pure[A]
      } catch {
        case e: IndexOutOfBoundsException => RuntimeError("Index out of bounds.").pure[A]
      }
    }),
    "List_set" -> strict(propagate(_) { arguments =>
      (try {
        arguments(0)
          .asInstanceOf[List[Any]]
          .updated(arguments(1).asInstanceOf[BigInt].toInt, arguments(2))
      } catch {
        case e: IndexOutOfBoundsException => RuntimeError("Index out of bounds.")
      }).pure[A]
    }),
    "String_concat" -> strict(propagate(_) { arguments =>
      (arguments(0).asInstanceOf[String] + arguments(1).asInstanceOf[String]).pure[A]
    }),
    "toString" -> strict(propagate(_)(_(0).toString.pure[A])),
    "String_format" -> strict(propagate(_) { arguments =>
      arguments(0)
        .asInstanceOf[String]
        .formatLocal(Locale.ROOT, arguments(1))
        .pure[A] // TODO: error handling?
    }),
    "CTF_getInt" -> strict(propagate(_) { arguments =>
      (try {
        Ctf.getInt(
          arguments(0).asInstanceOf[IEventDefinition],
          arguments(1).asInstanceOf[String],
          Location.unknown
        ) // TODO: error handling?
      } catch {
        case e: ClassCastException => RuntimeError(e.getMessage)
      }).pure[A]
    }),
    "CTF_getString" -> strict(propagate(_) { arguments =>
      (try {
        Ctf.getString(
          arguments(0).asInstanceOf[IEventDefinition],
          arguments(1).asInstanceOf[String],
          Location.unknown
        ) // TODO: error handling?
      } catch {
        case e: ClassCastException => RuntimeError(e.getMessage)
      }).pure[A]
    }),
    "error" -> strict(propagate(_) { arguments => // TODO: declare in standard library
      RuntimeEvaluator.RuntimeError(arguments(0).asInstanceOf[String]).pure[A]
    })
  )

}

package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.KeyNotFound
import de.uni_luebeck.isp.tessla.PrimitiveOperators.{Monomorphic, Strict}
import de.uni_luebeck.isp.tessla.builtins.{AbstractTimeQueue, BigIntInfinity, BigInterval, TimeQueue}

trait CustomBuiltIns {
  def customTypes: Map[String, Types.CustomType]
  def customOperators: Map[(String, Int), PrimitiveOperators.CustomBuiltIn]
}

object CustomBuiltIns {
  val none: CustomBuiltIns = new CustomBuiltIns {
    override def customOperators = Map()
    override def customTypes = Map()
  }

  val mapSetAndQueue: CustomBuiltIns = new CustomBuiltIns {
    case class IntIntMap(value: Map[BigInt, BigInt], loc: Location) extends TesslaCore.CustomValue {
      override def typ = IntIntMap.Type

      override def withLoc(loc: Location) = IntIntMap(value, loc)
    }

    object IntIntMap {
      case object Type extends Types.CustomType {
        override def toString = "Map[Int, Int]"
      }

      case object Empty extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict{
        override def argumentTypes = Seq()

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq() =>
            Some(IntIntMap(Map(), loc))
        }
      }

      case object Add extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntIntMap(map, _), TesslaCore.IntLiteral(key, _), TesslaCore.IntLiteral(value, _)) =>
            Some(IntIntMap(map + (key -> value), loc))
        }
      }

      case object Get extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Int

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntIntMap(map, _), key: TesslaCore.IntLiteral) =>
            map.get(key.value) match {
              case Some(value) =>
                Some(TesslaCore.IntLiteral(value, loc))
              case None =>
                throw KeyNotFound(key, map, loc)
            }
        }
      }

      case object Contains extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Bool

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntIntMap(map, _), TesslaCore.IntLiteral(key, _)) =>
            Some(TesslaCore.BoolLiteral(map.contains(key), loc))
        }
      }

      case object Remove extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntIntMap(map, _), TesslaCore.IntLiteral(key, _)) =>
            Some(IntIntMap(map - key, loc))
        }
      }
    }

    val INFINITY = BigInt("99999999999999999999")

    case class IntTimeQueue(value: TimeQueue[BigInt], loc: Location) extends TesslaCore.CustomValue {
      override def typ = IntTimeQueue.Type

      override def withLoc(loc: Location) = IntTimeQueue(value, loc)
    }

    object IntTimeQueue {
      case object Type extends Types.CustomType {
        override def toString = "TimeQueue[Int]"
      }

      case object Empty extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict{
        override def argumentTypes = Seq()

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq() =>
            Some(IntTimeQueue(TimeQueue.empty, loc))
        }
      }

      case object Enqueue extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntTimeQueue(queue, _), TesslaCore.IntLiteral(time, _), TesslaCore.IntLiteral(value, _)) =>
            Some(IntTimeQueue(queue.enqueue(time, value), loc))
        }
      }

      case object RemoveOlder extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntTimeQueue(queue, _), TesslaCore.IntLiteral(time, _)) =>
            Some(IntTimeQueue(queue.removeOlder(time), loc))
        }
      }

      case object RemoveNewer extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntTimeQueue(queue, _), TesslaCore.IntLiteral(time, _)) =>
            Some(IntTimeQueue(queue.removeNewer(time), loc))
        }
      }

      case object WeightedSum extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Int

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntTimeQueue(queue, _), TesslaCore.IntLiteral(until, _)) =>
            Some(TesslaCore.IntLiteral(queue.fold(0: BigInt, until){(t1, t2, d, acc) => acc + (t2 - t1) * d}, loc))
        }
      }

      case object DataTimeout extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type)

        override protected def returnType = Types.Int

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntTimeQueue(queue, _)) => {
            queue.dataTimeout.value match {
              case Some(result) => Some(TesslaCore.IntLiteral(result, loc))
              case None => Some(TesslaCore.IntLiteral(INFINITY, loc))
            }
          }
        }
      }
    }

    case class AbstractIntTimeQueue(value: AbstractTimeQueue, loc: Location) extends TesslaCore.CustomValue {
      override def typ = AbstractIntTimeQueue.Type

      override def withLoc(loc: Location) = AbstractIntTimeQueue(value, loc)
    }

    object AbstractIntTimeQueue {
      case object Type extends Types.CustomType {
        override def toString = "AbstractTimeQueue[Int]"
      }

      case object Empty extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict{
        override def argumentTypes = Seq()

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq() =>
            Some(AbstractIntTimeQueue(AbstractTimeQueue.empty, loc))
        }
      }

      case object Top extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict{
        override def argumentTypes = Seq()

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq() =>
            Some(AbstractIntTimeQueue(AbstractTimeQueue.top, loc))
        }
      }

      case object Enqueue extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(AbstractIntTimeQueue(queue, _), TesslaCore.IntLiteral(time, _), TesslaCore.IntLiteral(value, _)) =>
            Some(AbstractIntTimeQueue(queue.enqueue(time, BigInterval(value)), loc))
        }
      }

      case object EnqueueFinite extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int, Types.Int, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(AbstractIntTimeQueue(queue, _), TesslaCore.IntLiteral(time, _), TesslaCore.IntLiteral(value, _), TesslaCore.IntLiteral(limit, _)) =>
            Some(AbstractIntTimeQueue(queue.enqueueFinite(time, BigInterval(value), limit.intValue()), loc))
        }
      }

      case object RemoveOlder extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(AbstractIntTimeQueue(queue, _), TesslaCore.IntLiteral(time, _)) =>
            Some(AbstractIntTimeQueue(queue.removeOlder(time), loc))
        }
      }

      case object RemoveNewer extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(AbstractIntTimeQueue(queue, _), TesslaCore.IntLiteral(time, _)) =>
            Some(AbstractIntTimeQueue(queue.removeNewer(time), loc))
        }
      }

      val weightedSum = (startTime: BigInt) => (a: BigInt, b: BigInt, v: BigInterval, acc: BigInterval) =>
        (acc.limit(0, (a - startTime) * 1000) + v.limit(0, 1) * BigInterval(b-a))

      case object WeightedSum1 extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int, Types.Int)

        override protected def returnType = Types.Int

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(AbstractIntTimeQueue(queue, _), TesslaCore.IntLiteral(startTime, _), TesslaCore.IntLiteral(until, _)) =>
            val value = queue.fold(BigInterval(0), until)(weightedSum(startTime)).left
            val v = value.value match {
              case None => INFINITY
              case Some(x) => x
            }
            Some(TesslaCore.IntLiteral(v, loc))
        }
      }

      case object WeightedSum2 extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int, Types.Int)

        override protected def returnType = Types.Int

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(AbstractIntTimeQueue(queue, _), TesslaCore.IntLiteral(startTime, _), TesslaCore.IntLiteral(until, _)) =>
            val value = queue.fold(BigInterval(0), until)(weightedSum(startTime)).right
            val v = value.value match {
              case None => INFINITY
              case Some(x) => x
            }
            Some(TesslaCore.IntLiteral(v, loc))
        }
      }

      case object DataTimeout extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type)

        override protected def returnType = Types.Int

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(AbstractIntTimeQueue(queue, _)) => {
            queue.dataTimeout.value match {
              case Some(result) => Some(TesslaCore.IntLiteral(result, loc))
              case None => Some(TesslaCore.IntLiteral(INFINITY, loc))
            }
          }
        }
      }
    }

    case class IntSet(value: Set[BigInt], loc: Location) extends TesslaCore.CustomValue {
      override def typ = IntSet.Type

      override def withLoc(loc: Location) = IntSet(value, loc)
    }

    object IntSet {
      case object Type extends Types.CustomType {
        override def toString = "Set[Int]"
      }

      case object Empty extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq()

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq() =>
            Some(IntSet(Set(), loc))
        }
      }

      case object Add extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntSet(set, _), TesslaCore.IntLiteral(value, _)) =>
            Some(IntSet(set + value, loc))
        }
      }

      case object Contains extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Bool

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntSet(set, _), TesslaCore.IntLiteral(value, _)) =>
            Some(TesslaCore.BoolLiteral(set.contains(value), loc))
        }
      }

      case object Remove extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntSet(set, _), TesslaCore.IntLiteral(key, _)) =>
            Some(IntSet(set - key, loc))
        }
      }
    }

    override def customTypes = Map("IntIntMap" -> IntIntMap.Type, "IntSet" -> IntSet.Type)

    def addMonomorphicOperators(operators: Map[String, PrimitiveOperators.CustomBuiltIn with Monomorphic]) = {
      operators.map { case (name, op) => (name, op.argumentTypes.size) -> op }
    }

    override def customOperators = addMonomorphicOperators(Map(
      "Map_empty" -> IntIntMap.Empty,
      "Map_add" -> IntIntMap.Add,
      "Map_get" -> IntIntMap.Get,
      "Map_contains" -> IntIntMap.Contains,
      "Map_remove" -> IntIntMap.Remove,
      "Set_empty" -> IntSet.Empty,
      "Set_add" -> IntSet.Add,
      "Set_contains" -> IntSet.Contains,
      "Set_remove" -> IntSet.Remove,
      "TimeQueue_empty" -> IntTimeQueue.Empty,
      "TimeQueue_enqueue" -> IntTimeQueue.Enqueue,
      "TimeQueue_removeOlder" -> IntTimeQueue.RemoveOlder,
      "TimeQueue_removeNewer" -> IntTimeQueue.RemoveNewer,
      "TimeQueue_weightedSum" -> IntTimeQueue.WeightedSum,
      "TimeQueue_dataTimeout" -> IntTimeQueue.DataTimeout,
      "AbstractTimeQueue_empty" -> AbstractIntTimeQueue.Empty,
      "AbstractTimeQueue_top" -> AbstractIntTimeQueue.Top,
      "AbstractTimeQueue_enqueue" -> AbstractIntTimeQueue.Enqueue,
      "AbstractTimeQueue_enqueueFinite" -> AbstractIntTimeQueue.EnqueueFinite,
      "AbstractTimeQueue_removeOlder" -> AbstractIntTimeQueue.RemoveOlder,
      "AbstractTimeQueue_removeNewer" -> AbstractIntTimeQueue.RemoveNewer,
      "AbstractTimeQueue_weightedSum1" -> AbstractIntTimeQueue.WeightedSum1,
      "AbstractTimeQueue_weightedSum2" -> AbstractIntTimeQueue.WeightedSum2,
      "AbstractTimeQueue_dataTimeout" -> AbstractIntTimeQueue.DataTimeout
    ))
  }
}

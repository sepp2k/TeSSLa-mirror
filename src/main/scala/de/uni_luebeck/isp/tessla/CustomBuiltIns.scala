package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.KeyNotFound
import de.uni_luebeck.isp.tessla.PrimitiveOperators.{Monomorphic, Strict}
import de.uni_luebeck.isp.tessla.builtins.TimeQueue

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
        override def toString = "Map<Int, Int>"
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

      case object Newest extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Int

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntTimeQueue(queue, _), TesslaCore.IntLiteral(default, _)) =>
            Some(TesslaCore.IntLiteral(queue.newest(default), loc))
        }
      }

      case object WeightedSum extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type)

        override protected def returnType = Types.Int

        override protected def strictEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntTimeQueue(queue, _)) =>
            Some(TesslaCore.IntLiteral(queue.fold(0: BigInt){(t1, t2, d, acc) => acc + (t2 - t1) * d}, loc))
        }
      }
    }

    case class IntSet(value: Set[BigInt], loc: Location) extends TesslaCore.CustomValue {
      override def typ = IntSet.Type

      override def withLoc(loc: Location) = IntSet(value, loc)
    }

    object IntSet {
      case object Type extends Types.CustomType {
        override def toString = "Set<Int>"
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

    override def customOperators = Map(
      ("map_empty", 0) -> IntIntMap.Empty,
      ("map_add", 3) -> IntIntMap.Add,
      ("map_get", 2) -> IntIntMap.Get,
      ("map_contains", 2) -> IntIntMap.Contains,
      ("map_remove", 2) -> IntIntMap.Remove,
      ("set_empty", 0) -> IntSet.Empty,
      ("set_add", 2) -> IntSet.Add,
      ("set_contains", 2) -> IntSet.Contains,
      ("set_remove", 2) -> IntSet.Remove,
      ("TimeQueue_empty", 0) -> IntTimeQueue.Empty,
      ("TimeQueue_enqueue", 3) -> IntTimeQueue.Enqueue,
      ("TimeQueue_removeOlder", 2) -> IntTimeQueue.RemoveOlder,
      ("TimeQueue_removeNewer", 2) -> IntTimeQueue.RemoveNewer,
      ("TimeQueue_newest", 1) -> IntTimeQueue.Newest,
      ("TimeQueue_weightedSum", 1) -> IntTimeQueue.WeightedSum
    )
  }
}

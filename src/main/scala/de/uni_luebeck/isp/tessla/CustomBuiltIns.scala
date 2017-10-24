package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.KeyNotFound

trait CustomBuiltIns {
  def customTypes: Map[String, Types.CustomType]
  def customOperators: Map[(String, Int), PrimitiveOperators.CustomBuiltIn]
}

object CustomBuiltIns {
  val none: CustomBuiltIns = new CustomBuiltIns {
    override def customOperators = Map()
    override def customTypes = Map()
  }

  val mapAndSet: CustomBuiltIns = new CustomBuiltIns {
    case class IntIntMap(map: Map[BigInt, BigInt], loc: Location) extends TesslaCore.CustomValue {
      override def typ = IntIntMap.Type

      override def withLoc(loc: Location) = IntIntMap(map, loc)

      override def toString = map.toString
    }

    object IntIntMap {
      case object Type extends Types.CustomType

      case object Empty extends PrimitiveOperators.CustomBuiltIn with PrimitiveOperators.Monomorphic {
        override def argumentTypes = Seq()

        override protected def returnType = Type

        override protected def doEval(values: Seq[TesslaCore.Value], loc: Location) = values match {
          case Seq() =>
            Some(IntIntMap(Map(), loc))
        }
      }

      case object Add extends PrimitiveOperators.CustomBuiltIn with PrimitiveOperators.Monomorphic {
        override def argumentTypes = Seq(Type, Types.Int, Types.Int)

        override protected def returnType = Type

        override protected def doEval(values: Seq[TesslaCore.Value], loc: Location) = values match {
          case Seq(IntIntMap(map, _), TesslaCore.IntLiteral(key, _), TesslaCore.IntLiteral(value, _)) =>
            Some(IntIntMap(map + (key -> value), loc))
        }
      }

      case object Get extends PrimitiveOperators.CustomBuiltIn with PrimitiveOperators.Monomorphic {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Int

        override protected def doEval(values: Seq[TesslaCore.Value], loc: Location) = values match {
          case Seq(IntIntMap(map, _), key: TesslaCore.IntLiteral) =>
            map.get(key.value) match {
              case Some(value) =>
                Some(TesslaCore.IntLiteral(value, loc))
              case None =>
                throw KeyNotFound(key, map, loc)
            }
        }
      }

      case object Contains extends PrimitiveOperators.CustomBuiltIn with PrimitiveOperators.Monomorphic {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Bool

        override protected def doEval(values: Seq[TesslaCore.Value], loc: Location) = values match {
          case Seq(IntIntMap(map, _), TesslaCore.IntLiteral(key, _)) =>
            Some(TesslaCore.BoolLiteral(map.contains(key), loc))
        }
      }
    }

    case class IntSet(set: Set[BigInt], loc: Location) extends TesslaCore.CustomValue {
      override def typ = IntSet.Type

      override def withLoc(loc: Location) = IntSet(set, loc)

      override def toString = set.toString
    }

    object IntSet {
      case object Type extends Types.CustomType

      case object Empty extends PrimitiveOperators.CustomBuiltIn with PrimitiveOperators.Monomorphic {
        override def argumentTypes = Seq()

        override protected def returnType = Type

        override protected def doEval(values: Seq[TesslaCore.Value], loc: Location) = values match {
          case Seq() =>
            Some(IntSet(Set(), loc))
        }
      }

      case object Add extends PrimitiveOperators.CustomBuiltIn with PrimitiveOperators.Monomorphic {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def doEval(values: Seq[TesslaCore.Value], loc: Location) = values match {
          case Seq(IntSet(set, _), TesslaCore.IntLiteral(value, _)) =>
            Some(IntSet(set + value, loc))
        }
      }

      case object Contains extends PrimitiveOperators.CustomBuiltIn with PrimitiveOperators.Monomorphic {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Bool

        override protected def doEval(values: Seq[TesslaCore.Value], loc: Location) = values match {
          case Seq(IntSet(set, _), TesslaCore.IntLiteral(value, _)) =>
            Some(TesslaCore.BoolLiteral(set.contains(value), loc))
        }
      }
    }

    override def customTypes = Map("IntIntMap" -> IntIntMap.Type, "IntSet" -> IntSet.Type)

    override def customOperators = Map(
      ("map_empty", 0) -> IntIntMap.Empty,
      ("map_add", 3) -> IntIntMap.Add,
      ("map_get", 2) -> IntIntMap.Get,
      ("map_contains", 2) -> IntIntMap.Contains,
      ("set_empty", 0) -> IntSet.Empty,
      ("set_add", 2) -> IntSet.Add,
      ("set_contains", 2) -> IntSet.Contains
    )
  }
}
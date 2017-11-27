package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{KeyNotFound, TypeArityMismatch, TypeMismatch}
import de.uni_luebeck.isp.tessla.PrimitiveOperators.{Monomorphic, Strict}

trait CustomBuiltIns {
  // TODO: Explicit type name currently not supported
  // def customTypes: Map[String, Types.CustomType]
  def customOperators: Map[(String, Int), PrimitiveOperators.CustomBuiltIn]
}

object CustomBuiltIns {
  val none: CustomBuiltIns = new CustomBuiltIns {
    override def customOperators = Map()
    // override def customTypes = Map()
  }

  val mapAndSet: CustomBuiltIns = new CustomBuiltIns {
    case class TesslaMap(value: Map[TesslaCore.LiteralValue, TesslaCore.LiteralValue], typ: TesslaMap.Type, loc: Location)
      extends TesslaCore.CustomValue {
      override def withLoc(loc: Location) = TesslaMap(value, typ, loc)
    }

    object TesslaMap {
      case class Type(keyType: Types.ValueType, valueType: Types.ValueType) extends Types.CustomType {
        override def toString = s"Map<$keyType, $valueType>"
      }

      case object Empty extends PrimitiveOperators.CustomBuiltIn with Strict {
        override def returnTypeFor(typeArgs: Seq[Types.ValueType], argTypes: Seq[(Types.ValueType, Location)]) = {
          if (typeArgs.length != 2) {
            // TODO Unknown location
            throw TypeArityMismatch("map_empty", 2, typeArgs.length, Location.unknown)
          }
          Type(typeArgs(0), typeArgs(1))
        }

        override protected def strictEval(typeArgs: Seq[Types.ValueType], args: Seq[TesslaCore.LiteralValue], loc: Location) = {
          args match {
            case Seq() =>
              Some(TesslaMap(Map(), Type(typeArgs(0), typeArgs(1)), loc))
          }
        }
      }

      case object Add extends PrimitiveOperators.CustomBuiltIn with Strict {
        def getOrInferTypeArgs(typeArgs: Seq[Types.ValueType], argTypes: Seq[Types.ValueType]) = typeArgs match {
          case Seq() => Type(argTypes(1), argTypes(2))
          case Seq(keyType, valueType) => Type(keyType, valueType)
          case _ =>
            // TODO Unknown location
            throw TypeArityMismatch("map_add", 2, typeArgs.length, Location.unknown)
        }

        override def returnTypeFor(typeArgs: Seq[Types.ValueType], argTypes: Seq[(Types.ValueType, Location)]) = {
          val returnType = getOrInferTypeArgs(typeArgs, argTypes.map(_._1))
          Types.requireType(returnType, argTypes(0)._1, argTypes(0)._2)
          Types.requireType(returnType.keyType, argTypes(1)._1, argTypes(1)._2)
          Types.requireType(returnType.valueType, argTypes(2)._1, argTypes(2)._2)
          returnType
        }

        override protected def strictEval(typeArgs: Seq[Types.ValueType], args: Seq[TesslaCore.LiteralValue], loc: Location) = args match {
          case Seq(map: TesslaMap, key, value) =>
            Some(TesslaMap(map.value + (key -> value), map.typ, loc))
        }
      }

      case object Get extends PrimitiveOperators.CustomBuiltIn with Strict {
        def getOrInferTypeArgs(typeArgs: Seq[Types.ValueType], argTypes: Seq[(Types.ValueType, Location)]) = typeArgs match {
          case Seq() => argTypes(0) match {
            case (mapType: Type, _) => mapType
            case (other, loc) => throw TypeMismatch(Type(Types.Nothing, Types.Nothing), other, loc)
          }
          case Seq(keyType, valueType) => Type(keyType, valueType)
          case _ =>
            // TODO Unknown location
            throw TypeArityMismatch("map_get", 2, typeArgs.length, Location.unknown)
        }

        override def returnTypeFor(typeArgs: Seq[Types.ValueType], argTypes: Seq[(Types.ValueType, Location)]) = {
          val mapType = getOrInferTypeArgs(typeArgs, argTypes)
          Types.requireType(mapType, argTypes(0)._1, argTypes(0)._2)
          Types.requireType(mapType.keyType, argTypes(1)._1, argTypes(1)._2)
          mapType.valueType
        }

        override protected def strictEval(typeArgs: Seq[Types.ValueType], args: Seq[TesslaCore.LiteralValue], loc: Location) = args match {
          case Seq(TesslaMap(map, _, _), key) =>
            map.get(key) match {
              case Some(value) =>
                Some(value)
              case None =>
                throw KeyNotFound(key, map, loc)
            }
        }
      }

      case object Contains extends PrimitiveOperators.CustomBuiltIn with Strict {
        def getOrInferTypeArgs(typeArgs: Seq[Types.ValueType], argTypes: Seq[(Types.ValueType, Location)]) = typeArgs match {
          case Seq() => argTypes(0) match {
            case (mapType: Type, _) => mapType
            case (other, loc) => throw TypeMismatch(Type(Types.Nothing, Types.Nothing), other, loc)
          }
          case Seq(keyType, valueType) => Type(keyType, valueType)
          case _ =>
            // TODO Unknown location
            throw TypeArityMismatch("map_contains", 2, typeArgs.length, Location.unknown)
        }

        override def returnTypeFor(typeArgs: Seq[Types.ValueType], argTypes: Seq[(Types.ValueType, Location)]) = {
          val mapType = getOrInferTypeArgs(typeArgs, argTypes)
          Types.requireType(mapType, argTypes(0)._1, argTypes(0)._2)
          Types.requireType(mapType.keyType, argTypes(1)._1, argTypes(1)._2)
          Types.Bool
        }

        override protected def strictEval(typeArgs: Seq[Types.ValueType], args: Seq[TesslaCore.LiteralValue], loc: Location) = args match {
          case Seq(TesslaMap(map, _, _), key) =>
            Some(TesslaCore.BoolLiteral(map.contains(key), loc))
        }
      }

      case object Remove extends PrimitiveOperators.CustomBuiltIn with Strict {
        def getOrInferTypeArgs(typeArgs: Seq[Types.ValueType], argTypes: Seq[(Types.ValueType, Location)]) = typeArgs match {
          case Seq() => argTypes(0) match {
            case (mapType: Type, _) => mapType
            case (other, loc) =>
              throw TypeMismatch(Type(Types.Nothing, Types.Nothing), other, loc)
          }
          case Seq(keyType, valueType) => Type(keyType, valueType)
          case _ =>
            // TODO Unknown location
            throw TypeArityMismatch("map_contains", 2, typeArgs.length, Location.unknown)
        }

        override def returnTypeFor(typeArgs: Seq[Types.ValueType], argTypes: Seq[(Types.ValueType, Location)]) = {
          val mapType = getOrInferTypeArgs(typeArgs, argTypes)
          Types.requireType(mapType, argTypes(0)._1, argTypes(0)._2)
          Types.requireType(mapType.keyType, argTypes(1)._1, argTypes(1)._2)
          mapType
        }

        override protected def strictEval(typeArgs: Seq[Types.ValueType], args: Seq[TesslaCore.LiteralValue], loc: Location) = args match {
          case Seq(TesslaMap(map, typ, _), key) =>
            Some(TesslaMap(map - key, typ, loc))
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

        override protected def strictEval(typeArgs: Seq[Types.ValueType], values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq() =>
            Some(IntSet(Set(), loc))
        }
      }

      case object Add extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(typeArgs: Seq[Types.ValueType], values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntSet(set, _), TesslaCore.IntLiteral(value, _)) =>
            Some(IntSet(set + value, loc))
        }
      }

      case object Contains extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Types.Bool

        override protected def strictEval(typeArgs: Seq[Types.ValueType], values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntSet(set, _), TesslaCore.IntLiteral(value, _)) =>
            Some(TesslaCore.BoolLiteral(set.contains(value), loc))
        }
      }

      case object Remove extends PrimitiveOperators.CustomBuiltIn with Monomorphic with Strict {
        override def argumentTypes = Seq(Type, Types.Int)

        override protected def returnType = Type

        override protected def strictEval(typeArgs: Seq[Types.ValueType], values: Seq[TesslaCore.LiteralValue], loc: Location) = values match {
          case Seq(IntSet(set, _), TesslaCore.IntLiteral(key, _)) =>
            Some(IntSet(set - key, loc))
        }
      }
    }

    // TODO: Explicit type name currently not supported
    // override def customTypes = Map("Map" -> TesslaMap.Type, "IntSet" -> IntSet.Type)

    override def customOperators = Map(
      ("map_empty", 0) -> TesslaMap.Empty,
      ("map_add", 3) -> TesslaMap.Add,
      ("map_get", 2) -> TesslaMap.Get,
      ("map_contains", 2) -> TesslaMap.Contains,
      ("map_remove", 2) -> TesslaMap.Remove,
      ("set_empty", 0) -> IntSet.Empty,
      ("set_add", 2) -> IntSet.Add,
      ("set_contains", 2) -> IntSet.Contains,
      ("set_remove", 2) -> IntSet.Remove,
    )
  }
}
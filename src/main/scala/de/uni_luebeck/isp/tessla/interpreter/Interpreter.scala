package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Compiler, NestedLoc, TesslaCore, TesslaSource}
import shapeless.{::, HNil}

class Interpreter(spec: TesslaCore.Specification) extends Specification[BigInt] {
  case class InterpreterError(message: String, loc: NestedLoc) extends RuntimeException

  sealed abstract class Value
  final case class StreamValue(stream: Stream[PrimValue]) extends Value

  sealed abstract class PrimValue extends Value
  final case class IntValue(i: BigInt) extends PrimValue
  final case class BoolValue(b: Boolean) extends PrimValue
  final case object UnitValue extends PrimValue

  val inStreams: Map[String, Input[PrimValue]] = spec.inStreams.map {
    case (name, _) =>
      (name, Input[PrimValue]())
  }.toMap

  lazy val defs: Map[String, Lazy[Value]] = inStreams.mapValues { s =>
    Lazy(StreamValue(s))
  } ++ spec.streams.mapValues(eval)

  def liftBinIntOp(opName: String, op: (BigInt, BigInt) => BigInt, lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = (lhs, rhs) match {
    case (IntValue(l), IntValue(r)) =>
      IntValue(op(l, r))
    case (IntValue(_), r) =>
      throw InterpreterError(s"Invalid type for right operand of $opName: ${r.getClass.getSimpleName}", rhsLoc)
    case (l, _) =>
      throw InterpreterError(s"Invalid type for left operand of $opName: ${l.getClass.getSimpleName}", lhsLoc)
  }

  def add(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinIntOp("+", _ + _, lhs, lhsLoc, rhs, rhsLoc)
  }

  def sub(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinIntOp("-", _ - _, lhs, lhsLoc, rhs, rhsLoc)
  }

  def evalBinOp(op: (PrimValue, NestedLoc, PrimValue, NestedLoc) => PrimValue, lhs: TesslaCore.Expression, rhs: TesslaCore.Expression): Lazy[Value] = {
    (eval(lhs).get, eval(rhs).get) match {
      case (l: PrimValue, r: PrimValue) =>
        Lazy(op(l, lhs.loc, r, rhs.loc))
      case (StreamValue(s), r: PrimValue) =>
        val stream = lift(s :: HNil) {
          (args: PrimValue :: HNil) => args match {
            case l :: HNil => Some(op(l, lhs.loc, r, rhs.loc))
          }
        }
        Lazy(StreamValue(stream))
      case (l: PrimValue, StreamValue(s)) =>
        val stream = lift(s :: HNil) {
          (args: PrimValue :: HNil) => args match {
            case r :: HNil => Some(op(l, lhs.loc, r, rhs.loc))
          }
        }
        Lazy(StreamValue(stream))

      case (StreamValue(s1), StreamValue(s2)) =>
        val stream = lift(s1 :: s2 :: HNil) {
          (args: PrimValue :: PrimValue :: HNil) => args match {
            case l :: r :: HNil =>
              Some(op(l, lhs.loc, r, rhs.loc))
          }
        }
        Lazy(StreamValue(stream))
    }
  }

  def eval(exp: TesslaCore.Expression): Lazy[Value] = exp match {
    case TesslaCore.Var(name, loc) =>
      defs.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc))

    case TesslaCore.Add(lhs, rhs, _) =>
      evalBinOp(add, lhs, rhs)

    case TesslaCore.Sub(lhs, rhs, _) =>
      evalBinOp(sub, lhs, rhs)

    case TesslaCore.BoolLiteral(b, _) => Lazy(BoolValue(b))
    case TesslaCore.IntLiteral(i, _) => Lazy(IntValue(i))
    case TesslaCore.Unit(_) => Lazy(UnitValue)

    case TesslaCore.Default(values, default, loc) =>
      (eval(values).get, eval(default).get) match {
        case (StreamValue(v), StreamValue(d)) =>
          Lazy(StreamValue(v.default(d)))
        case (StreamValue(v), d: PrimValue) =>
          Lazy(StreamValue(v.default(d)))
        case _ =>
          throw InterpreterError("Invalid argument types for default", loc)
      }

    case TesslaCore.Last(values, clock, loc) =>
      (eval(values).get, eval(clock).get) match {
        case (StreamValue(v), StreamValue(d)) =>
          Lazy(StreamValue(v.default(d)))
        case _ =>
          throw InterpreterError("Invalid argument types for last", loc)
      }
  }
}

object Interpreter {
  def fromString(tesslaSource: String) {
    new Compiler().applyPasses(TesslaSource.fromString(tesslaSource)) match {
      case Some(spec: TesslaCore.Specification) =>
        new Interpreter(spec)

      case None =>
        throw new RuntimeException("Failed to compile tessla specification")
    }
  }
}
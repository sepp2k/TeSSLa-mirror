package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Compiler, NestedLoc, TesslaCore, TesslaSource}
import shapeless.{::, HNil}

class Interpreter(val spec: TesslaCore.Specification) extends Specification[BigInt] {
  case class InterpreterError(message: String, loc: NestedLoc) extends RuntimeException(s"Interpreter error at $loc: $message")

  sealed abstract class Value
  final case class StreamValue(stream: Lazy[Stream[Lazy[PrimValue]]]) extends Value

  sealed abstract class PrimValue extends Value
  final case class IntValue(i: Lazy[BigInt]) extends PrimValue
  final case class BoolValue(b: Lazy[Boolean]) extends PrimValue
  final case object UnitValue extends PrimValue

  val inStreams: Map[String, Input[Lazy[PrimValue]]] = spec.inStreams.map {
    case (name, _) =>
      (name, Input[Lazy[PrimValue]]())
  }.toMap

  lazy val defs: Map[String, Lazy[Value]] = spec.streams.mapValues(exp => Lazy(eval(exp)))

  lazy val outStreams: Map[String, Stream[Lazy[PrimValue]]] = spec.outStreams.map {
    case (name, _) => defs(name).get match {
      case StreamValue(s) => name -> s.get
      case value: PrimValue => name -> nil.default(Lazy(value))
    }
  }.toMap

  private def liftBinIntOp(opName: String, op: (=>BigInt, =>BigInt) => Lazy[BigInt], lhs: =>PrimValue, lhsLoc: NestedLoc, rhs: =>PrimValue, rhsLoc: NestedLoc): Lazy[PrimValue] =
    Lazy {
      (lhs, rhs) match {
        case (IntValue(l), IntValue(r)) =>
          IntValue(Lazy(op(l, r)))
        case (IntValue(_), r) =>
          throw InterpreterError(s"Invalid type for right operand of $opName: ${r.getClass.getSimpleName}", rhsLoc)
        case (l, _) =>
          throw InterpreterError(s"Invalid type for left operand of $opName: ${l.getClass.getSimpleName}", lhsLoc)
      }
    }

  private def add(lhs: =>PrimValue, lhsLoc: NestedLoc, rhs: =>PrimValue, rhsLoc: NestedLoc): Lazy[PrimValue] = {
    liftBinIntOp("+", (a,b) => Lazy(a + b), lhs, lhsLoc, rhs, rhsLoc)
  }

  private def sub(lhs: =>PrimValue, lhsLoc: NestedLoc, rhs: =>PrimValue, rhsLoc: NestedLoc): Lazy[PrimValue] = {
    liftBinIntOp("-", (a,b) => Lazy(a - b), lhs, lhsLoc, rhs, rhsLoc)
  }

  private def evalBinOp(op: (=>PrimValue, NestedLoc, =>PrimValue, NestedLoc) => Lazy[PrimValue], lhs: TesslaCore.Expression, rhs: TesslaCore.Expression): Value = {
    lazy val s1 = getStream(eval(lhs), lhs.loc)
    lazy val s2 = getStream(eval(rhs), rhs.loc)
    lazy val stream = lift(s1 :: s2 :: HNil) {
      (args: Lazy[PrimValue] :: Lazy[PrimValue] :: HNil) =>
        args match {
          case l :: r :: HNil =>
            Some(op(l, lhs.loc, r, rhs.loc))
        }
    }
    StreamValue(Lazy(stream))
  }

  private def default(values: Value, defaultValue: Value, loc: NestedLoc): Stream[Lazy[PrimValue]] = {
    (values, defaultValue) match {
      case (StreamValue(v), StreamValue(d)) =>
        v.get.default(d)
      case (StreamValue(v), d: PrimValue) =>
        v.get.default(Lazy(d))
      case (v, d) =>
        throw InterpreterError(s"Invalid arguments for default: $v, $d", loc)
    }
  }

  private def getStream(value: Value, loc: NestedLoc) = value match {
    case StreamValue(s) => s.get
    case other => throw InterpreterError(s"Expected stream, got $other", loc)
  }

  private def eval(exp: TesslaCore.Expression): Value = exp match {
    case TesslaCore.Var(name, loc) =>
      defs.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc)).get

    case TesslaCore.Input(name, loc) =>
      StreamValue(Lazy(inStreams.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc))))

    case TesslaCore.Add(lhs, rhs, _) =>
      evalBinOp(add, lhs, rhs)

    case TesslaCore.Sub(lhs, rhs, _) =>
      evalBinOp(sub, lhs, rhs)

    case TesslaCore.BoolLiteral(b, _) => BoolValue(Lazy(b))
    case TesslaCore.IntLiteral(i, _) => IntValue(Lazy(i))
    case TesslaCore.Unit(_) => UnitValue

    case TesslaCore.Default(values, defaultValue, loc) =>
      StreamValue(Lazy(default(eval(values), eval(defaultValue), loc)))

    case TesslaCore.Last(values, clock, loc) =>
      StreamValue(Lazy(last(getStream(eval(clock), loc), getStream(eval(values), loc))))

    case TesslaCore.Nil(loc) =>
      StreamValue(Lazy(nil))
  }
}

object Interpreter {
  def fromString(tesslaSource: String): Interpreter = {
    new Compiler().applyPasses(TesslaSource.fromString(tesslaSource)) match {
      case Some(spec: TesslaCore.Specification) =>
        new Interpreter(spec)

      case None =>
        throw new RuntimeException("Failed to compile tessla specification")
    }
  }

  def fromFile(file: String): Interpreter = {
    new Compiler().applyPasses(TesslaSource.fromFile(file)) match {
      case Some(spec: TesslaCore.Specification) =>
        new Interpreter(spec)

      case None =>
        throw new RuntimeException("Failed to compile tessla specification")
    }
  }

}
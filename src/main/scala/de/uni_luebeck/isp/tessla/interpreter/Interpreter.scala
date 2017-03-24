package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Compiler, NestedLoc, TesslaCore, TesslaSource}
import shapeless.{::, HNil}

class Interpreter(val spec: TesslaCore.Specification) extends Specification[BigInt] {
  case class InterpreterError(message: String, loc: NestedLoc) extends RuntimeException(s"Interpreter error at $loc: $message")

  sealed abstract class Value
  final case class StreamValue(stream: Stream[PrimValue]) extends Value

  sealed abstract class PrimValue extends Value

  final case class IntValue(i: BigInt) extends PrimValue {
    override def toString: String = i.toString
  }

  final case class BoolValue(b: Boolean) extends PrimValue {
    override def toString: String = b.toString
  }

  final case object UnitValue extends PrimValue {
    override def toString: String = "()"
  }

  val inStreams: Map[String, Input[PrimValue]] = spec.inStreams.map {
    case (name, _) =>
      (name, Input[PrimValue]())
  }.toMap

  lazy val defs: Map[String, Lazy[Value]] = spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp)))
  }

  lazy val outStreams: Map[String, Stream[PrimValue]] = spec.outStreams.map {
    case (name, _) => defs(name).get match {
      case StreamValue(s) => name -> s
      case value: PrimValue => name -> nil.default(value)
    }
  }.toMap

  private def liftBinIntOp(opName: String, op: (BigInt, BigInt) => BigInt, lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue =
    (lhs, rhs) match {
      case (IntValue(l), IntValue(r)) =>
        IntValue(op(l, r))
      case (IntValue(_), r) =>
        throw InterpreterError(s"Invalid type for right operand of $opName: ${r.getClass.getSimpleName}", rhsLoc)
      case (l, _) =>
        throw InterpreterError(s"Invalid type for left operand of $opName: ${l.getClass.getSimpleName}", lhsLoc)
    }

  private def add(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinIntOp("+", _ + _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def sub(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinIntOp("-", _ - _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def mul(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinIntOp("*", _ * _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def liftBinBoolOp(opName: String, op: (Boolean, Boolean) => Boolean, lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue =
    (lhs, rhs) match {
      case (BoolValue(l), BoolValue(r)) =>
        BoolValue(op(l, r))
      case (IntValue(_), r) =>
        throw InterpreterError(s"Invalid type for right operand of $opName: ${r.getClass.getSimpleName}", rhsLoc)
      case (l, _) =>
        throw InterpreterError(s"Invalid type for left operand of $opName: ${l.getClass.getSimpleName}", lhsLoc)
    }

  private def and(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinBoolOp("&&", _ && _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def or(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinBoolOp("||", _ || _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def liftBinCompOp(opName: String, op: (BigInt, BigInt) => Boolean, lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue =
    (lhs, rhs) match {
      case (IntValue(l), IntValue(r)) =>
        BoolValue(op(l, r))
      case (IntValue(_), r) =>
        throw InterpreterError(s"Invalid type for right operand of $opName: ${r.getClass.getSimpleName}", rhsLoc)
      case (l, _) =>
        throw InterpreterError(s"Invalid type for left operand of $opName: ${l.getClass.getSimpleName}", lhsLoc)
    }

  private def lt(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinCompOp("<", _ < _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def gt(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinCompOp(">", _ > _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def lte(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinCompOp("<=", _ <= _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def gte(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinCompOp(">=", _ >= _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def eq(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinCompOp("==", _ == _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def neq(lhs: PrimValue, lhsLoc: NestedLoc, rhs: PrimValue, rhsLoc: NestedLoc): PrimValue = {
    liftBinCompOp("!=", _ != _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def evalBinOp(op: (PrimValue, NestedLoc, PrimValue, NestedLoc) => PrimValue, lhs: TesslaCore.Expression, rhs: TesslaCore.Expression): Value = {
    lazy val s1 = getStream(eval(lhs), lhs.loc)
    lazy val s2 = getStream(eval(rhs), rhs.loc)
    lazy val stream = lift(s1 :: s2 :: HNil) {
      (args: PrimValue :: PrimValue :: HNil) =>
        args match {
          case l :: r :: HNil =>
            Some(op(l, lhs.loc, r, rhs.loc))
        }
    }
    StreamValue(stream)
  }

  private def getStream(value: Value, loc: NestedLoc) = value match {
    case StreamValue(s) => s
    case other => throw InterpreterError(s"Expected stream, got $other", loc)
  }

  private def eval(exp: TesslaCore.Expression): Value = exp match {
    case TesslaCore.Var(name, loc) =>
      defs.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc)).get
    case TesslaCore.Input(name, loc) =>
      StreamValue(inStreams.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc)))
    case TesslaCore.Add(lhs, rhs, _) => evalBinOp(add, lhs, rhs)
    case TesslaCore.Sub(lhs, rhs, _) => evalBinOp(sub, lhs, rhs)
    case TesslaCore.Mul(lhs, rhs, _) => evalBinOp(mul, lhs, rhs)
    case TesslaCore.And(lhs, rhs, _) => evalBinOp(and, lhs, rhs)
    case TesslaCore.Or(lhs, rhs, _) => evalBinOp(or, lhs, rhs)
    case TesslaCore.Not(arg, loc) =>
      StreamValue(boolStreamToValueStream(!boolStream(getStream(eval(arg), loc), loc)))
    case TesslaCore.Lt(lhs, rhs, _) => evalBinOp(lt, lhs, rhs)
    case TesslaCore.Lte(lhs, rhs, _) => evalBinOp(lte, lhs, rhs)
    case TesslaCore.Gt(lhs, rhs, _) => evalBinOp(gt, lhs, rhs)
    case TesslaCore.Gte(lhs, rhs, _) => evalBinOp(gte, lhs, rhs)
    case TesslaCore.Eq(lhs, rhs, _) => evalBinOp(eq, lhs, rhs)
    case TesslaCore.Neq(lhs, rhs, _) => evalBinOp(neq, lhs, rhs)
    case TesslaCore.IfThenElse(cond, thenCase, elseCase, loc) =>
      StreamValue(boolStream(getStream(eval(cond), loc), loc).
        ifThenElse(getStream(eval(thenCase), loc), getStream(eval(elseCase), loc)))
    case TesslaCore.IfThen(cond, thenCase, loc) =>
      StreamValue(boolStream(getStream(eval(cond), loc), loc).
        ifThen(getStream(eval(thenCase), loc)))
    case TesslaCore.BoolLiteral(b, _) => BoolValue(b)
    case TesslaCore.IntLiteral(i, _) => IntValue(i)
    case TesslaCore.Unit(_) => UnitValue
    case TesslaCore.Default(values, defaultValue, loc) =>
      StreamValue(getStream(eval(values), loc).default(eval(defaultValue).asInstanceOf[PrimValue]))
    case TesslaCore.DefaultFrom(values, defaults, loc) =>
      StreamValue(getStream(eval(values), loc).default(getStream(eval(defaults), loc)))
    case TesslaCore.Last(values, clock, loc) =>
      StreamValue(last(getStream(eval(clock), loc), getStream(eval(values), loc)))
    case TesslaCore.DelayedLast(values, delays, loc) =>
      StreamValue(delayedLast(intStream(getStream(eval(delays), loc), loc), getStream(eval(values), loc)))
    case TesslaCore.Const(value, clock, loc) =>
      StreamValue(getStream(eval(clock), loc).const(eval(value).asInstanceOf[PrimValue]))
    case TesslaCore.Nil(loc) => StreamValue(nil)
    case TesslaCore.Time(values, loc) =>
      StreamValue(intStreamToValueStream(getStream(eval(values), loc).time()))
  }

  def intStream(stream: Stream[PrimValue], loc: NestedLoc): Stream[BigInt] = {
    lift(stream :: HNil) {
      (args: PrimValue :: HNil) =>
        args match {
          case IntValue(i) :: HNil => Some(i)
          case value :: HNil => throw InterpreterError(s"Expected it integer value, got: $value", loc)
        }
    }
  }

  def boolStream(stream: Stream[PrimValue], loc: NestedLoc): Stream[Boolean] = {
    lift(stream :: HNil) {
      (args: PrimValue :: HNil) =>
        args match {
          case BoolValue(b) :: HNil => Some(b)
          case value :: HNil => throw InterpreterError(s"Expected it integer value, got: $value", loc)
        }
    }
  }


  def intStreamToValueStream(stream: Stream[BigInt]): Stream[PrimValue] = {
    lift(stream :: HNil) {
      (args: BigInt :: HNil) =>
        args match {
          case i :: HNil => Some(IntValue(i))
        }
    }
  }

  def boolStreamToValueStream(stream: Stream[Boolean]): Stream[PrimValue] = {
    lift(stream :: HNil) {
      (args: Boolean :: HNil) =>
        args match {
          case b :: HNil => Some(BoolValue(b))
        }
    }
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
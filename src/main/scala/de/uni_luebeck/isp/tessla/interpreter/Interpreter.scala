package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Compiler, Location, TesslaCore, TesslaSource}
import shapeless.{::, HNil}

class Interpreter(val spec: TesslaCore.Specification) extends Specification[BigInt] {
  case class InterpreterError(message: String, loc: Location) extends RuntimeException(s"Interpreter error at $loc: $message")

  sealed abstract class Value

  final case class IntValue(i: BigInt) extends Value {
    override def toString: String = i.toString
  }

  final case class StringValue(s: String) extends Value {
    override def toString: String = s
  }

  final case class BoolValue(b: Boolean) extends Value {
    override def toString: String = b.toString
  }

  final case object UnitValue extends Value {
    override def toString: String = "()"
  }

  val inStreams: Map[String, Input[Value]] = spec.inStreams.map {
    case (name, _) =>
      (name, Input[Value]())
  }.toMap

  lazy val defs: Map[String, Lazy[Stream[Value]]] = spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp)))
  }

  lazy val outStreams: Map[String, Stream[Value]] = spec.outStreams.map {
    case (name, streamRef) => name -> defs(streamRef.name).get
  }.toMap

  private def liftBinIntOp(opName: String, op: (BigInt, BigInt) => BigInt, lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value =
    (lhs, rhs) match {
      case (IntValue(l), IntValue(r)) =>
        IntValue(op(l, r))
      case (IntValue(_), r) =>
        throw InterpreterError(s"Invalid type for right operand of $opName: ${r.getClass.getSimpleName}", rhsLoc)
      case (l, _) =>
        throw InterpreterError(s"Invalid type for left operand of $opName: ${l.getClass.getSimpleName}", lhsLoc)
    }

  private def add(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinIntOp("+", _ + _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def sub(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinIntOp("-", _ - _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def mul(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinIntOp("*", _ * _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def bitand(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinIntOp("&", _ & _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def bitor(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinIntOp("|", _ | _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def bitxor(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinIntOp("^", _ ^ _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def leftshift(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinIntOp("<<", _ << _.toInt, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def rightshift(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinIntOp(">>", _ >> _.toInt, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def liftBinBoolOp(opName: String, op: (Boolean, Boolean) => Boolean, lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value =
    (lhs, rhs) match {
      case (BoolValue(l), BoolValue(r)) =>
        BoolValue(op(l, r))
      case (IntValue(_), r) =>
        throw InterpreterError(s"Invalid type for right operand of $opName: ${r.getClass.getSimpleName}", rhsLoc)
      case (l, _) =>
        throw InterpreterError(s"Invalid type for left operand of $opName: ${l.getClass.getSimpleName}", lhsLoc)
    }

  private def and(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinBoolOp("&&", _ && _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def or(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinBoolOp("||", _ || _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def liftBinCompOp(opName: String, op: (BigInt, BigInt) => Boolean, lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value =
    (lhs, rhs) match {
      case (IntValue(l), IntValue(r)) =>
        BoolValue(op(l, r))
      case (IntValue(_), r) =>
        throw InterpreterError(s"Invalid type for right operand of $opName: ${r.getClass.getSimpleName}", rhsLoc)
      case (l, _) =>
        throw InterpreterError(s"Invalid type for left operand of $opName: ${l.getClass.getSimpleName}", lhsLoc)
    }

  private def lt(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinCompOp("<", _ < _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def gt(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinCompOp(">", _ > _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def lte(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinCompOp("<=", _ <= _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def gte(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    liftBinCompOp(">=", _ >= _, lhs, lhsLoc, rhs, rhsLoc)
  }

  private def eq(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    (lhs, rhs) match {
      case (IntValue(l), IntValue(r)) =>
        BoolValue(l == r)
      case (StringValue(l), StringValue(r)) =>
        BoolValue(l == r)
      case (BoolValue(l), BoolValue(r)) =>
        BoolValue(l == r)
      case (UnitValue, UnitValue) =>
        BoolValue(true)
      case (l, r) =>
        throw InterpreterError(s"Invalid type for right operand of ==. Expected: ${l.getClass.getSimpleName}, got: ${r.getClass.getSimpleName}", rhsLoc)
    }
  }

  private def neq(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location): Value = {
    eq(lhs, lhsLoc, rhs, rhsLoc) match {
      case BoolValue(b) => BoolValue(!b)
      case _ => sys.error("Internal error: Unreachable case reached")
    }
  }

  private def evalBinOp(op: (Value, Location, Value, Location) => Value,
                        lhs: TesslaCore.StreamRef,
                        rhs: TesslaCore.StreamRef): Stream[Value] = {
    lift(evalStream(lhs) :: evalStream(rhs) :: HNil) {
      (args: Value :: Value :: HNil) =>
        args match {
          case l :: r :: HNil =>
            Some(op(l, lhs.loc, r, rhs.loc))
        }
    }
  }

  private def evalStream(arg: TesslaCore.StreamRef): Stream[Value] = arg match {
    case TesslaCore.Stream(name, loc) =>
      defs.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc)).get
    case TesslaCore.InputStream(name, loc) =>
      inStreams.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc))
    case TesslaCore.Nil(_) => nil
  }

  private def evalLit(arg: TesslaCore.LiteralValue): Value = arg match {
    case TesslaCore.BoolLiteral(b, _) => BoolValue(b)
    case TesslaCore.IntLiteral(i, _) => IntValue(i)
    case TesslaCore.StringLiteral(s, _) => StringValue(s)
    case TesslaCore.Unit(_) => UnitValue
  }

  private def eval(exp: TesslaCore.Expression): Stream[Value] = exp match {
    case TesslaCore.Add(lhs, rhs, _) => evalBinOp(add, lhs, rhs)
    case TesslaCore.Sub(lhs, rhs, _) => evalBinOp(sub, lhs, rhs)
    case TesslaCore.Mul(lhs, rhs, _) => evalBinOp(mul, lhs, rhs)
    case TesslaCore.BitAnd(lhs, rhs, _) => evalBinOp(bitand, lhs, rhs)
    case TesslaCore.BitOr(lhs, rhs, _) => evalBinOp(bitor, lhs, rhs)
    case TesslaCore.BitXor(lhs, rhs, _) => evalBinOp(bitxor, lhs, rhs)
    case TesslaCore.LeftShift(lhs, rhs, _) => evalBinOp(leftshift, lhs, rhs)
    case TesslaCore.RightShift(lhs, rhs, _) => evalBinOp(rightshift, lhs, rhs)
    case TesslaCore.And(lhs, rhs, _) => evalBinOp(and, lhs, rhs)
    case TesslaCore.Or(lhs, rhs, _) => evalBinOp(or, lhs, rhs)
    case TesslaCore.Not(arg, loc) =>
      boolStreamToValueStream(!boolStream(evalStream(arg), loc))
    case TesslaCore.BitFlip(arg, loc) =>
      lazy val ints = intStream(evalStream(arg), loc)
      lazy val stream = lift(ints :: HNil) {
        (args: BigInt :: HNil) =>
          args match {
            case i :: HNil =>
              Some(~i)
          }
      }
      intStreamToValueStream(stream)
    case TesslaCore.Lt(lhs, rhs, _) => evalBinOp(lt, lhs, rhs)
    case TesslaCore.Lte(lhs, rhs, _) => evalBinOp(lte, lhs, rhs)
    case TesslaCore.Gt(lhs, rhs, _) => evalBinOp(gt, lhs, rhs)
    case TesslaCore.Gte(lhs, rhs, _) => evalBinOp(gte, lhs, rhs)
    case TesslaCore.Eq(lhs, rhs, _) => evalBinOp(eq, lhs, rhs)
    case TesslaCore.Neq(lhs, rhs, _) => evalBinOp(neq, lhs, rhs)
    case TesslaCore.IfThenElse(cond, thenCase, elseCase, loc) =>
      boolStream(evalStream(cond), loc).ifThenElse(evalStream(thenCase), evalStream(elseCase))
    case TesslaCore.IfThen(cond, thenCase, loc) =>
      boolStream(evalStream(cond), loc).ifThen(evalStream(thenCase))
    case TesslaCore.Default(values, defaultValue, _) =>
      evalStream(values).default(evalLit(defaultValue))
    case TesslaCore.DefaultFrom(values, defaults, _) =>
      evalStream(values).default(evalStream(defaults))
    case TesslaCore.Last(values, clock, _) =>
      last(evalStream(clock), evalStream(values))
    case TesslaCore.DelayedLast(values, delays, loc) =>
      delayedLast(intStream(evalStream(delays), loc), evalStream(values))
    case TesslaCore.Const(value, clock, _) =>
      evalStream(clock).const(evalLit(value))
    case TesslaCore.Time(values, _) =>
      intStreamToValueStream(evalStream(values).time())
  }

  def intStream(stream: Stream[Value], loc: Location): Stream[BigInt] = {
    lift(stream :: HNil) {
      (args: Value :: HNil) =>
        args match {
          case IntValue(i) :: HNil => Some(i)
          case value :: HNil => throw InterpreterError(s"Expected it integer value, got: $value", loc)
        }
    }
  }

  def boolStream(stream: Stream[Value], loc: Location): Stream[Boolean] = {
    lift(stream :: HNil) {
      (args: Value :: HNil) =>
        args match {
          case BoolValue(b) :: HNil => Some(b)
          case value :: HNil => throw InterpreterError(s"Expected it integer value, got: $value", loc)
        }
    }
  }


  def intStreamToValueStream(stream: Stream[BigInt]): Stream[Value] = {
    lift(stream :: HNil) {
      (args: BigInt :: HNil) =>
        args match {
          case i :: HNil => Some(IntValue(i))
        }
    }
  }

  def boolStreamToValueStream(stream: Stream[Boolean]): Stream[Value] = {
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
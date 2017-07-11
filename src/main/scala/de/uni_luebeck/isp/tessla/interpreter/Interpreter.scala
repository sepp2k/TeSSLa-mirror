package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Types
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import de.uni_luebeck.isp.tessla.{CompilationError, Compiler, Location, TesslaCore, TesslaSource, TranslationPhase}
import shapeless.{::, HNil}

import scala.io.Source
import Interpreter._

class Interpreter(val spec: TesslaCore.Specification) extends Specification[BigInt] {
  val inStreams: Map[String, (Input[Value], Types.ValueType)] = spec.inStreams.map {
    case (name, typ, _) =>
      name -> (Input[Value](), typ.elementType)
  }.toMap

  lazy val defs: Map[String, Lazy[Stream[Value]]] = inStreams.mapValues {
    case (inputStream, _) => Lazy(inputStream)
  } ++ spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp)))
  }

  lazy val outStreams: Map[String, Stream[Value]] = spec.outStreams.map {
    case (name, streamRef) => name -> defs(streamRef.name).get
  }.toMap

  private def typeError(loc: Location): Nothing = {
    throw InterpreterError(s"Internal error: Invalid types for operation (this is indicative of a bug the type checker)", loc)
  }

  private def liftBinIntOp(opName: String, op: (BigInt, BigInt) => BigInt, lhs: Value, rhs: Value, loc: Location): Value =
    (lhs, rhs) match {
      case (IntValue(l), IntValue(r)) =>
        IntValue(op(l, r))
      case _ =>
        typeError(loc)
    }

  private def add(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinIntOp("+", _ + _, lhs, rhs, expLoc)
  }

  private def sub(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinIntOp("-", _ - _, lhs, rhs, expLoc)
  }

  private def mul(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinIntOp("*", _ * _, lhs, rhs, expLoc)
  }

  private def div(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    (lhs, rhs) match {
      case (IntValue(_), IntValue(r)) if r == 0 =>
        ErrorValue(InterpreterError("Division by zero", rhsLoc))
      case (IntValue(l), IntValue(r)) =>
        IntValue(l / r)
      case _ =>
        typeError(expLoc)
    }
  }

  private def bitand(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinIntOp("&", _ & _, lhs, rhs, expLoc)
  }

  private def bitor(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinIntOp("|", _ | _, lhs, rhs, expLoc)
  }

  private def bitxor(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinIntOp("^", _ ^ _, lhs, rhs, expLoc)
  }

  private def leftshift(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinIntOp("<<", _ << _.toInt, lhs, rhs, expLoc)
  }

  private def rightshift(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinIntOp(">>", _ >> _.toInt, lhs, rhs, expLoc)
  }

  private def liftBinBoolOp(opName: String, op: (Boolean, Boolean) => Boolean, lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value =
    (lhs, rhs) match {
      case (BoolValue(l), BoolValue(r)) =>
        BoolValue(op(l, r))
      case _ =>
        typeError(expLoc)
    }

  private def and(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinBoolOp("&&", _ && _, lhs, lhsLoc, rhs, rhsLoc, expLoc)
  }

  private def or(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinBoolOp("||", _ || _, lhs, lhsLoc, rhs, rhsLoc, expLoc)
  }

  private def liftBinCompOp(opName: String, op: (BigInt, BigInt) => Boolean, lhs: Value, lhsLoc: Location,
                            rhs: Value, rhsLoc: Location, expLoc: Location): Value =
    (lhs, rhs) match {
      case (IntValue(l), IntValue(r)) =>
        BoolValue(op(l, r))
      case _ =>
        typeError(expLoc)
    }

  private def lt(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinCompOp("<", _ < _, lhs, lhsLoc, rhs, rhsLoc, expLoc)
  }

  private def gt(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinCompOp(">", _ > _, lhs, lhsLoc, rhs, rhsLoc, expLoc)
  }

  private def lte(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinCompOp("<=", _ <= _, lhs, lhsLoc, rhs, rhsLoc, expLoc)
  }

  private def gte(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    liftBinCompOp(">=", _ >= _, lhs, lhsLoc, rhs, rhsLoc, expLoc)
  }

  private def eq(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
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

  private def neq(lhs: Value, lhsLoc: Location, rhs: Value, rhsLoc: Location, expLoc: Location): Value = {
    eq(lhs, lhsLoc, rhs, rhsLoc, expLoc) match {
      case BoolValue(b) => BoolValue(!b)
      case _ => sys.error("Internal error: Unreachable case reached")
    }
  }

  private def evalBinOp(op: (Value, Location, Value, Location, Location) => Value,
                        lhs: TesslaCore.StreamRef,
                        rhs: TesslaCore.StreamRef,
                        loc: Location): Stream[Value] = {
    lift(evalStream(lhs) :: evalStream(rhs) :: HNil) {
      (args: Value :: Value :: HNil) =>
        args match {
          case l :: r :: HNil =>
            Some(op(l, lhs.loc, r, rhs.loc, loc))
        }
    }
  }

  private def evalStream(arg: TesslaCore.StreamRef): Stream[Value] = arg match {
    case TesslaCore.Stream(name, loc) =>
      defs.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc)).get
    case TesslaCore.InputStream(name, loc) =>
      inStreams.getOrElse(name, throw InterpreterError(s"Couldn't find stream named $name", loc))._1
    case TesslaCore.Nil(_) => nil
  }

  private def evalLit(arg: TesslaCore.LiteralValue): Value = arg match {
    case TesslaCore.BoolLiteral(b, _) => BoolValue(b)
    case TesslaCore.IntLiteral(i, _) => IntValue(i)
    case TesslaCore.StringLiteral(s, _) => StringValue(s)
    case TesslaCore.Unit(_) => UnitValue
  }

  private def eval(exp: TesslaCore.Expression): Stream[Value] = exp match {
    case TesslaCore.Add(lhs, rhs, loc) => evalBinOp(add, lhs, rhs, loc)
    case TesslaCore.Sub(lhs, rhs, loc) => evalBinOp(sub, lhs, rhs, loc)
    case TesslaCore.Mul(lhs, rhs, loc) => evalBinOp(mul, lhs, rhs, loc)
    case TesslaCore.Div(lhs, rhs, loc) => evalBinOp(div, lhs, rhs, loc)
    case TesslaCore.BitAnd(lhs, rhs, loc) => evalBinOp(bitand, lhs, rhs, loc)
    case TesslaCore.BitOr(lhs, rhs, loc) => evalBinOp(bitor, lhs, rhs, loc)
    case TesslaCore.BitXor(lhs, rhs, loc) => evalBinOp(bitxor, lhs, rhs, loc)
    case TesslaCore.LeftShift(lhs, rhs, loc) => evalBinOp(leftshift, lhs, rhs, loc)
    case TesslaCore.RightShift(lhs, rhs, loc) => evalBinOp(rightshift, lhs, rhs, loc)
    case TesslaCore.And(lhs, rhs, loc) => evalBinOp(and, lhs, rhs, loc)
    case TesslaCore.Or(lhs, rhs, loc) => evalBinOp(or, lhs, rhs, loc)
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
    case TesslaCore.Lt(lhs, rhs, loc) => evalBinOp(lt, lhs, rhs, loc)
    case TesslaCore.Lte(lhs, rhs, loc) => evalBinOp(lte, lhs, rhs, loc)
    case TesslaCore.Gt(lhs, rhs, loc) => evalBinOp(gt, lhs, rhs, loc)
    case TesslaCore.Gte(lhs, rhs, loc) => evalBinOp(gte, lhs, rhs, loc)
    case TesslaCore.Eq(lhs, rhs, loc) => evalBinOp(eq, lhs, rhs, loc)
    case TesslaCore.Neq(lhs, rhs, loc) => evalBinOp(neq, lhs, rhs, loc)
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
          case value :: HNil => throw InterpreterError(s"Expected integer value, got: $value", loc)
        }
    }
  }

  def boolStream(stream: Stream[Value], loc: Location): Stream[Boolean] = {
    lift(stream :: HNil) {
      (args: Value :: HNil) =>
        args match {
          case BoolValue(b) :: HNil => Some(b)
          case value :: HNil => throw InterpreterError(s"Expected Boolean value, got: $value", loc)
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
  sealed abstract class Value {
    def typ: Types.ValueType
  }

  final case class IntValue(i: BigInt) extends Value {
    override def toString: String = i.toString
    override def typ = Types.Int
  }

  final case class StringValue(s: String) extends Value {
    override def toString: String = s""""$s""""
    override def typ = Types.String
  }

  final case class BoolValue(b: Boolean) extends Value {
    override def toString: String = b.toString
    override def typ = Types.Bool
  }

  final case object UnitValue extends Value {
    override def toString: String = "()"
    override def typ = Types.Unit
  }

  final case class ErrorValue(error: InterpreterError) extends Value {
    override def toString: String = throw error
    override def typ = Types.WildCard
  }

  case class InterpreterError(message: String, loc: Location) extends CompilationError

  class CoreToInterpreterSpec extends TranslationPhase[TesslaCore.Specification, Interpreter] {
    def translateSpec(spec: TesslaCore.Specification): Interpreter = new Interpreter(spec)
  }

  def fromSource(source: Source): Result[Interpreter] = {
    new Compiler().applyPasses(new TesslaSource(source)).andThen(new CoreToInterpreterSpec)
  }

  def fromString(tesslaSource: String): Result[Interpreter] = {
    fromSource(Source.fromString(tesslaSource))
  }

  def fromFile(file: String): Result[Interpreter] = {
    fromSource(Source.fromFile(file))
  }

}
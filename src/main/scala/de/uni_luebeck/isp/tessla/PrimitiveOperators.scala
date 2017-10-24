package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.DivideByZero

object PrimitiveOperators {
  def ArityError =
    Errors.InternalError("Arity mismatch should have been caught during overload resolution")

  sealed abstract class PrimitiveOperator {
    /** Return the return type of the result for the given types of the operands, or throw a TypeError if the provided
      * types aren't valid operand types for this operator.
      *
      * @param argTypes The types of the arguments and their locations (used for type error messages)
      */
    def returnTypeFor(argTypes: Seq[(Types.ValueType, Location)]): Types.ValueType

    private def checkTypes(argTypes: Seq[(Types.ValueType, Location)]): Unit = {
      returnTypeFor(argTypes)
    }

    final def eval(args: Seq[TesslaCore.Value], loc: Location): Option[TesslaCore.Value] = {
      checkTypes(args.map(arg => (arg.typ, arg.loc)))
      doEval(args, loc)
    }

    protected def doEval(values: Seq[TesslaCore.Value], loc: Location): Option[TesslaCore.Value]
  }

  abstract class CustomBuiltIn extends PrimitiveOperator

  final case class Const(value: TesslaCore.Value) extends PrimitiveOperator {
    override def returnTypeFor(argTypes: Seq[(Types.ValueType, Location)]) = value.typ
    def doEval(values: Seq[TesslaCore.Value], loc: Location) = Some(value)
  }

  /**
    * Take a variable amount of arguments and return the first one
    */
  case object First extends PrimitiveOperator {
    override def returnTypeFor(argTypes: Seq[(Types.ValueType, Location)]) = {
      require(argTypes.nonEmpty)
      argTypes.head._1
    }

    def doEval(values: Seq[TesslaCore.Value], loc: Location) = Some(values.head)
  }

  trait Monomorphic {
    protected def argumentTypes: Seq[Types.ValueType]
    protected def returnType: Types.ValueType

    private def checkArgumentTypes(argTypes: Seq[(Types.ValueType, Location)]) = {
      require(argTypes.length == argumentTypes.length)
      (argumentTypes, argTypes).zipped.foreach {
        case (required, (actual, loc)) => Types.requireType(required, actual, loc)
      }
    }

    def returnTypeFor(argTypes: Seq[(Types.ValueType, Location)]) = {
      checkArgumentTypes(argTypes)
      returnType
    }
  }

  sealed abstract class PrefixOperator extends PrimitiveOperator

  sealed abstract class InfixOperator extends PrimitiveOperator

  sealed abstract class UnaryIntOperator(op: BigInt => BigInt) extends PrefixOperator with Monomorphic {
    val argumentTypes = Seq(Types.Int)
    val returnType = Types.Int
    def doEval(args: Seq[TesslaCore.Value], loc: Location): Option[TesslaCore.Value] = args match {
      case Seq(TesslaCore.IntLiteral(arg, _)) =>
        Some(TesslaCore.IntLiteral(op(arg), loc))
    }
  }

  sealed abstract class BinaryIntOperator(op: (BigInt, BigInt) => BigInt) extends InfixOperator with Monomorphic {
    val argumentTypes = Seq(Types.Int, Types.Int)
    val returnType = Types.Int
    def doEval(args: Seq[TesslaCore.Value], loc: Location): Some[TesslaCore.Value] = args match {
      case Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, rhsLoc)) =>
        try {
          Some(TesslaCore.IntLiteral(op(lhs, rhs), loc))
        } catch {
          case _: ArithmeticException =>
            // The only way an arithmetic exception can be thrown by the operations we support is by division by zero
            Some(TesslaCore.ErrorValue(DivideByZero(rhsLoc)))
        }
    }
  }

  sealed abstract class IntComparissonOperator(op: (BigInt, BigInt) => Boolean) extends InfixOperator with Monomorphic {
    val argumentTypes = Seq(Types.Int, Types.Int)
    val returnType = Types.Bool
    override def doEval(args: Seq[TesslaCore.Value], loc: Location): Some[TesslaCore.BoolLiteral] = args match {
      case Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)) =>
        Some(TesslaCore.BoolLiteral(op(lhs, rhs), loc))
    }
  }

  sealed abstract class BinaryBoolOperator(op: (Boolean, Boolean) => Boolean) extends InfixOperator with Monomorphic {
    val argumentTypes = Seq(Types.Bool, Types.Bool)
    val returnType = Types.Bool
    def doEval(args: Seq[TesslaCore.Value], loc: Location): Some[TesslaCore.BoolLiteral] = args match {
      case Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)) =>
        Some(TesslaCore.BoolLiteral(op(lhs, rhs), loc))
    }
  }

  sealed abstract class AnyComparissonOperator(op: (Any, Any) => Boolean) extends InfixOperator {
    override def returnTypeFor(argTypes: Seq[(Types.ValueType, Location)]) = argTypes match {
      case Seq((t1, _), (t2, rhsLoc)) =>
        Types.requireType(t1, t2, rhsLoc)
        Types.Bool
      case _ =>
        throw ArityError
    }

    def doEval(args: Seq[TesslaCore.Value], loc: Location): Some[TesslaCore.BoolLiteral] = args match {
      case Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)) =>
        Some(TesslaCore.BoolLiteral(op(lhs, rhs), loc))
      case Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)) =>
        Some(TesslaCore.BoolLiteral(op(lhs, rhs), loc))
      case Seq(TesslaCore.StringLiteral(lhs, _), TesslaCore.StringLiteral(rhs, _)) =>
        Some(TesslaCore.BoolLiteral(op(lhs, rhs), loc))
      case Seq(TesslaCore.Unit(_), TesslaCore.Unit(_)) =>
        Some(TesslaCore.BoolLiteral(op((), ()), loc))
    }
  }

  case object Add extends BinaryIntOperator(_+_) {
    override def toString = "+"
  }

  case object Sub extends BinaryIntOperator(_-_) {
    override def toString = "-"
  }

  case object Mul extends BinaryIntOperator(_*_) {
    override def toString = "*"
  }

  case object Div extends BinaryIntOperator(_/_) {
    override def toString = "/"
  }

  case object BitAnd extends BinaryIntOperator(_&_) {
    override def toString = "&"
  }

  case object BitOr extends BinaryIntOperator(_|_) {
    override def toString = "|"
  }

  case object BitXor extends BinaryIntOperator(_^_) {
    override def toString = "^"
  }

  case object LeftShift extends BinaryIntOperator(_ << _.toInt) {
    override def toString = "<<"
  }

  case object RightShift extends BinaryIntOperator(_ >> _.toInt) {
    override def toString = ">>"
  }

  case object BitFlip extends UnaryIntOperator(~_) {
    override def toString = "~"
  }

  case object Negate extends UnaryIntOperator(-_) {
    override def toString = "-"
  }

  case object Lt extends IntComparissonOperator(_<_) {
    override def toString = "<"
  }

  case object Gt extends IntComparissonOperator(_>_) {
    override def toString = ">"
  }

  case object Lte extends IntComparissonOperator(_<=_) {
    override def toString = "<="
  }

  case object Gte extends IntComparissonOperator(_>=_) {
    override def toString = ">="
  }

  case object Eq extends AnyComparissonOperator(_==_) {
    override def toString = "=="
  }

  case object Neq extends AnyComparissonOperator(_!=_) {
    override def toString = "!="
  }

  case object And extends BinaryBoolOperator(_&&_) {
    override def toString = "&&"
  }

  case object Or extends BinaryBoolOperator(_||_) {
    override def toString = "||"
  }

  case object Not extends PrefixOperator with Monomorphic {
    override def toString = "!"
    val argumentTypes = Seq(Types.Bool)
    val returnType = Types.Bool
    def doEval(args: Seq[TesslaCore.Value], loc: Location): Some[TesslaCore.BoolLiteral] = args match {
      case Seq(TesslaCore.BoolLiteral(arg, _)) =>
        Some(TesslaCore.BoolLiteral(!arg, loc))
    }
  }

  case object IfThenElse extends PrimitiveOperator {
    override def returnTypeFor(argTypes: Seq[(Types.ValueType, Location)]) = argTypes match {
      case Seq((condType, condLoc), (thenType, _), (elseType, elseLoc)) =>
        Types.requireType(Types.Bool, condType, condLoc)
        Types.requireType(thenType, elseType, elseLoc)
      case _ => throw ArityError
    }

    def doEval(args: Seq[TesslaCore.Value], loc: Location): Some[TesslaCore.Value] = args match {
      case Seq(TesslaCore.BoolLiteral(cond, _), thenCase, elseCase) =>
        Some(if (cond) thenCase else elseCase)
    }
  }

  case object IfThen extends PrimitiveOperator {
    override def returnTypeFor(argTypes: Seq[(Types.ValueType, Location)]) = argTypes match {
      case Seq((condType, condLoc), (thenType, _)) =>
        Types.requireType(Types.Bool, condType, condLoc)
        thenType
      case _ => throw ArityError
    }

    def doEval(args: Seq[TesslaCore.Value], loc: Location): Option[TesslaCore.Value] = args match {
      case Seq(TesslaCore.BoolLiteral(cond, _), thenCase) =>
        if (cond) Some(thenCase) else None
    }
  }
}

package de.uni_luebeck.isp.tessla

object PrimitiveOperators {
  def ArityError(loc: Location) =
    AstToCore.InternalError("Arity mismatch should have been caught during overload resolution", loc)

  sealed abstract class PrimitiveOperator {
    /** Return the return type of the result for the given types of the operands, or throw a TypeError if the provided
      * types aren't valid operand types for this operator.
      *
      * @param argTypes The types of the arguments
      * @param loc The location information for the corresponding function call. This will be used when a type
      *            error occurs.
      */
    def returnTypeFor(argTypes: Seq[Types.ValueType], loc: Location): Types.ValueType

    private def checkTypes(argTypes: Seq[Types.ValueType], loc: Location): Unit = {
      returnTypeFor(argTypes, loc)
    }

    final def eval(args: Seq[TesslaCore.LiteralValue], loc: Location): Option[TesslaCore.LiteralValue] = {
      checkTypes(args.map(_.typ), loc)
      doEval(args, loc)
    }

    protected def doEval(values: Seq[TesslaCore.LiteralValue], loc: Location): Option[TesslaCore.LiteralValue]
  }

  final case class Const(value: TesslaCore.LiteralValue) extends PrimitiveOperator {
    override def returnTypeFor(argTypes: Seq[Types.ValueType], loc: Location) = value.typ
    def doEval(values: Seq[TesslaCore.LiteralValue], loc: Location) = Some(value)
  }

  sealed protected trait Monomorphic extends PrimitiveOperator {
    protected def argumentTypes: Seq[Types.ValueType]
    protected def returnType: Types.ValueType

    private def checkArgumentTypes(argTypes: Seq[Types.ValueType], loc: Location) = {
      require(argTypes.length == argumentTypes.length)
      (argumentTypes, argTypes).zipped.foreach(Types.requireType(_, _, loc))
    }

    override def returnTypeFor(argTypes: Seq[Types.ValueType], loc: Location) = {
      checkArgumentTypes(argTypes, loc)
      returnType
    }
  }

  sealed abstract class PrefixOperator extends PrimitiveOperator

  sealed abstract class InfixOperator extends PrimitiveOperator

  sealed abstract class UnaryIntOperator(op: BigInt => BigInt) extends PrefixOperator with Monomorphic {
    val argumentTypes = Seq(Types.Int)
    val returnType = Types.Int
    def doEval(args: Seq[TesslaCore.LiteralValue], loc: Location): Option[TesslaCore.LiteralValue] = args match {
      case Seq(TesslaCore.IntLiteral(arg, _)) =>
        Some(TesslaCore.IntLiteral(op(arg), loc))
    }
  }

  sealed abstract class BinaryIntOperator(op: (BigInt, BigInt) => BigInt) extends InfixOperator with Monomorphic {
    val argumentTypes = Seq(Types.Int, Types.Int)
    val returnType = Types.Int
    def doEval(args: Seq[TesslaCore.LiteralValue], loc: Location): Some[TesslaCore.IntLiteral] = args match {
      case Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)) =>
        try {
          Some(TesslaCore.IntLiteral(op(lhs, rhs), loc))
        } catch {
          case _: ArithmeticException =>
            // The only way an arithmetic exception can be thrown by the operations we support is by division by zero
            throw DivideByZero(loc)
        }
    }
  }

  sealed abstract class IntComparissonOperator(op: (BigInt, BigInt) => Boolean) extends InfixOperator with Monomorphic {
    val argumentTypes = Seq(Types.Int, Types.Int)
    val returnType = Types.Bool
    def doEval(args: Seq[TesslaCore.LiteralValue], loc: Location): Some[TesslaCore.BoolLiteral] = args match {
      case Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)) =>
        Some(TesslaCore.BoolLiteral(op(lhs, rhs), loc))
    }
  }

  sealed abstract class BinaryBoolOperator(op: (Boolean, Boolean) => Boolean) extends InfixOperator with Monomorphic {
    val argumentTypes = Seq(Types.Bool, Types.Bool)
    val returnType = Types.Bool
    def doEval(args: Seq[TesslaCore.LiteralValue], loc: Location): Some[TesslaCore.BoolLiteral] = args match {
      case Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)) =>
        Some(TesslaCore.BoolLiteral(op(lhs, rhs), loc))
    }
  }

  sealed abstract class AnyComparissonOperator(op: (Any, Any) => Boolean) extends InfixOperator {
    override def returnTypeFor(argTypes: Seq[Types.ValueType], loc: Location) = argTypes match {
      case Seq(t1, t2) =>
        Types.requireType(t1, t2, loc)
        Types.Bool
      case _ =>
        throw ArityError(loc)
    }

    def doEval(args: Seq[TesslaCore.LiteralValue], loc: Location): Some[TesslaCore.BoolLiteral] = args match {
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
    val argumentTypes = Seq(Types.Int)
    val returnType = Types.Int
    def doEval(args: Seq[TesslaCore.LiteralValue], loc: Location): Some[TesslaCore.BoolLiteral] = args match {
    val argumentTypes = Seq(Types.Bool)
    val returnType = Types.Bool
      case Seq(TesslaCore.BoolLiteral(arg, _)) =>
        Some(TesslaCore.BoolLiteral(!arg, loc))
    }
  }

  case object IfThenElse extends PrimitiveOperator {
    override def returnTypeFor(argTypes: Seq[Types.ValueType], loc: Location) = argTypes match {
      case Seq(Types.Bool, thenType, elseType) => Types.requireType(thenType, elseType, loc)
      case _ => throw ArityError(loc)
    }

    def doEval(args: Seq[TesslaCore.LiteralValue], loc: Location): Some[TesslaCore.LiteralValue] = args match {
      case Seq(TesslaCore.BoolLiteral(cond, _), thenCase, elseCase) =>
        Some(if (cond) thenCase else elseCase)
    }
  }

  case object IfThen extends PrimitiveOperator {
    override def returnTypeFor(argTypes: Seq[Types.ValueType], loc: Location) = argTypes match {
      case Seq(Types.Bool, thenType) => thenType
      case _ => throw ArityError(loc)
    }

    def doEval(args: Seq[TesslaCore.LiteralValue], loc: Location): Option[TesslaCore.LiteralValue] = args match {
      case Seq(TesslaCore.BoolLiteral(cond, _), thenCase) =>
        if (cond) Some(thenCase) else None
    }
  }

  case class DivideByZero(loc: Location) extends CompilationError {
    def message = "Division by zero"
  }
}

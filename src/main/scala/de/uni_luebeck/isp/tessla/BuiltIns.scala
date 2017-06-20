package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AstToCore.{TranslatedExpression, TypeError}

class BuiltIns private(mkId: String => String) {
  def binaryIntFunction(
                         constantOperation: (BigInt, BigInt, Location) => TesslaCore.LiteralValue,
                         createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
    case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.StreamRef), name, loc) =>
      (Seq(name -> createExpression(lhs, rhs, loc)), TesslaCore.Stream(name, loc))
    case (Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)), _, loc) =>
      (Seq(), constantOperation(lhs, rhs, loc))
    case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.LiteralValue), name, loc) =>
      val liftedRhs = mkId(name)
      (
        Seq(
          liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
          name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
        ),
        TesslaCore.Stream(name, loc)
      )
    case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.StreamRef), name, loc) =>
      val liftedLhs = mkId(name)
      (
        Seq(
          liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
          name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
        ),
        TesslaCore.Stream(name, loc)
      )
    case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.IntLiteral), _, _) =>
      throw TypeError(rhs.getClass.getSimpleName, lhs.getClass.getSimpleName, lhs.loc)
    case (Seq(lhs: TesslaCore.IntLiteral, rhs: TesslaCore.LiteralValue), _, _) =>
      throw TypeError(lhs.getClass.getSimpleName, rhs.getClass.getSimpleName, rhs.loc)
  }

  def binaryBoolFunction(
                          constantOperation: (Boolean, Boolean, Location) => TesslaCore.LiteralValue,
                          createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
    case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.StreamRef), name, loc) =>
      (Seq(name -> createExpression(lhs, rhs, loc)), TesslaCore.Stream(name, loc))
    case (Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)), _, loc) =>
      (Seq(), constantOperation(lhs, rhs, loc))
    case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.LiteralValue), name, loc) =>
      val liftedRhs = mkId(name)
      (
        Seq(
          liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
          name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
        ),
        TesslaCore.Stream(name, loc)
      )
    case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.StreamRef), name, loc) =>
      val liftedLhs = mkId(name)
      (
        Seq(
          liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
          name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
        ),
        TesslaCore.Stream(name, loc)
      )
    case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.BoolLiteral), _, _) =>
      throw TypeError(rhs.getClass.getSimpleName, lhs.getClass.getSimpleName, lhs.loc)
    case (Seq(lhs: TesslaCore.IntLiteral, rhs: TesslaCore.LiteralValue), _, _) =>
      throw TypeError(lhs.getClass.getSimpleName, rhs.getClass.getSimpleName, rhs.loc)
  }

  def binaryFunction(
                      constantOperation: (Any, Any, Location) => TesslaCore.LiteralValue,
                      createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
    case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.StreamRef), name, loc) =>
      (Seq(name -> createExpression(lhs, rhs, loc)), TesslaCore.Stream(name, loc))
    case (Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)), _, loc) =>
      (Seq(), constantOperation(lhs, rhs, loc))
    case (Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)), _, loc) =>
      (Seq(), constantOperation(lhs, rhs, loc))
    case (Seq(TesslaCore.StringLiteral(lhs, _), TesslaCore.StringLiteral(rhs, _)), _, loc) =>
      (Seq(), constantOperation(lhs, rhs, loc))
    case (Seq(TesslaCore.Unit(_), TesslaCore.Unit(_)), _, loc) =>
      (Seq(), constantOperation((), (), loc))
    case (Seq(lhs: TesslaCore.StreamRef, rhs: TesslaCore.LiteralValue), name, loc) =>
      val liftedRhs = mkId(name)
      (
        Seq(
          liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
          name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
        ),
        TesslaCore.Stream(name, loc)
      )
    case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.StreamRef), name, loc) =>
      val liftedLhs = mkId(name)
      (
        Seq(
          liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
          name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
        ),
        TesslaCore.Stream(name, loc)
      )
    case (Seq(lhs: TesslaCore.LiteralValue, rhs: TesslaCore.LiteralValue), _, _) =>
      throw TypeError(lhs.getClass.getSimpleName, rhs.getClass.getSimpleName, rhs.loc)
  }

  def unaryIntFunction(
                        constantOperation: (BigInt, Location) => TesslaCore.LiteralValue,
                        createExpression: (TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
    case (Seq(operand: TesslaCore.StreamRef), name, loc) =>
      (Seq(name -> createExpression(operand, loc)), TesslaCore.Stream(name, loc))
    case (Seq(TesslaCore.IntLiteral(operand, _)), _, loc) =>
      (Seq(), constantOperation(operand, loc))
    case (Seq(operand: TesslaCore.LiteralValue), _, _) =>
      throw TypeError(classOf[TesslaCore.IntLiteral].getSimpleName, operand.getClass.getSimpleName, operand.loc)
  }

  def unaryBoolFunction(
                         constantOperation: (Boolean, Location) => TesslaCore.LiteralValue,
                         createExpression: (TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression = {
    case (Seq(operand: TesslaCore.StreamRef), name, loc) =>
      (Seq(name -> createExpression(operand, loc)), TesslaCore.Stream(name, loc))
    case (Seq(TesslaCore.BoolLiteral(operand, _)), _, loc) =>
      (Seq(), constantOperation(operand, loc))
    case (Seq(operand: TesslaCore.LiteralValue), _, _) =>
      throw TypeError(classOf[TesslaCore.IntLiteral].getSimpleName, operand.getClass.getSimpleName, operand.loc)
  }

  val builtins: Map[(String, Int), (Seq[TesslaCore.Arg], String, Location) => TranslatedExpression] = Map(
    ("nil", 0) -> {
      case (Seq(), _, loc) => (Seq(), TesslaCore.Nil(loc))
    },
    ("default", 2) -> {
      case (Seq(stream: TesslaCore.StreamRef, default: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.DefaultFrom(stream, default, loc)), TesslaCore.Stream(name, loc))
      case (Seq(stream: TesslaCore.StreamRef, default: TesslaCore.LiteralValue), name, loc) =>
        (Seq(name -> TesslaCore.Default(stream, default, loc)), TesslaCore.Stream(name, loc))
      case (Seq(stream, _), _, _) => throw TypeError("stream", "constant value", stream.loc)
    },
    ("const", 2) -> {
      case (Seq(value: TesslaCore.LiteralValue, clock: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.Const(value, clock, loc)), TesslaCore.Stream(name, loc))
      case (Seq(_: TesslaCore.LiteralValue, clock: TesslaCore.LiteralValue), _, _) =>
        throw TypeError("stream", "constant value", clock.loc)
    },
    ("time", 1) -> {
      case (Seq(clock: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.Time(clock, loc)), TesslaCore.Stream(name, loc))
      case (Seq(clock: TesslaCore.LiteralValue), _, _) =>
        throw TypeError("stream", "constant value", clock.loc)
    },
    ("last", 2) -> {
      case (Seq(values: TesslaCore.StreamRef, clock: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.Last(values, clock, loc)), TesslaCore.Stream(name, loc))
      case (Seq(values: TesslaCore.LiteralValue, _), _, _) =>
        throw TypeError("stream", "constant value", values.loc)
      case (Seq(_, clock: TesslaCore.LiteralValue, _), _, _) =>
        throw TypeError("stream", "constant value", clock.loc)
    },
    ("delayedLast", 2) -> {
      case (Seq(values: TesslaCore.StreamRef, delays: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.DelayedLast(values, delays, loc)), TesslaCore.Stream(name, loc))
      case (Seq(values: TesslaCore.LiteralValue, _), _, _) =>
        throw TypeError("stream", "constant value", values.loc)
      case (Seq(_, delays: TesslaCore.LiteralValue, _), _, _) =>
        throw TypeError("stream", "constant value", delays.loc)
    },
    ("+", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a+b, loc), TesslaCore.Add),
    ("-", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a-b, loc), TesslaCore.Sub),
    ("*", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a*b, loc), TesslaCore.Mul),
    ("&", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a&b, loc), TesslaCore.BitAnd),
    ("|", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a|b, loc), TesslaCore.BitOr),
    ("^", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a^b, loc), TesslaCore.BitXor),
    ("<<", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a << b.toInt, loc), TesslaCore.LeftShift),
    (">>", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a >> b.toInt, loc), TesslaCore.RightShift),
    ("~", 1) -> unaryIntFunction((a,loc) => TesslaCore.IntLiteral(~a, loc), TesslaCore.BitFlip),
    ("<", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.BoolLiteral(a<b, loc), TesslaCore.Lt),
    (">", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.BoolLiteral(a>b, loc), TesslaCore.Gt),
    ("<=", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.BoolLiteral(a<=b, loc), TesslaCore.Lte),
    (">=", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.BoolLiteral(a>=b, loc), TesslaCore.Gte),
    ("==", 2) -> binaryFunction((a,b,loc) => TesslaCore.BoolLiteral(a==b, loc), TesslaCore.Eq),
    ("!=", 2) -> binaryFunction((a,b,loc) => TesslaCore.BoolLiteral(a!=b, loc), TesslaCore.Neq),
    ("&&", 2) -> binaryBoolFunction((a,b,loc) => TesslaCore.BoolLiteral(a&&b, loc), TesslaCore.And),
    ("||", 2) -> binaryBoolFunction((a,b,loc) => TesslaCore.BoolLiteral(a||b, loc), TesslaCore.Or),
    ("!", 1) -> unaryBoolFunction((a,loc) => TesslaCore.BoolLiteral(!a, loc), TesslaCore.Not),
    ("if then else", 3) -> {
      case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.StreamRef, elseCase: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.IfThenElse(condition, thenCase, elseCase, loc)), TesslaCore.Stream(name, loc))
      case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.LiteralValue, elseCase: TesslaCore.StreamRef), name, loc) =>
        val liftedThenCase = mkId(name)
        (
          Seq(
            liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase, loc),
            name -> TesslaCore.IfThenElse(condition, TesslaCore.Stream(liftedThenCase, loc), elseCase, loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.StreamRef, elseCase: TesslaCore.LiteralValue), name, loc) =>
        val liftedElseCase = mkId(name)
        (
          Seq(
            liftedElseCase -> TesslaCore.Default(TesslaCore.Nil(loc), elseCase, loc),
            name -> TesslaCore.IfThenElse(condition, thenCase, TesslaCore.Stream(liftedElseCase, loc), loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(TesslaCore.BoolLiteral(true, _), thenCase: TesslaCore.Arg, _: TesslaCore.Arg), _, _) =>
        (Seq(), thenCase)
      case (Seq(TesslaCore.BoolLiteral(false, _), _: TesslaCore.Arg, elseCase: TesslaCore.Arg), _, _) =>
        (Seq(), elseCase)
      case (Seq(condition: TesslaCore.LiteralValue, _: TesslaCore.Arg, _: TesslaCore.Arg), _, _) =>
        throw TypeError(classOf[TesslaCore.BoolLiteral].getSimpleName, condition.getClass.getSimpleName, condition.loc)
    },
    ("if then", 2) -> {
      case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.StreamRef), name, loc) =>
        (Seq(name -> TesslaCore.IfThen(condition, thenCase, loc)), TesslaCore.Stream(name, loc))
      case (Seq(condition: TesslaCore.StreamRef, thenCase: TesslaCore.LiteralValue), name, loc) =>
        val liftedThenCase = mkId(name)
        (
          Seq(
            liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase, loc),
            name -> TesslaCore.IfThen(condition, TesslaCore.Stream(liftedThenCase, loc), loc)
          ),
          TesslaCore.Stream(name, loc)
        )
      case (Seq(TesslaCore.BoolLiteral(true, _), thenCase: TesslaCore.Arg), _, _) =>
        (Seq(), thenCase)
      case (Seq(TesslaCore.BoolLiteral(false, _), _: TesslaCore.Arg), _, loc) =>
        (Seq(), TesslaCore.Nil(loc))
    }
  )
}

object BuiltIns {
  def apply(mkId: String => String) = {
    new BuiltIns(mkId).builtins
  }
}
package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AstToCore.{TranslatedExpression, TypeError, Arg, Stream, Literal}

class BuiltIns private(mkId: String => String) {
  def binaryIntFunction(
                         constantOperation: (BigInt, BigInt, Location) => TesslaCore.LiteralValue,
                         createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    case (Seq(Stream(lhs), Stream(rhs)), name, loc) =>
      (Seq(name -> createExpression(lhs, rhs, loc)), Stream(TesslaCore.Stream(name, loc)))
    case (Seq(Literal(TesslaCore.IntLiteral(lhs, _)), Literal(TesslaCore.IntLiteral(rhs, _))), _, loc) =>
      (Seq(), Literal(constantOperation(lhs, rhs, loc)))
    case (Seq(Stream(lhs), Literal(rhs)), name, loc) =>
      val liftedRhs = mkId(name)
      (
        Seq(
          liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
          name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
        ),
        Stream(TesslaCore.Stream(name, loc))
      )
    case (Seq(Literal(lhs), Stream(rhs)), name, loc) =>
      val liftedLhs = mkId(name)
      (
        Seq(
          liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
          name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
        ),
        Stream(TesslaCore.Stream(name, loc))
      )
    case (Seq(Literal(TesslaCore.IntLiteral(_, _)), Literal(rhs)), _, _) =>
      throw TypeError("IntLiteral", rhs.getClass.getSimpleName, rhs.loc)
    case (Seq(Literal(lhs), Literal(_)), _, _) =>
      throw TypeError("IntLiteral", lhs.getClass.getSimpleName, lhs.loc)
  }

  def binaryBoolFunction(
                          constantOperation: (Boolean, Boolean, Location) => TesslaCore.LiteralValue,
                          createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    case (Seq(Stream(lhs), Stream(rhs)), name, loc) =>
      (Seq(name -> createExpression(lhs, rhs, loc)), Stream(TesslaCore.Stream(name, loc)))
    case (Seq(Literal(TesslaCore.BoolLiteral(lhs, _)), Literal(TesslaCore.BoolLiteral(rhs, _))), _, loc) =>
      (Seq(), Literal(constantOperation(lhs, rhs, loc)))
    case (Seq(Stream(lhs), Literal(rhs)), name, loc) =>
      val liftedRhs = mkId(name)
      (
        Seq(
          liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
          name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
        ),
        Stream(TesslaCore.Stream(name, loc))
      )
    case (Seq(Literal(lhs), Stream(rhs)), name, loc) =>
      val liftedLhs = mkId(name)
      (
        Seq(
          liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
          name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
        ),
        Stream(TesslaCore.Stream(name, loc))
      )
    case (Seq(Literal(TesslaCore.BoolLiteral(_, _)), Literal(rhs)), _, _) =>
      throw TypeError("BoolLiteral", rhs.getClass.getSimpleName, rhs.loc)
    case (Seq(Literal(lhs), Literal(_)), _, _) =>
      throw TypeError("BoolLiteral", lhs.getClass.getSimpleName, lhs.loc)
  }

  def binaryFunction(
                      constantOperation: (Any, Any, Location) => TesslaCore.LiteralValue,
                      createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    case (Seq(Stream(lhs), Stream(rhs)), name, loc) =>
      (Seq(name -> createExpression(lhs, rhs, loc)), Stream(TesslaCore.Stream(name, loc)))
    case (Seq(Literal(TesslaCore.IntLiteral(lhs, _)), Literal(TesslaCore.IntLiteral(rhs, _))), _, loc) =>
      (Seq(), Literal(constantOperation(lhs, rhs, loc)))
    case (Seq(Literal(TesslaCore.BoolLiteral(lhs, _)), Literal(TesslaCore.BoolLiteral(rhs, _))), _, loc) =>
      (Seq(), Literal(constantOperation(lhs, rhs, loc)))
    case (Seq(Literal(TesslaCore.StringLiteral(lhs, _)), Literal(TesslaCore.StringLiteral(rhs, _))), _, loc) =>
      (Seq(), Literal(constantOperation(lhs, rhs, loc)))
    case (Seq(Literal(TesslaCore.Unit(_)), Literal(TesslaCore.Unit(_))), _, loc) =>
      (Seq(), Literal(constantOperation((), (), loc)))
    case (Seq(Stream(lhs), Literal(rhs)), name, loc) =>
      val liftedRhs = mkId(name)
      (
        Seq(
          liftedRhs -> TesslaCore.Default(TesslaCore.Nil(loc), rhs, loc),
          name -> createExpression(lhs, TesslaCore.Stream(liftedRhs, loc), loc)
        ),
        Stream(TesslaCore.Stream(name, loc))
      )
    case (Seq(Literal(lhs), Stream(rhs)), name, loc) =>
      val liftedLhs = mkId(name)
      (
        Seq(
          liftedLhs -> TesslaCore.Default(TesslaCore.Nil(loc), lhs, loc),
          name -> createExpression(TesslaCore.Stream(liftedLhs, loc), rhs, loc)
        ),
        Stream(TesslaCore.Stream(name, loc))
      )
    case (Seq(Literal(lhs), Literal(rhs)), _, _) =>
      throw TypeError(lhs.getClass.getSimpleName, rhs.getClass.getSimpleName, rhs.loc)
  }

  def unaryIntFunction(
                        constantOperation: (BigInt, Location) => TesslaCore.LiteralValue,
                        createExpression: (TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    case (Seq(Stream(operand)), name, loc) =>
      (Seq(name -> createExpression(operand, loc)), Stream(TesslaCore.Stream(name, loc)))
    case (Seq(Literal(TesslaCore.IntLiteral(operand, _))), _, loc) =>
      (Seq(), Literal(constantOperation(operand, loc)))
    case (Seq(Literal(operand)), _, _) =>
      throw TypeError("IntLiteral", operand.getClass.getSimpleName, operand.loc)
  }

  def unaryBoolFunction(
                         constantOperation: (Boolean, Location) => TesslaCore.LiteralValue,
                         createExpression: (TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    case (Seq(Stream(operand)), name, loc) =>
      (Seq(name -> createExpression(operand, loc)), Stream(TesslaCore.Stream(name, loc)))
    case (Seq(Literal(TesslaCore.BoolLiteral(operand, _))), _, loc) =>
      (Seq(), Literal(constantOperation(operand, loc)))
    case (Seq(Literal(operand)), _, _) =>
      throw TypeError("BoolLiteral", operand.getClass.getSimpleName, operand.loc)
  }

  val builtins: Map[(String, Int), (Seq[Arg], String, Location) => TranslatedExpression] = Map(
    ("nil", 0) -> {
      case (Seq(), _, loc) => (Seq(), Stream(TesslaCore.Nil(loc)))
    },
    ("default", 2) -> {
      case (Seq(Stream(stream), Stream(default)), name, loc) =>
        (Seq(name -> TesslaCore.DefaultFrom(stream, default, loc)), Stream(TesslaCore.Stream(name, loc)))
      case (Seq(Stream(stream), Literal(default)), name, loc) =>
        (Seq(name -> TesslaCore.Default(stream, default, loc)), Stream(TesslaCore.Stream(name, loc)))
      case (Seq(Literal(lit), _), _, _) => throw TypeError("stream", "constant value", lit.loc)
    },
    ("const", 2) -> {
      case (Seq(Literal(value), Stream(clock)), name, loc) =>
        (Seq(name -> TesslaCore.Const(value, clock, loc)), Stream(TesslaCore.Stream(name, loc)))
      case (Seq(Literal(_), Literal(clock)), _, _) =>
        throw TypeError("stream", "constant value", clock.loc)
      case (Seq(Stream(stream), _), _, _) => throw TypeError("stream", "constant value", stream.loc)
    },
    ("time", 1) -> {
      case (Seq(Stream(clock)), name, loc) =>
        (Seq(name -> TesslaCore.Time(clock, loc)), Stream(TesslaCore.Stream(name, loc)))
      case (Seq(Literal(clock)), _, _) =>
        throw TypeError("stream", "constant value", clock.loc)
    },
    ("last", 2) -> {
      case (Seq(Stream(values), Stream(clock)), name, loc) =>
        (Seq(name -> TesslaCore.Last(values, clock, loc)), Stream(TesslaCore.Stream(name, loc)))
      case (Seq(Literal(values), _), _, _) =>
        throw TypeError("stream", "constant value", values.loc)
      case (Seq(_, Literal(clock), _), _, _) =>
        throw TypeError("stream", "constant value", clock.loc)
    },
    ("delayedLast", 2) -> {
      case (Seq(Stream(values), Stream(delays)), name, loc) =>
        (Seq(name -> TesslaCore.DelayedLast(values, delays, loc)), Stream(TesslaCore.Stream(name, loc)))
      case (Seq(Literal(values), _), _, _) =>
        throw TypeError("stream", "constant value", values.loc)
      case (Seq(_, Literal(delays), _), _, _) =>
        throw TypeError("stream", "constant value", delays.loc)
    },
    ("+", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a+b, loc), TesslaCore.Add),
    ("-", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a-b, loc), TesslaCore.Sub),
    ("*", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a*b, loc), TesslaCore.Mul),
    ("/", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a/b, loc), TesslaCore.Div),
    ("&", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a&b, loc), TesslaCore.BitAnd),
    ("|", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a|b, loc), TesslaCore.BitOr),
    ("^", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a^b, loc), TesslaCore.BitXor),
    ("<<", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a << b.toInt, loc), TesslaCore.LeftShift),
    (">>", 2) -> binaryIntFunction((a,b,loc) => TesslaCore.IntLiteral(a >> b.toInt, loc), TesslaCore.RightShift),
    ("~", 1) -> unaryIntFunction((a,loc) => TesslaCore.IntLiteral(~a, loc), TesslaCore.BitFlip),
    ("-", 1) -> {
      case (Seq(Stream(operand)), name, loc) =>
        val zero = mkId(name)
        (
          Seq(
            zero -> TesslaCore.Default(TesslaCore.Nil(loc), TesslaCore.IntLiteral(0, loc), loc),
            name -> TesslaCore.Sub(TesslaCore.Stream(zero, loc), operand, loc)
          ),
          Stream(TesslaCore.Stream(name, loc))
        )
      case (Seq(Literal(TesslaCore.IntLiteral(operand, _))), _, loc) =>
        (Seq(), Literal(TesslaCore.IntLiteral(-operand, loc)))
      case (Seq(Literal(operand)), _, _) =>
        throw TypeError(classOf[TesslaCore.IntLiteral].getSimpleName, operand.getClass.getSimpleName, operand.loc)
    },
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
      case (Seq(Stream(condition), Stream(thenCase), Stream(elseCase)), name, loc) =>
        (Seq(name -> TesslaCore.IfThenElse(condition, thenCase, elseCase, loc)), Stream(TesslaCore.Stream(name, loc)))
      case (Seq(Stream(condition), Literal(thenCase), Stream(elseCase)), name, loc) =>
        val liftedThenCase = mkId(name)
        (
          Seq(
            liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase, loc),
            name -> TesslaCore.IfThenElse(condition, TesslaCore.Stream(liftedThenCase, loc), elseCase, loc)
          ),
          Stream(TesslaCore.Stream(name, loc))
        )
      case (Seq(Stream(condition), Stream(thenCase), Literal(elseCase)), name, loc) =>
        val liftedElseCase = mkId(name)
        (
          Seq(
            liftedElseCase -> TesslaCore.Default(TesslaCore.Nil(loc), elseCase, loc),
            name -> TesslaCore.IfThenElse(condition, thenCase, TesslaCore.Stream(liftedElseCase, loc), loc)
          ),
          Stream(TesslaCore.Stream(name, loc))
        )
      case (Seq(Stream(condition), Literal(thenCase), Literal(elseCase)), name, loc) =>
        val liftedThenCase = mkId(name)
        val liftedElseCase = mkId(name)
        (
          Seq(
            liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase, loc),
            liftedElseCase -> TesslaCore.Default(TesslaCore.Nil(loc), elseCase, loc),
            name -> TesslaCore.IfThenElse(condition, TesslaCore.Stream(liftedThenCase, loc), TesslaCore.Stream(liftedElseCase, loc), loc)
          ),
          Stream(TesslaCore.Stream(name, loc))
        )
      case (Seq(Literal(TesslaCore.BoolLiteral(true, _)), thenCase, _), _, _) =>
        (Seq(), thenCase)
      case (Seq(Literal(TesslaCore.BoolLiteral(false, _)), _, elseCase), _, _) =>
        (Seq(), elseCase)
      case (Seq(Literal(condition), _, _), _, _) =>
        throw TypeError("BoolLiteral", condition.getClass.getSimpleName, condition.loc)
    },
    ("if then", 2) -> {
      case (Seq(Stream(condition), Stream(thenCase)), name, loc) =>
        (Seq(name -> TesslaCore.IfThen(condition, thenCase, loc)), Stream(TesslaCore.Stream(name, loc)))
      case (Seq(Stream(condition), Literal(thenCase)), name, loc) =>
        val liftedThenCase = mkId(name)
        (
          Seq(
            liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase, loc),
            name -> TesslaCore.IfThen(condition, TesslaCore.Stream(liftedThenCase, loc), loc)
          ),
          Stream(TesslaCore.Stream(name, loc))
        )
      case (Seq(Literal(TesslaCore.BoolLiteral(true, _)), thenCase), _, _) =>
        (Seq(), thenCase)
      case (Seq(Literal(TesslaCore.BoolLiteral(false, _)), _), _, loc) =>
        (Seq(), Stream(TesslaCore.Nil(loc)))
    }
  )
}

object BuiltIns {
  def apply(mkId: String => String) = {
    new BuiltIns(mkId).builtins
  }
}
package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AstToCore.{TranslatedExpression, Arg, Stream, Literal}
import Types.TypeMismatch

class BuiltIns private(mkId: String => String) {
  type ConstOpT = (Seq[TesslaCore.LiteralValue], Location) => TesslaCore.LiteralValue
  type CreateExpT = (Seq[TesslaCore.StreamRef], Location) => TesslaCore.Expression
  def liftableFunction(
                        resultType: Types.ValueType, operandTypes: Seq[Types.ValueType],
                        constantOperation: ConstOpT,
                        createExpression: CreateExpT):
  (Seq[Arg], String, Location) => TranslatedExpression = { (args, name, loc) =>
    val literals = args.collect {
      case lit @ Literal(_) => lit
    }
    if (literals.length == args.length) {
      literals.zip(operandTypes).foreach {
        case (arg, typ) => Types.requireType(typ, arg.typ, arg.loc)
      }
      (Seq(), Literal(constantOperation(literals.map(_.value), loc)))
    } else {
      val streams = args.zip(operandTypes).map {
        case (s @ Stream(_, _), desiredType) =>
          Types.requireType(Types.Stream(desiredType), s.typ, s.loc)
          (Seq(), s)
        case (l @ Literal(_), desiredType) =>
          Types.requireType(desiredType, l.typ, l.loc)
          val liftedName = mkId(name)
          (Seq(liftedName -> TesslaCore.Default(TesslaCore.Nil(l.loc), l.value, l.loc)),
            Stream(TesslaCore.Stream(liftedName, l.loc), resultType))
      }
      (streams.flatMap(_._1) :+ (name -> createExpression(streams.map(_._2.value), loc)),
        Stream(TesslaCore.Stream(name, loc), resultType))
    }
  }

  def binaryIntFunction(
                         resultType: Types.ValueType,
                         constantOperation: (BigInt, BigInt, Location) => TesslaCore.LiteralValue,
                         createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    val constOp: ConstOpT = {
      case (Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)), loc) =>
        constantOperation(lhs, rhs, loc)
    }
    val createExp: CreateExpT = {
      case (Seq(lhs, rhs), loc) =>
        createExpression(lhs, rhs, loc)
    }
    liftableFunction(resultType, Seq(Types.Int, Types.Int), constOp, createExp)
  }

  def binaryBoolFunction(
                          constantOperation: (Boolean, Boolean, Location) => TesslaCore.LiteralValue,
                          createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    val constOp: ConstOpT = {
      case (Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)), loc) =>
        constantOperation(lhs, rhs, loc)
    }
    val createExp: CreateExpT = {
      case (Seq(lhs, rhs), loc) =>
        createExpression(lhs, rhs, loc)
    }
    liftableFunction(Types.Bool, Seq(Types.Bool, Types.Bool), constOp, createExp)
  }

  def binaryFunction(
                      constantOperation: (Any, Any, Location) => TesslaCore.LiteralValue,
                      createExpression: (TesslaCore.StreamRef, TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    val constOp: ConstOpT = {
      case (Seq(TesslaCore.BoolLiteral(lhs, _), TesslaCore.BoolLiteral(rhs, _)), loc) =>
        constantOperation(lhs, rhs, loc)
      case (Seq(TesslaCore.IntLiteral(lhs, _), TesslaCore.IntLiteral(rhs, _)), loc) =>
        constantOperation(lhs, rhs, loc)
      case (Seq(TesslaCore.StringLiteral(lhs, _), TesslaCore.StringLiteral(rhs, _)), loc) =>
        constantOperation(lhs, rhs, loc)
      case (Seq(TesslaCore.Unit(_), TesslaCore.Unit(_)), loc) =>
        constantOperation((), (), loc)
      case (Seq(lhs, rhs), _) =>
        throw TypeMismatch(Literal(lhs).typ, Literal(rhs).typ, rhs.loc)
    }
    val createExp: CreateExpT = {
      case (Seq(lhs, rhs), loc) =>
        createExpression(lhs, rhs, loc)
    }
    liftableFunction(Types.Bool, Seq(Types.WildCard, Types.WildCard), constOp, createExp)
  }

  def unaryIntFunction(
                        constantOperation: (BigInt, Location) => TesslaCore.LiteralValue,
                        createExpression: (TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    val constOp: ConstOpT = {
      case (Seq(TesslaCore.IntLiteral(operand, _)), loc) =>
        constantOperation(operand, loc)
    }
    val createExp: CreateExpT = {
      case (Seq(operand), loc) =>
        createExpression(operand, loc)
    }
    liftableFunction(Types.Int, Seq(Types.Int, Types.Int), constOp, createExp)
  }

  def unaryBoolFunction(
                         constantOperation: (Boolean, Location) => TesslaCore.LiteralValue,
                         createExpression: (TesslaCore.StreamRef, Location) => TesslaCore.Expression):
  (Seq[Arg], String, Location) => TranslatedExpression = {
    val constOp: ConstOpT = {
      case (Seq(TesslaCore.BoolLiteral(operand, _)), loc) =>
        constantOperation(operand, loc)
    }
    val createExp: CreateExpT = {
      case (Seq(operand), loc) =>
        createExpression(operand, loc)
    }
    liftableFunction(Types.Bool, Seq(Types.Bool, Types.Bool), constOp, createExp)
  }

  val builtins: Map[(String, Int), (Seq[Arg], String, Location) => TranslatedExpression] = Map(
    ("nil", 0) -> {
      case (Seq(), _, loc) => (Seq(), Stream(TesslaCore.Nil(loc), Types.WildCard))
    },
    ("default", 2) -> {
      case (Seq(Stream(stream, t1), Stream(default, t2)), name, loc) =>
        Types.requireType(Types.Stream(t1), Types.Stream(t2), default.loc)
        (Seq(name -> TesslaCore.DefaultFrom(stream, default, loc)), Stream(TesslaCore.Stream(name, loc), t1))
      case (Seq(Stream(stream, t), default @ Literal(_)), name, loc) =>
        Types.requireType(t, default.typ, default.loc)
        (Seq(name -> TesslaCore.Default(stream, default.value, loc)), Stream(TesslaCore.Stream(name, loc), t))
      case (Seq(lit @ Literal(_), _), _, _) => throw TypeMismatch(Types.Stream(Types.WildCard), lit.typ, lit.loc)
    },
    ("const", 2) -> {
      case (Seq(lit @ Literal(value), Stream(clock, _)), name, loc) =>
        (Seq(name -> TesslaCore.Const(value, clock, loc)), Stream(TesslaCore.Stream(name, loc), lit.typ))
      case (Seq(Literal(_), clock @ Literal(_)), _, _) =>
        throw TypeMismatch(Types.Stream(Types.WildCard), clock.typ, clock.loc)
      case (Seq(stream @ Stream(_, _), _), _, _) => throw TypeMismatch(Types.WildCard, stream.typ, stream.loc)
    },
    ("time", 1) -> {
      case (Seq(Stream(clock, _)), name, loc) =>
        (Seq(name -> TesslaCore.Time(clock, loc)), Stream(TesslaCore.Stream(name, loc), Types.Int))
      case (Seq(clock @ Literal(_)), _, _) =>
        throw TypeMismatch(Types.Stream(Types.WildCard), clock.typ, clock.loc)
    },
    ("last", 2) -> {
      case (Seq(Stream(values, t), Stream(clock, _)), name, loc) =>
        (Seq(name -> TesslaCore.Last(values, clock, loc)), Stream(TesslaCore.Stream(name, loc), t))
      case (Seq(values @ Literal(_), _), _, _) =>
        throw TypeMismatch(Types.Stream(Types.WildCard), values.typ, values.loc)
      case (Seq(_, clock @ Literal(_), _), _, _) =>
        throw TypeMismatch(Types.Stream(Types.WildCard), clock.typ, clock.loc)
    },
    ("delayedLast", 2) -> {
      case (Seq(Stream(values, valueType), Stream(delays, delayType)), name, loc) =>
        Types.requireType(Types.Stream(Types.Int), Types.Stream(delayType), delays.loc)
        (Seq(name -> TesslaCore.DelayedLast(values, delays, loc)), Stream(TesslaCore.Stream(name, loc), valueType))
      case (Seq(values @ Literal(_), _), _, _) =>
        throw TypeMismatch(Types.Stream(Types.WildCard), values.typ, values.loc)
      case (Seq(_, delays @ Literal(_), _), _, _) =>
        throw TypeMismatch(Types.Stream(Types.WildCard), delays.typ, delays.loc)
    },
    ("+", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a+b, loc), TesslaCore.Add),
    ("-", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a-b, loc), TesslaCore.Sub),
    ("*", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a*b, loc), TesslaCore.Mul),
    ("/", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a/b, loc), TesslaCore.Div),
    ("&", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a&b, loc), TesslaCore.BitAnd),
    ("|", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a|b, loc), TesslaCore.BitOr),
    ("^", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a^b, loc), TesslaCore.BitXor),
    ("<<", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a << b.toInt, loc), TesslaCore.LeftShift),
    (">>", 2) -> binaryIntFunction(Types.Int, (a,b,loc) => TesslaCore.IntLiteral(a >> b.toInt, loc), TesslaCore.RightShift),
    ("~", 1) -> unaryIntFunction((a,loc) => TesslaCore.IntLiteral(~a, loc), TesslaCore.BitFlip),
    ("-", 1) -> {
      case (Seq(Stream(operand, t)), name, loc) =>
        Types.requireType(Types.Int, t, operand.loc)
        val zero = mkId(name)
        (
          Seq(
            zero -> TesslaCore.Default(TesslaCore.Nil(loc), TesslaCore.IntLiteral(0, loc), loc),
            name -> TesslaCore.Sub(TesslaCore.Stream(zero, loc), operand, loc)
          ),
          Stream(TesslaCore.Stream(name, loc), t)
        )
      case (Seq(Literal(TesslaCore.IntLiteral(operand, _))), _, loc) =>
        (Seq(), Literal(TesslaCore.IntLiteral(-operand, loc)))
      case (Seq(operand @ Literal(_)), _, _) =>
        throw TypeMismatch(Types.Int, operand.typ, operand.loc)
    },
    ("<", 2) -> binaryIntFunction(Types.Bool, (a,b,loc) => TesslaCore.BoolLiteral(a<b, loc), TesslaCore.Lt),
    (">", 2) -> binaryIntFunction(Types.Bool, (a,b,loc) => TesslaCore.BoolLiteral(a>b, loc), TesslaCore.Gt),
    ("<=", 2) -> binaryIntFunction(Types.Bool, (a,b,loc) => TesslaCore.BoolLiteral(a<=b, loc), TesslaCore.Lte),
    (">=", 2) -> binaryIntFunction(Types.Bool, (a,b,loc) => TesslaCore.BoolLiteral(a>=b, loc), TesslaCore.Gte),
    ("==", 2) -> binaryFunction((a,b,loc) => TesslaCore.BoolLiteral(a==b, loc), TesslaCore.Eq),
    ("!=", 2) -> binaryFunction((a,b,loc) => TesslaCore.BoolLiteral(a!=b, loc), TesslaCore.Neq),
    ("&&", 2) -> binaryBoolFunction((a,b,loc) => TesslaCore.BoolLiteral(a&&b, loc), TesslaCore.And),
    ("||", 2) -> binaryBoolFunction((a,b,loc) => TesslaCore.BoolLiteral(a||b, loc), TesslaCore.Or),
    ("!", 1) -> unaryBoolFunction((a,loc) => TesslaCore.BoolLiteral(!a, loc), TesslaCore.Not),
    ("if then else", 3) -> {
      case (Seq(Stream(condition, condType), Stream(thenCase, thenType), Stream(elseCase, elseType)), name, loc) =>
        Types.requireType(Types.Stream(Types.Bool), Types.Stream(condType), condition.loc)
        Types.requireType(Types.Stream(thenType), Types.Stream(elseType), elseCase.loc)
        (Seq(name -> TesslaCore.IfThenElse(condition, thenCase, elseCase, loc)), Stream(TesslaCore.Stream(name, loc), thenType))
      case (Seq(Stream(condition, condType), thenCase @ Literal(_), Stream(elseCase, elseType)), name, loc) =>
        Types.requireType(Types.Stream(Types.Bool), Types.Stream(condType), condition.loc)
        Types.requireType(Types.Stream(thenCase.typ), Types.Stream(elseType), elseCase.loc)
        val liftedThenCase = mkId(name)
        (
          Seq(
            liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase.value, loc),
            name -> TesslaCore.IfThenElse(condition, TesslaCore.Stream(liftedThenCase, loc), elseCase, loc)
          ),
          Stream(TesslaCore.Stream(name, loc), thenCase.typ)
        )
      case (Seq(Stream(condition, condType), Stream(thenCase, thenType), elseCase @ Literal(_)), name, loc) =>
        Types.requireType(Types.Stream(Types.Bool), Types.Stream(condType), condition.loc)
        Types.requireType(thenType, elseCase.typ, elseCase.loc)
        val liftedElseCase = mkId(name)
        (
          Seq(
            liftedElseCase -> TesslaCore.Default(TesslaCore.Nil(loc), elseCase.value, loc),
            name -> TesslaCore.IfThenElse(condition, thenCase, TesslaCore.Stream(liftedElseCase, loc), loc)
          ),
          Stream(TesslaCore.Stream(name, loc), thenType)
        )
      case (Seq(Stream(condition, condType), thenCase @ Literal(_), elseCase @ Literal(_)), name, loc) =>
        Types.requireType(Types.Stream(Types.Bool), Types.Stream(condType), condition.loc)
        Types.requireType(thenCase.typ, elseCase.typ, elseCase.loc)
        val liftedThenCase = mkId(name)
        val liftedElseCase = mkId(name)
        (
          Seq(
            liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase.value, loc),
            liftedElseCase -> TesslaCore.Default(TesslaCore.Nil(loc), elseCase.value, loc),
            name -> TesslaCore.IfThenElse(condition, TesslaCore.Stream(liftedThenCase, loc), TesslaCore.Stream(liftedElseCase, loc), loc)
          ),
          Stream(TesslaCore.Stream(name, loc), thenCase.typ)
        )
      case (Seq(Literal(TesslaCore.BoolLiteral(true, _)), thenCase, _), _, _) =>
        (Seq(), thenCase)
      case (Seq(Literal(TesslaCore.BoolLiteral(false, _)), _, elseCase), _, _) =>
        (Seq(), elseCase)
      case (Seq(condition @ Literal(_), _, _), _, _) =>
        throw TypeMismatch(Types.Bool, condition.typ, condition.loc)
    },
    ("if then", 2) -> {
      case (Seq(Stream(condition, condType), Stream(thenCase, thenType)), name, loc) =>
        Types.requireType(Types.Stream(Types.Bool), Types.Stream(condType), condition.loc)
        (Seq(name -> TesslaCore.IfThen(condition, thenCase, loc)), Stream(TesslaCore.Stream(name, loc), thenType))
      case (Seq(Stream(condition, condType), thenCase @ Literal(_)), name, loc) =>
        Types.requireType(Types.Stream(Types.Bool), Types.Stream(condType), condition.loc)
        val liftedThenCase = mkId(name)
        (
          Seq(
            liftedThenCase -> TesslaCore.Default(TesslaCore.Nil(loc), thenCase.value, loc),
            name -> TesslaCore.IfThen(condition, TesslaCore.Stream(liftedThenCase, loc), loc)
          ),
          Stream(TesslaCore.Stream(name, loc), thenCase.typ)
        )
      case (Seq(Literal(TesslaCore.BoolLiteral(true, _)), thenCase), _, _) =>
        (Seq(), thenCase)
      case (Seq(Literal(TesslaCore.BoolLiteral(false, _)), _), _, loc) =>
        (Seq(), Stream(TesslaCore.Nil(loc), Types.WildCard))
    }
  )
}

object BuiltIns {
  def apply(mkId: String => String) = {
    new BuiltIns(mkId).builtins
  }
}
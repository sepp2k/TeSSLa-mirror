package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AstToCore.{TranslatedExpression, Arg, Stream, Literal}
import Errors.TypeMismatch

class BuiltIns private(mkId: String => String) {
  def primOp(op: PrimitiveOperators.PrimitiveOperator): (Seq[Arg], String, Location) => TranslatedExpression = {
    (args, name, loc) =>
      val literals = args.collect {
        case Literal(lit) => lit
      }
      if (literals.length == args.length) {
        (Seq(), Literal(op.eval(literals, loc).get.toLiteral))
      } else {
        val streams: Seq[(Seq[(String, TesslaCore.Expression)], Stream)] = args.map {
          case s @ Stream(_, _) =>
            (Seq(), s)
          case l @ Literal(_) =>
            val liftedName = mkId(name)
            (Seq(liftedName -> TesslaCore.Default(TesslaCore.Nil(l.loc), l.value, l.loc)),
              Stream(TesslaCore.Stream(liftedName, l.loc), l.typ))
        }
        try {
          (streams.flatMap(_._1) :+ (name -> TesslaCore.Lift(op, streams.map(_._2.value), loc)),
            Stream(TesslaCore.Stream(name, loc), op.returnTypeFor(streams.map {
              case (_, stream) => (stream.elementType, stream.loc)
            }))
          )
        } catch {
          case TypeMismatch(expected: Types.ValueType, actual: Types.ValueType, loc) =>
            throw TypeMismatch(Types.Stream(expected), Types.Stream(actual), loc)
        }
      }
  }

  val builtins: Map[(String, Int), (Seq[Arg], String, Location) => TranslatedExpression] = Map(
    ("nil", 0) -> {
      case (Seq(), _, loc) => (Seq(), Stream(TesslaCore.Nil(loc), Types.Nothing))
    },
    ("default", 2) -> {
      case (Seq(Stream(stream, t1), Stream(default, t2)), name, loc) =>
        val t = Types.requireType(Types.Stream(t1), Types.Stream(t2), default.loc).elementType
        (Seq(name -> TesslaCore.DefaultFrom(stream, default, loc)), Stream(TesslaCore.Stream(name, loc), t))
      case (Seq(Stream(stream, t), default @ Literal(_)), name, loc) =>
        val resultType = Types.requireType(t, default.typ, default.loc)
        (Seq(name -> TesslaCore.Default(stream, default.value, loc)), Stream(TesslaCore.Stream(name, loc), resultType))
      case (Seq(lit @ Literal(_), _), _, _) => throw TypeMismatch(Types.Stream(Types.Nothing), lit.typ, lit.loc)
    },
    ("const", 2) -> {
      case (Seq(lit @ Literal(value), Stream(clock, _)), name, loc) =>
        (Seq(name -> TesslaCore.Lift(PrimitiveOperators.Const(value), Seq(clock), loc)),
          Stream(TesslaCore.Stream(name, loc), lit.typ))
      case (Seq(Literal(_), clock @ Literal(_)), _, _) =>
        throw TypeMismatch(Types.Stream(Types.Nothing), clock.typ, clock.loc)
      case (Seq(stream @ Stream(_, _), _), _, _) => throw TypeMismatch(Types.Nothing, stream.typ, stream.loc)
    },
    ("time", 1) -> {
      case (Seq(Stream(clock, _)), name, loc) =>
        (Seq(name -> TesslaCore.Time(clock, loc)), Stream(TesslaCore.Stream(name, loc), Types.Int))
      case (Seq(clock @ Literal(_)), _, _) =>
        throw TypeMismatch(Types.Stream(Types.Nothing), clock.typ, clock.loc)
    },
    ("last", 2) -> {
      case (Seq(Stream(values, t), Stream(clock, _)), name, loc) =>
        (Seq(name -> TesslaCore.Last(values, clock, loc)), Stream(TesslaCore.Stream(name, loc), t))
      case (Seq(values @ Literal(_), _), _, _) =>
        throw TypeMismatch(Types.Stream(Types.Nothing), values.typ, values.loc)
      case (Seq(_, clock @ Literal(_), _), _, _) =>
        throw TypeMismatch(Types.Stream(Types.Nothing), clock.typ, clock.loc)
    },
    ("delayedLast", 2) -> {
      case (Seq(Stream(values, valueType), Stream(delays, delayType)), name, loc) =>
        Types.requireType(Types.Stream(Types.Int), Types.Stream(delayType), delays.loc)
        (Seq(name -> TesslaCore.DelayedLast(values, delays, loc)), Stream(TesslaCore.Stream(name, loc), valueType))
      case (Seq(values @ Literal(_), _), _, _) =>
        throw TypeMismatch(Types.Stream(Types.Nothing), values.typ, values.loc)
      case (Seq(_, delays @ Literal(_), _), _, _) =>
        throw TypeMismatch(Types.Stream(Types.Nothing), delays.typ, delays.loc)
    },
    ("+", 2) -> primOp(PrimitiveOperators.Add),
    ("-", 2) -> primOp(PrimitiveOperators.Sub),
    ("*", 2) -> primOp(PrimitiveOperators.Mul),
    ("/", 2) -> primOp(PrimitiveOperators.Div),
    ("&", 2) -> primOp(PrimitiveOperators.BitAnd),
    ("|", 2) -> primOp(PrimitiveOperators.BitOr),
    ("^", 2) -> primOp(PrimitiveOperators.BitXor),
    ("<<", 2) -> primOp(PrimitiveOperators.LeftShift),
    (">>", 2) -> primOp(PrimitiveOperators.RightShift),
    ("~", 1) -> primOp(PrimitiveOperators.BitFlip),
    ("-", 1) -> primOp(PrimitiveOperators.Negate),
    ("<", 2) -> primOp(PrimitiveOperators.Lt),
    (">", 2) -> primOp(PrimitiveOperators.Gt),
    ("<=", 2) -> primOp(PrimitiveOperators.Lte),
    (">=", 2) -> primOp(PrimitiveOperators.Gte),
    ("==", 2) -> primOp(PrimitiveOperators.Eq),
    ("!=", 2) -> primOp(PrimitiveOperators.Neq),
    ("&&", 2) -> primOp(PrimitiveOperators.And),
    ("||", 2) -> primOp(PrimitiveOperators.Or),
    ("!", 1) -> primOp(PrimitiveOperators.Not),
    ("first", 2) -> primOp(PrimitiveOperators.First),
    ("if then else", 3) -> primOp(PrimitiveOperators.IfThenElse),
    ("if then", 2) -> primOp(PrimitiveOperators.IfThen)
  )
}

object BuiltIns {
  def apply(mkId: String => String) = {
    new BuiltIns(mkId).builtins
  }
}
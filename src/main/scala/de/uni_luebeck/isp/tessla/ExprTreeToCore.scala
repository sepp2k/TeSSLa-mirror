package de.uni_luebeck.isp.tessla

import scala.util.Try
import scala.collection.mutable.ArrayBuffer

object ExprTreeToCore extends CompilerPass[Definitions, TesslaCore.Specification] {
  case class InternalCoreTranslationError(msg: String, atLocation: NestedLoc) extends Fatal
  case class ArityMismatch(function: String, expected: Int, actual: Int, atLocation: NestedLoc) extends Fatal
  case class NotYetImplementedError(feature: String, atLocation: NestedLoc) extends Fatal
  case class UndefinedMacroError(macroName: String, atLocation: NestedLoc) extends Fatal

  override def apply(compiler: Compiler, definitions: Definitions) = Try {
    val inStreams = new ArrayBuffer[(String, NestedLoc)]

    def translateExpression(exprTree: ExprTree): TesslaCore.Expression = {
      val args = exprTree.args.mapValues(translateExpression)

      def checkArity(function: String, num: Int, loc: NestedLoc): List[TesslaCore.Expression] = {
        (0 until num).map { i =>
          args.getOrElse(Pos(i), throw ArityMismatch(function, expected = num, actual = i, atLocation = loc))
        }.toList
      }

      def lift(exp: TesslaCore.Expression) = exp match {
        case _ : TesslaCore.LiteralValue =>
          TesslaCore.Default(TesslaCore.Nil(exp.loc), exp, exp.loc)
        case _ =>
          exp
      }

      exprTree.fn match {
        case InputFn(name, _, loc) =>
          inStreams += name -> loc
          TesslaCore.Input(name, loc)

        case TypeAscrFn(_, loc) =>
          args.getOrElse(Pos(0), throw InternalCoreTranslationError("Missing argument in type ascriptio node", loc))

        case LiteralFn(IntLiteral(i), loc) =>
          TesslaCore.IntLiteral(i, loc)

        case LiteralFn(BoolLiteral(b), loc) =>
          TesslaCore.BoolLiteral(b, loc)

        case LiteralFn(lit, loc) =>
          throw NotYetImplementedError(s"Literal of type ${lit.getClass.getSimpleName}", loc)

        case NamedFn("+", loc) =>
          var List(lhs, rhs) = checkArity("+", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Add(lhs, rhs, loc)

        case NamedFn("-", loc) =>
          var List(lhs, rhs) = checkArity("-", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Sub(lhs, rhs, loc)

        case NamedFn("*", loc) =>
          var List(lhs, rhs) = checkArity("*", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Mul(lhs, rhs, loc)

        case NamedFn("<", loc) =>
          var List(lhs, rhs) = checkArity("<", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Lt(lhs, rhs, loc)

        case NamedFn(">", loc) =>
          var List(lhs, rhs) = checkArity(">", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Gt(lhs, rhs, loc)

        case NamedFn("<=", loc) =>
          var List(lhs, rhs) = checkArity("<=", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Lte(lhs, rhs, loc)

        case NamedFn(">=", loc) =>
          var List(lhs, rhs) = checkArity(">=", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Gte(lhs, rhs, loc)

        case NamedFn("==", loc) =>
          var List(lhs, rhs) = checkArity("==", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Eq(lhs, rhs, loc)

        case NamedFn("!=", loc) =>
          var List(lhs, rhs) = checkArity("!=", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Neq(lhs, rhs, loc)

        case NamedFn("||", loc) =>
          var List(lhs, rhs) = checkArity("||", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.Or(lhs, rhs, loc)

        case NamedFn("&&", loc) =>
          var List(lhs, rhs) = checkArity("&&", 2, loc)
          lhs = lift(lhs)
          rhs = lift(rhs)
          TesslaCore.And(lhs, rhs, loc)

        case NamedFn("!", loc) =>
          var List(arg) = checkArity("!", 1, loc)
          arg = lift(arg)
          TesslaCore.Not(arg, loc)

        case NamedFn("if then else", loc) =>
          var List(cond, thenCase, elseCase) = checkArity("if-then-else", 3, loc)
          cond = lift(cond)
          thenCase = lift(thenCase)
          elseCase = lift(elseCase)
          TesslaCore.IfThenElse(cond, thenCase, elseCase, loc)

        case NamedFn("if then", loc) =>
          var List(cond, thenCase) = checkArity("if-then", 2, loc)
          cond = lift(cond)
          thenCase = lift(thenCase)
          TesslaCore.IfThen(cond, thenCase, loc)

        case NamedFn("last", loc) =>
          val List(values, clock) = checkArity("last", 2, loc)
          TesslaCore.Last(values, clock, loc)

        case NamedFn("delayedLast", loc) =>
          val List(values, delays) = checkArity("delayedLast", 2, loc)
          TesslaCore.DelayedLast(values, delays, loc)

        case NamedFn("default", loc) =>
          val List(values, default) = checkArity("default", 2, loc)
          TesslaCore.Default(values, default, loc)

        case NamedFn("defaultFrom", loc) =>
          val List(values, default) = checkArity("defaultFrom", 2, loc)
          TesslaCore.DefaultFrom(values, default, loc)

        case NamedFn("time", loc) =>
          val List(values) = checkArity("defaultFrom", 1, loc)
          TesslaCore.Time(values, loc)

        case NamedFn("()", loc) =>
          val List() = checkArity("()", 0, loc)
          TesslaCore.Unit(loc)

        case NamedFn("nil", loc) =>
          val List() = checkArity("nil", 0, loc)
          TesslaCore.Nil(loc)

        case NamedFn(name, loc) if definitions.streamDefs.isDefinedAt(name) =>
          checkArity(name, 0, loc)
          TesslaCore.Var(name, loc)

        case NamedFn(name, loc) =>
          throw UndefinedMacroError(name, loc)
      }
    }

    val defs = definitions.streamDefs.map { case (name, sdef) => name -> translateExpression(sdef.expr) }
    val outs = definitions.outStreams.values.map { out => (out.name, out.loc) }.toSeq

    TesslaCore.Specification(defs, inStreams = inStreams, outStreams = outs)
  }
}

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
          val List(lhs, rhs) = checkArity("+", 2, loc)
          TesslaCore.Add(lhs, rhs, loc)

        case NamedFn("-", loc) =>
          val List(lhs, rhs) = checkArity("-", 2, loc)
          TesslaCore.Sub(lhs, rhs, loc)

        case NamedFn("last", loc) =>
          val List(values, clock) = checkArity("last", 2, loc)
          TesslaCore.Last(values, clock, loc)

        case NamedFn("default", loc) =>
          val List(values, default) = checkArity("default", 2, loc)
          TesslaCore.Default(values, default, loc)

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

    TesslaCore.Specification(definitions.streamDefs.mapValues(sdef => translateExpression(sdef.expr)),
      inStreams = inStreams, outStreams = definitions.outStreams.values.map { out => (out.name, out.loc) }.toSeq)
  }
}

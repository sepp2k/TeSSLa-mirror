package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._

/**
 * @author Normann Decker <decker@isp.uni-luebeck.de>
 */

object ASTText {

  implicit class UnresolvedFunctionText(fn: UnresolvedFunction) {
    def toText = fn.name
  }

  implicit class UnresolvedConstantText(c: UnresolvedConstant) {
    def toText = c.name
  }

  implicit class IntegralConstantText(c: IntegralConstant) {
    def toText = c.value.toString
  }

  implicit class SpecText(s: Spec) {
    def toText = s.statements.map { _.toText }.mkString("\n")
  }
  
  implicit class StatementText(s: Statement) {
    def toText = s match {
      case d:Def => "define " + d.name + " := " + d.definition.toText
      case md: MacroDef => "define " + md.name + "(" + md.args.mkString(", ") + ")" + " := " + md.definition.toText
      case o: Out =>  "out " + o.name
    }
  }
  
  implicit class NamedArgText(n: NamedArg[TreeTerm]) {
    def toText = n.name + " := " + n.arg.toText
  }

  implicit class TreeTermText(t: TreeTerm) {
    def toText: String = t match {
      case TreeTerm(x@UnresolvedTerm(_, _))            => x.toText
      case TreeTerm(Const(x@UnresolvedConstant(_), _)) => x.toText
      case TreeTerm(Const(x@IntegralConstant(_), _))   => x.toText
      case TreeTerm(x@App(UnresolvedFunction(f), args: List[TreeTerm], nargs: List[NamedArg[TreeTerm]], _)) =>
        f + "(" + nargs.map { _.toText }.mkString(args.map { _.toText }.mkString(", "), ", ", "") + ")"
      case TreeTerm(x@TypeAscr(_, _)) => x.toText
      case x                          => x.toString
    }
  }

  implicit class UnresolvedTermText(t: UnresolvedTerm) {
    def toText = t.name
  }

  implicit class TypeAscrText[SubTerm](ta: TypeAscr[SubTerm]) {
    def toText = ta.term match {
      case x: TreeTerm => x.toText
      case x           => x.toString
    }
  }
}
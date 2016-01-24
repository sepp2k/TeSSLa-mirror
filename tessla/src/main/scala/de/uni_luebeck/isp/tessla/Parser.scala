package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Ast.ExprTypeAscr

import scala.io.Source
import de.uni_luebeck.isp.compacom.{WithLocation, Parsers, SimpleTokens, SimpleTokenizer}

import scala.util.{Failure, Success}

object Parser extends CompilerPass[TesslaSource, Ast.Spec] {
  case class ParserError(val parserFailure: Parsers.Failure) extends Exception {
    override def toString = "ParserError(" + parserFailure + ")"
  }

  override def apply(compiler: Compiler, source: TesslaSource) = {
    Parsers.parseAll(Parsers.spec, source.src) match {
      case Parsers.Success(_, spec, _, _) => Success(spec)
      case fail: Parsers.Failure => Failure(ParserError(fail))
    }
  }

  object Tokens extends SimpleTokens {
    case object DEFINE extends Token("define")
    case object OF_TYPE extends Token(":")
    case object PERCENT extends Token("%")
    case object DEFINE_AS extends Token(":=")
    case object COMMA extends Token(",")
    case object LPAREN extends Token("(")
    case object RPAREN extends Token(")")
    case object LT extends Token("<")
    case object GT extends Token(">")
  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens
    import tokens._

    override val keywords = List(DEFINE)
    override val symbols = List(DEFINE_AS, OF_TYPE, COMMA, LPAREN, RPAREN, PERCENT, LT, GT)
    override val comments = List("--" -> "\n")
  }

  object Parsers extends Parsers {
    import scala.language.implicitConversions
    import Tokens._
    override val tokenizer = Tokenizer

    implicit def tokenToParser(t: Token): Parser[WithLocation[Token]] = token(t)

    def spec: Parser[Ast.Spec] = statement.* ^^ Ast.Spec

    def statement: Parser[Ast.Statement] = defOrMacroDef

    // TODO identifier completion, requires some small compacom enhancements
    def identifier: Parser[Ast.Identifier] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => Ast.Identifier(name, SourceLoc(loc))
    }

    def intLit: Parser[Ast.IntLit] = matchToken("integer", Set("<integer>")) {
      case WithLocation(loc, INT(value)) => Ast.IntLit(value, SourceLoc(loc))
    }

    def stringLit: Parser[Ast.StringLit] = matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(value)) => Ast.StringLit(value, SourceLoc(loc))
    }

    def defOrMacroDef: Parser[Ast.Statement] =
      (DEFINE ~> identifier ~ macroArgs.? ~ typeAscr.? ~ (DEFINE_AS ~> expr)) ^^! {
        case (loc, (((name, args), typeAscr), expr)) =>
          Ast.Def(name, args getOrElse Seq(), typeAscr, expr, SourceLoc(loc))
      }

    def macroArgs: Parser[Seq[Ast.MacroArg]] = LPAREN ~> rep1sep(macroArg, COMMA) <~ RPAREN

    def macroArg: Parser[Ast.MacroArg] = (identifier ~ typeAscr.?) ^^ Ast.MacroArg.tupled

    def typeAscr: Parser[Ast.Type] = OF_TYPE ~> `type`

    def `type`: Parser[Ast.Type] = typeNameOrApp

    def typeNameOrApp: Parser[Ast.Type] = identifier ~ typeAppArgs.? ^^! {
      case (_, (name, None)) => Ast.TypeName(name)
      case (loc, (name, Some(args))) => Ast.TypeApp(name, args, SourceLoc(loc))
    }

    def typeAppArgs: Parser[Seq[Ast.Type]] = LT ~> rep1sep(`type`, COMMA) <~ GT

    def expr: Parser[Ast.Expr] = exprAtomic ~ typeAscr.? ^^ {
      case (expr, None) => expr
      case (expr, Some(typeAscr)) => Ast.ExprTypeAscr(expr, typeAscr)
    }

    def exprAtomic: Parser[Ast.Expr] = exprGroup | exprNameOrApp | exprLit

    def exprGroup: Parser[Ast.Expr] = (LPAREN ~> expr <~ RPAREN) ^^! {
      case (loc, expr) => Ast.ExprGrouped(expr, SourceLoc(loc))
    }

    def exprNameOrApp: Parser[Ast.Expr] = identifier ~ exprAppArgs.? ^^! {
      case (_, (name, None)) => Ast.ExprName(name)
      case (loc, (name, Some(args))) => Ast.ExprApp(name, args, SourceLoc(loc))
    }

    def exprLit: Parser[Ast.Expr] = exprIntLit | exprStringLit

    def exprIntLit: Parser[Ast.Expr] = intLit ^^ Ast.ExprIntLit

    def exprStringLit: Parser[Ast.Expr] = stringLit ^^ Ast.ExprStringLit

    def exprAppArgs: Parser[Seq[Ast.AppArg]] = LPAREN ~> rep1sep(exprAppArg, COMMA) <~ RPAREN

    def exprAppArg: Parser[Ast.AppArg] = expr ~^ {
      case (x @ Ast.ExprName(name)) =>
        DEFINE_AS ~> expr ^^ (Ast.NamedArg(name, _)) | success(Ast.PosArg(x))
      case x => success(Ast.PosArg(x))
    }
  }
}

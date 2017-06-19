package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.{WithLocation, Parsers, SimpleTokens, SimpleTokenizer}
import de.uni_luebeck.isp.compacom

import scala.util.{Failure, Success}

object Parser extends CompilerPass[TesslaSource, Ast.Spec] {
  case class ParserError(parserFailure: Parsers.Failure) extends CompilationError {
    override def loc = SourceLoc(parserFailure.loc)
    override def message = parserFailure.message
  }

  override def apply(compiler: Compiler, source: TesslaSource) = {
    Parsers.parseAll(Parsers.spec, source.src) match {
      case Parsers.Success(_, spec, _, _) => spec
      case fail: Parsers.Failure => throw ParserError(fail)
    }
  }

  object Tokens extends SimpleTokens {
    case object DEFINE extends Token("define")
    case object DEF extends Token("def")
    case object OUT extends Token("out")
    case object IN extends Token("in")
    case object IF extends Token("if")
    case object THEN extends Token("then")
    case object ELSE extends Token("else")
    case object OF_TYPE extends Token(":")
    case object PERCENT extends Token("%")
    case object DEFINE_AS extends Token(":=")
    case object COMMA extends Token(",")
    case object LPAREN extends Token("(")
    case object RPAREN extends Token(")")
    case object LBRACE extends Token("{")
    case object RBRACE extends Token("}")
    case object LSHIFT extends Token(">>")
    case object RSHIFT extends Token("<<")
    case object GEQ extends Token(">=")
    case object LEQ extends Token("<=")
    case object LT extends Token("<")
    case object GT extends Token(">")
    case object NEQ extends Token("!=")
    case object EQ extends Token("==")
    case object AND extends Token("&&")
    case object OR extends Token("||")
    case object BITFLIP extends Token("~")
    case object BITAND extends Token("&")
    case object BITOR extends Token("|")
    case object BITXOR extends Token("^")
    case object PLUS extends Token("+")
    case object MINUS extends Token("-")
    case object TIMES extends Token("*")
    case object SLASH extends Token("/")
    case object BANG extends Token("!")
  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens
    import tokens._

    override val keywords = List(DEFINE, DEF, OUT, IN, IF, THEN, ELSE)
    override val symbols = List(DEFINE_AS, OF_TYPE, COMMA, LPAREN, RPAREN, LBRACE, RBRACE, PERCENT, LSHIFT, RSHIFT,
      GEQ, LEQ, NEQ, EQ, LT, GT, AND, OR, BITFLIP, BITAND, BITOR, BITXOR, PLUS, MINUS, TIMES, SLASH, BANG)
    override val comments = List("--" -> "\n")

    override def isIdentifierCont(c: Char): Boolean = super.isIdentifierCont(c) || c == '.'
  }

  object Parsers extends Parsers {
    import scala.language.implicitConversions
    import Tokens._
    override val tokenizer = Tokenizer

    implicit def tokenToParser(t: Token): Parser[WithLocation[Token]] = token(t)

    def spec: Parser[Ast.Spec] = statement.* ^^ Ast.Spec

    // TODO identifier completion, requires some small compacom enhancements
    def identifier: Parser[Ast.Identifier] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => Ast.Identifier(name, SourceLoc(loc))
    }

    def boolLit: Parser[Ast.BoolLit] = matchToken("boolean", Set("<boolean>")) {
      case WithLocation(loc, ID("true")) => {
        Ast.BoolLit(true, SourceLoc(loc))
      }
      case WithLocation(loc, ID("false")) => Ast.BoolLit(false, SourceLoc(loc))
    }

    def intLit: Parser[Ast.IntLit] = matchToken("integer", Set("<integer>")) {
      case WithLocation(loc, INT(value)) => Ast.IntLit(BigInt(value), SourceLoc(loc))
    }

    def floatLit: Parser[Ast.FloatLit] = matchToken("float", Set("<float>")) {
      case WithLocation(loc, FLOAT(value)) => Ast.FloatLit(BigDecimal(value), SourceLoc(loc))
    }

    def stringLit: Parser[Ast.StringLit] = matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(value)) => Ast.StringLit(value, SourceLoc(loc))
    }

    def defOrMacroDef =
      ((DEFINE | DEF) ~> identifier ~ macroArgs.? ~ typeAscr.? ~ (DEFINE_AS ~> expr)) ^^! {
        case (loc, (((name, args), typeAscr), expr)) =>
          Ast.Def(name, args getOrElse Seq(), typeAscr, expr, SourceLoc(loc))
      }

    def statement: Parser[Ast.Statement] =
      defOrMacroDef |
      OUT ~> identifier ^^! {
        case (loc, name) =>
          Ast.Out(name, SourceLoc(loc))
      } |
      IN ~> identifier ~ typeAscr ^^! {
        case (loc, (name, typeAscr)) =>
          Ast.In(name, typeAscr, SourceLoc(loc))
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

    def expr: Parser[Ast.Expr] = ifThenElse | typedExpression

    def ifThenElse = (IF ~ expr) ~ (THEN ~> expr) ~ (ELSE ~> expr).? ^^! {
      case (loc, (((ifToken, cond), thenCase), Some(elseCase))) =>
        Ast.ExprApp(Ast.Identifier("if then else", SourceLoc(ifToken.loc)), List(Ast.PosArg(cond), Ast.PosArg(thenCase), Ast.PosArg(elseCase)), SourceLoc(loc))
      case (loc, (((ifToken, cond), thenCase), None)) =>
        Ast.ExprApp(Ast.Identifier("if then", SourceLoc(ifToken.loc)), List(Ast.PosArg(cond), Ast.PosArg(thenCase)), SourceLoc(loc))
    }

    def typedExpression: Parser[Ast.Expr] = infixExpr ~ typeAscr.? ^^ {
      case (expr, None) => expr
      case (expr, Some(typeAscr)) => Ast.ExprTypeAscr(expr, typeAscr)
    }

    def infixOp(loc: compacom.Location, lhs: Ast.Expr, rhss: List[(WithLocation[Token], Ast.Expr)]) = {
      rhss.foldLeft(lhs) {
        case (l,(op, r)) =>
          Ast.ExprApp(Ast.Identifier(op.value.string, SourceLoc(op.loc)), List(Ast.PosArg(l), Ast.PosArg(r)), SourceLoc(loc))
      }
    }

    def infixExpr: Parser[Ast.Expr] = conjunction ~ (OR ~ conjunction).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def conjunction: Parser[Ast.Expr] = comparison ~ (AND ~ comparison).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def comparison: Parser[Ast.Expr] = bitOrExpr ~ ((EQ | LT | GT | LEQ | GEQ | NEQ) ~ bitOrExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitOrExpr: Parser[Ast.Expr] = bitAndExpr ~ ((BITOR | BITXOR) ~ bitAndExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitAndExpr: Parser[Ast.Expr] = bitShiftExpr ~ (BITAND ~ bitShiftExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitShiftExpr: Parser[Ast.Expr] = additiveExpr ~ ((LSHIFT | RSHIFT) ~ additiveExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def additiveExpr: Parser[Ast.Expr] = multiplicativeExpr ~ ((PLUS | MINUS) ~ multiplicativeExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def multiplicativeExpr: Parser[Ast.Expr] = unaryExpr ~ ((TIMES | SLASH) ~ unaryExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def unaryExpr: Parser[Ast.Expr] =
      BANG ~ exprAtomic ^^! {
        case (loc, (op, expr)) =>
          Ast.ExprApp(Ast.Identifier("!", SourceLoc(op.loc)), List(Ast.PosArg(expr)), SourceLoc(loc))
      } |
      BITFLIP ~ exprAtomic ^^! {
        case (loc, (op, expr)) =>
          Ast.ExprApp(Ast.Identifier("~", SourceLoc(op.loc)), List(Ast.PosArg(expr)), SourceLoc(loc))
      } |
      MINUS ~ exprAtomic ^^! {
        case (loc, (op, expr)) =>
          Ast.ExprApp(Ast.Identifier("-", SourceLoc(op.loc)), List(Ast.PosArg(expr)), SourceLoc(loc))
      } |
      exprAtomic

    def exprAtomic: Parser[Ast.Expr] = exprLit | exprGroup | exprBlock | exprNameOrApp

    def exprGroup: Parser[Ast.Expr] = (LPAREN ~> expr.? <~ RPAREN) ^^! {
      case (loc, Some(expr)) => expr
      case (loc, None) => Ast.ExprUnit(SourceLoc(loc))
    }

    def exprBlock = (LBRACE ~> defOrMacroDef.* ~ expr <~ RBRACE) ^^! {
      case (loc, (statements, expr)) =>
        Ast.ExprBlock(statements, expr, SourceLoc(loc))
    }

    def exprNameOrApp: Parser[Ast.Expr] = identifier ~ exprAppArgs.? ^^! {
      case (_, (name, None)) => Ast.ExprName(name)
      case (loc, (name, Some(args))) => Ast.ExprApp(name, args, SourceLoc(loc))
    }

    def exprLit: Parser[Ast.Expr] = exprIntLit | exprStringLit | exprBoolLit

    def exprBoolLit: Parser[Ast.Expr] = boolLit ^^ Ast.ExprBoolLit

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

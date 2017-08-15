package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.compacom
import de.uni_luebeck.isp.tessla.TimeUnit._

class Parser extends TranslationPhase[TesslaSource, Ast.Spec] {

  case class ParserError(parserFailure: Parsers.Failure) extends CompilationError {
    override def loc = SourceLoc(parserFailure.loc)

    override def message = parserFailure.message
  }

  override def translateSpec(source: TesslaSource) = {
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

    case object AS extends Token("as")

    case object TRUE extends Token("true")

    case object FALSE extends Token("false")

    case object COLON extends Token(":")

    case object PERCENT extends Token("%")

    case object COLONEQ extends Token(":=")

    case object COMMA extends Token(",")

    case object LPAREN extends Token("(")

    case object RPAREN extends Token(")")

    case object LBRACE extends Token("{")

    case object RBRACE extends Token("}")

    case object LSHIFT extends Token("<<")

    case object RSHIFT extends Token(">>")

    case object GEQ extends Token(">=")

    case object LEQ extends Token("<=")

    case object LT extends Token("<")

    case object GT extends Token(">")

    case object NEQ extends Token("!=")

    case object EQEQ extends Token("==")

    case object EQ extends Token("=")

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

    override val keywords = List(DEFINE, DEF, OUT, IN, IF, THEN, ELSE, TRUE, FALSE, AS)
    override val symbols = List(COLONEQ, COLON, COMMA, LPAREN, RPAREN, LBRACE, RBRACE, PERCENT,
      LSHIFT, RSHIFT, GEQ, LEQ, NEQ, EQEQ, EQ, LT, GT, AND, OR, BITFLIP, BITAND, BITOR, BITXOR, PLUS, MINUS, TIMES,
      SLASH, BANG)
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

    def define =
      DEFINE ^^^! {
        loc => warn(SourceLoc(loc), "The keyword 'define' is deprecated, use 'def' instead.")
      } | DEF

    def defOrMacroDef =
      (define ~> identifier ~ macroArgs.? ~ typeAscr.? ~ (COLONEQ ~> expr)) ^^! {
        case (loc, (((name, args), typeAscr), expr)) =>
          Ast.Def(name, args getOrElse Seq(), typeAscr, expr, SourceLoc(loc))
      }

    def statement: Parser[Ast.Statement] =
      defOrMacroDef |
        OUT ~> expr ~ (AS ~> identifier).? ^^! {
          case (loc, (expr, name)) =>
            Ast.Out(expr, name, SourceLoc(loc))
        } |
        IN ~> identifier ~ typeAscr ^^! {
          case (loc, (name, typeAscr)) =>
            Ast.In(name, typeAscr, SourceLoc(loc))
        }

    def macroArgs: Parser[Seq[Ast.MacroArg]] = LPAREN ~> rep1sep(macroArg, COMMA) <~ RPAREN

    def macroArg: Parser[Ast.MacroArg] = (identifier ~ typeAscr.?) ^^ Ast.MacroArg.tupled

    def typeAscr: Parser[Ast.Type] = COLON ~> `type`

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
        case (l, (op, r)) =>
          Ast.ExprApp(Ast.Identifier(op.value.string, SourceLoc(op.loc)), List(Ast.PosArg(l), Ast.PosArg(r)), SourceLoc(loc))
      }
    }

    def infixExpr: Parser[Ast.Expr] = conjunction ~ (OR ~ conjunction).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def conjunction: Parser[Ast.Expr] = comparison ~ (AND ~ comparison).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def comparison: Parser[Ast.Expr] = bitOrExpr ~ ((EQEQ | LT | GT | LEQ | GEQ | NEQ) ~ bitOrExpr).* ^^! {
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
      case (_, Some(expr)) => expr
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

    def exprBoolLit: Parser[Ast.ExprBoolLit] =
      TRUE ^^^! {
        loc => Ast.ExprBoolLit(true, SourceLoc(loc))
      } |
        FALSE ^^^! {
          loc => Ast.ExprBoolLit(false, SourceLoc(loc))
        }

    def exprIntLit: Parser[Ast.Expr] = matchToken("integer", Set("<integer>")){
      case WithLocation(loc, INT(value)) => BigInt(value)
    } ~ timeUnit.? ^^! {
      case (loc, (value, None)) => Ast.ExprIntLit(value, SourceLoc(loc))
      case (loc, (value, Some(unit))) => Ast.ExprTimeLit(value, unit, SourceLoc(loc))
    }

    def timeUnit: Parser[TimeUnit.TimeUnit] =
      ID("ns") ^^^ {
        Nanos
      } |
        ID("us") ^^^ {
          Micros
        } |
        ID("ms") ^^^ {
        Millis
      } |
        ID("s") ^^^ {
          Seconds
        } |
        ID("m") ^^^ {
          Minutes
        } |
        ID("h") ^^^ {
          Hours
        } |
        ID("d") ^^^ {
          Days
        }

    def exprStringLit: Parser[Ast.ExprStringLit] = matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(value)) => Ast.ExprStringLit(value, SourceLoc(loc))
    }

    def exprAppArgs: Parser[Seq[Ast.AppArg]] = LPAREN ~> rep1sep(exprAppArg, COMMA) <~ RPAREN

    def namedArgAssignmentOperator =
      COLONEQ ^^^! {
        loc => warn(SourceLoc(loc), "Using ':=' for named arguments is deprecated, use '=' instead.")
      } | EQ

    def exprAppArg: Parser[Ast.AppArg] = expr ~^ {
      case (x@Ast.ExprName(name)) =>
        namedArgAssignmentOperator ~> expr ^^ (Ast.NamedArg(name, _)) | success(Ast.PosArg(x))
      case x => success(Ast.PosArg(x))
    }
  }

}

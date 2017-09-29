package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.compacom
import de.uni_luebeck.isp.tessla.Errors.ParserError

object TesslaParser extends TranslationPhase[TesslaSource, Tessla.Spec] with Parsers {
  override def translateSpec(source: TesslaSource) = {
    val p =  new Parsers(source.path)
    parseAll(p.spec, source.src) match {
      case Success(_, spec, _, _) => spec
      case fail: Failure => throw ParserError(fail.message, SourceLoc(fail.loc, source.path))
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

    case object INCLUDE extends Token("include")

  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens

    import tokens._

    override val keywords = List(DEFINE, DEF, OUT, IN, IF, THEN, ELSE, TRUE, FALSE, AS, INCLUDE)
    override val symbols = List(COLONEQ, COLON, COMMA, LPAREN, RPAREN, LBRACE, RBRACE, PERCENT,
      LSHIFT, RSHIFT, GEQ, LEQ, NEQ, EQEQ, EQ, LT, GT, AND, OR, BITFLIP, BITAND, BITOR, BITXOR, PLUS, MINUS, TIMES,
      SLASH, BANG)
    override val comments = List("--" -> "\n", "#" -> "\n")

    override def isIdentifierCont(c: Char): Boolean = super.isIdentifierCont(c) || c == '.'
  }

  import Tokens._

  override val tokenizer = Tokenizer

  class Parsers(path: String) {

    def spec: Parser[Tessla.Spec] = include.* ~ statement.* ^^ {
      case (includes, statements) =>
        Tessla.Spec(includes.flatMap(_.statements) ++ statements)
    }

    def include = INCLUDE ~> exprStringLit ^^ { file =>
      import java.nio.file.Paths
      val includePath = Paths.get(path).getParent.resolve(file.value)
      TesslaParser.translateSpec(TesslaSource.fromFile(includePath.toString))
    }

    // TODO identifier completion, requires some small compacom enhancements
    def identifier: Parser[Tessla.Identifier] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => Tessla.Identifier(name, SourceLoc(loc, path))
    }

    def define =
      DEFINE ^^^! {
        loc => warn(SourceLoc(loc, path), "The keyword 'define' is deprecated, use 'def' instead.")
      } | DEF

    def defOrMacroDef =
      (define ~> identifier ~ macroArgs.? ~ typeAscr.? ~ (COLONEQ ~> expr)) ^^! {
        case (loc, (((name, args), typeAscr), expr)) =>
          Tessla.Def(name, args getOrElse Seq(), typeAscr, expr, SourceLoc(loc, path))
      }

    def statement: Parser[Tessla.Statement] =
      defOrMacroDef |
        OUT ~> outstatement |
        IN ~> identifier ~ typeAscr ^^! {
          case (loc, (name, typeAscr)) =>
            Tessla.In(name, typeAscr, SourceLoc(loc, path))
        }

    def outstatement: Parser[Tessla.Statement] =
      TIMES ^^^! {
        loc => Tessla.OutAll(SourceLoc(loc, path))
      } |
        expr ~ (AS ~> identifier).? ^^! {
          case (loc, (expr, name)) =>
            Tessla.Out(expr, name, SourceLoc(loc, path))
        }

    def macroArgs: Parser[Seq[Tessla.MacroArg]] = LPAREN ~> rep1sep(macroArg, COMMA) <~ RPAREN

    def macroArg: Parser[Tessla.MacroArg] = (identifier ~ typeAscr.?) ^^ Tessla.MacroArg.tupled

    def typeAscr: Parser[Tessla.Type] = COLON ~> `type`

    def `type`: Parser[Tessla.Type] = typeNameOrApp

    def typeNameOrApp: Parser[Tessla.Type] = identifier ~ typeAppArgs.? ^^! {
      case (_, (name, None)) => Tessla.TypeName(name)
      case (loc, (name, Some(args))) => Tessla.TypeApp(name, args, SourceLoc(loc, path))
    }

    def typeAppArgs: Parser[Seq[Tessla.Type]] = LT ~> rep1sep(`type`, COMMA) <~ GT

    def expr: Parser[Tessla.Expr] = ifThenElse | typedExpression

    def ifThenElse = (IF ~ expr) ~ (THEN ~> expr) ~ (ELSE ~> expr).? ^^! {
      case (loc, (((ifToken, cond), thenCase), Some(elseCase))) =>
        Tessla.ExprApp(Tessla.Identifier("if then else", SourceLoc(ifToken.loc, path)), List(Tessla.PosArg(cond), Tessla.PosArg(thenCase), Tessla.PosArg(elseCase)), SourceLoc(loc, path))
      case (loc, (((ifToken, cond), thenCase), None)) =>
        Tessla.ExprApp(Tessla.Identifier("if then", SourceLoc(ifToken.loc, path)), List(Tessla.PosArg(cond), Tessla.PosArg(thenCase)), SourceLoc(loc, path))
    }

    def typedExpression: Parser[Tessla.Expr] = infixExpr ~ typeAscr.? ^^ {
      case (expr, None) => expr
      case (expr, Some(typeAscr)) => Tessla.ExprTypeAscr(expr, typeAscr)
    }

    def infixOp(loc: compacom.Location, lhs: Tessla.Expr, rhss: Seq[(WithLocation[Token], Tessla.Expr)]) = {
      rhss.foldLeft(lhs) {
        case (l, (op, r)) =>
          Tessla.ExprApp(Tessla.Identifier(op.value.string, SourceLoc(op.loc, path)), List(Tessla.PosArg(l), Tessla.PosArg(r)), SourceLoc(loc, path))
      }
    }

    def infixExpr: Parser[Tessla.Expr] = conjunction ~ (OR ~ conjunction).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def conjunction: Parser[Tessla.Expr] = comparison ~ (AND ~ comparison).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def comparison: Parser[Tessla.Expr] = bitOrExpr ~ ((EQEQ | LT | GT | LEQ | GEQ | NEQ) ~ bitOrExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitOrExpr: Parser[Tessla.Expr] = bitAndExpr ~ ((BITOR | BITXOR) ~ bitAndExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitAndExpr: Parser[Tessla.Expr] = bitShiftExpr ~ (BITAND ~ bitShiftExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitShiftExpr: Parser[Tessla.Expr] = additiveExpr ~ ((LSHIFT | RSHIFT) ~ additiveExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def additiveExpr: Parser[Tessla.Expr] = multiplicativeExpr ~ ((PLUS | MINUS) ~ multiplicativeExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def multiplicativeExpr: Parser[Tessla.Expr] = unaryExpr ~ ((TIMES | SLASH) ~ unaryExpr).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def unaryExpr: Parser[Tessla.Expr] =
      BANG ~ exprAtomic ^^! {
        case (loc, (op, expr)) =>
          Tessla.ExprApp(Tessla.Identifier("!", SourceLoc(op.loc, path)), List(Tessla.PosArg(expr)), SourceLoc(loc, path))
      } |
        BITFLIP ~ exprAtomic ^^! {
          case (loc, (op, expr)) =>
            Tessla.ExprApp(Tessla.Identifier("~", SourceLoc(op.loc, path)), List(Tessla.PosArg(expr)), SourceLoc(loc, path))
        } |
        MINUS ~ exprAtomic ^^! {
          case (loc, (op, expr)) =>
            Tessla.ExprApp(Tessla.Identifier("-", SourceLoc(op.loc, path)), List(Tessla.PosArg(expr)), SourceLoc(loc, path))
        } |
        exprAtomic

    def exprAtomic: Parser[Tessla.Expr] = exprLit | exprGroup | exprBlock | exprNameOrApp

    def exprGroup: Parser[Tessla.Expr] = (LPAREN ~> expr.? <~ RPAREN) ^^! {
      case (_, Some(expr)) => expr
      case (loc, None) => Tessla.ExprUnit(SourceLoc(loc, path))
    }

    def exprBlock = (LBRACE ~> defOrMacroDef.* ~ expr <~ RBRACE) ^^! {
      case (loc, (statements, expr)) =>
        Tessla.ExprBlock(statements, expr, SourceLoc(loc, path))
    }

    def exprNameOrApp: Parser[Tessla.Expr] = identifier ~ exprAppArgs.? ^^! {
      case (_, (name, None)) => Tessla.ExprName(name)
      case (loc, (name, Some(args))) => Tessla.ExprApp(name, args, SourceLoc(loc, path))
    }

    def exprLit: Parser[Tessla.Expr] = exprIntLit | exprStringLit | exprBoolLit

    def exprBoolLit: Parser[Tessla.ExprBoolLit] =
      TRUE ^^^! {
        loc => Tessla.ExprBoolLit(true, SourceLoc(loc, path))
      } |
        FALSE ^^^! {
          loc => Tessla.ExprBoolLit(false, SourceLoc(loc, path))
        }

    def exprIntLit: Parser[Tessla.Expr] = matchToken("integer", Set("<integer>")) {
      case WithLocation(_, INT(value)) => BigInt(value)
    } ~ timeUnit.? ^^! {
      case (loc, (value, None)) => Tessla.ExprIntLit(value, SourceLoc(loc, path))
      case (loc, (value, Some(unit))) => Tessla.ExprTimeLit(value, unit, SourceLoc(loc, path))
    }

    def timeUnit: Parser[TimeUnit.TimeUnit] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => TimeUnit.fromString(name, SourceLoc(loc, path))
    }

    def exprStringLit: Parser[Tessla.ExprStringLit] = matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(value)) => Tessla.ExprStringLit(value, SourceLoc(loc, path))
    }

    def exprAppArgs: Parser[Seq[Tessla.AppArg]] = LPAREN ~> rep1sep(exprAppArg, COMMA) <~ RPAREN

    def namedArgAssignmentOperator =
      COLONEQ ^^^! {
        loc => warn(SourceLoc(loc, path), "Using ':=' for named arguments is deprecated, use '=' instead.")
      } | EQ

    def exprAppArg: Parser[Tessla.AppArg] = expr ~^ {
      case (x@Tessla.ExprName(name)) =>
        namedArgAssignmentOperator ~> expr ^^ (Tessla.NamedArg(name, _)) | success(Tessla.PosArg(x))
      case x => success(Tessla.PosArg(x))
    }
  }
}

package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.compacom
import de.uni_luebeck.isp.tessla.Errors.ParserError

class TesslaParser extends TranslationPhase[TesslaSource, Tessla.Specification] with Parsers {
  override def translateSpec(source: TesslaSource) = {
    val p =  new Parsers(source.path)
    parseAll(p.spec, source.src) match {
      case Success(_, spec, _, _) => spec
      case fail: Failure => throw ParserError(fail.message, Location(fail.loc, source.path))
    }
  }

  object Tokens extends SimpleTokens {

    case object DEFINE extends Token("define")

    case object DEF extends Token("def")

    case object OUT extends Token("out")

    case object IN extends Token("in")

    case object STATIC extends Token("static")

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

    case object LBRACKET extends Token("[")

    case object RBRACKET extends Token("]")

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

    case object ROCKET extends Token("=>")

    case object EQ extends Token("=")

    case object ANDAND extends Token("&&")

    case object PIPEPIPE extends Token("||")

    case object TILDE extends Token("~")

    case object AND extends Token("&")

    case object PIPE extends Token("|")

    case object HAT extends Token("^")

    case object PLUS extends Token("+")

    case object MINUS extends Token("-")

    case object STAR extends Token("*")

    case object SLASH extends Token("/")

    case object BANG extends Token("!")

    case object AT extends Token("@")

    case object INCLUDE extends Token("include")

  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens

    import tokens._

    override val keywords = List(DEFINE, DEF, OUT, IN, STATIC, IF, THEN, ELSE, TRUE, FALSE, AS, INCLUDE)
    override val symbols = List(COLONEQ, COLON, COMMA, LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE, PERCENT,
      LSHIFT, RSHIFT, GEQ, LEQ, NEQ, EQEQ, ROCKET, EQ, LT, GT, ANDAND, PIPEPIPE, TILDE, AND, PIPE, HAT, PLUS, MINUS,
      STAR, SLASH, BANG, AT)
    override val comments = List("--" -> "\n", "#" -> "\n")

    override def isIdentifierCont(c: Char): Boolean = super.isIdentifierCont(c) || c == '.'
  }

  import Tokens._

  override val tokenizer = Tokenizer

  class Parsers(path: String) {

    def spec: Parser[Tessla.Specification] = include.* ~ statement.* ^^ {
      case (includes, statements) =>
        Tessla.Specification(includes.flatMap(_.statements) ++ statements)
    }

    def include = INCLUDE ~> stringLiteral ^^ { file =>
      import java.nio.file.Paths
      // getParent returns null for relative paths without subdirectories (i.e. just a file name), which is
      // annoying and stupid. So we wrap the call in an option and fall back to "." as the default.
      val dir = Option(Paths.get(path).getParent).getOrElse(Paths.get("."))
      val includePath = dir.resolve(file.value)
      new TesslaParser().translateSpec(TesslaSource.fromFile(includePath.toString))
    }

    // TODO identifier completion, requires some small compacom enhancements
    def identifier: Parser[Tessla.Identifier] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => Tessla.Identifier(name, Location(loc, path))
    }

    def define =
      DEFINE ^^^! {
        loc => warn(Location(loc, path), "The keyword 'define' is deprecated, use 'def' instead.")
      } | DEF

    def annotation = AT ~> identifier

    def definitionHeader =
      ((annotation.* <~ define) ~ identifier ~ typeParameters.? ~ parameters.? ~ typeAnnotation.?) ^^! { (loc, ast) => (loc, ast) }

    def definition =
      (definitionHeader ~ (COLONEQ ~> expression)) ^^ {
        case ((loc, ((((annotations, name), typeParams), params), returnType)), expr) =>
          Tessla.Definition(annotations, name, typeParams.getOrElse(Seq()), params.getOrElse(Seq()),
            returnType, expr, Location(loc, path))
      }

    def statement: Parser[Tessla.Statement] =
      definition |
        OUT ~> outStatement |
        IN ~> identifier ~ typeAnnotation ^^! {
          case (loc, (name, streamType)) =>
            Tessla.In(name, streamType, Location(loc, path))
        }

    def outStatement: Parser[Tessla.Statement] =
      STAR ^^^! {
        loc => Tessla.OutAll(Location(loc, path))
      } |
        expression ~ (AS ~> identifier).? ^^! {
          case (loc, (expr, name)) =>
            Tessla.Out(expr, name, Location(loc, path))
        }

    def parameters: Parser[Seq[Tessla.Parameter]] = LPAREN ~> rep1sep(parameter, COMMA) <~ RPAREN

    def parameter: Parser[Tessla.Parameter] = (identifier ~ typeAnnotation.?) ^^ Tessla.Parameter.tupled

    def typeAnnotation: Parser[Tessla.Type] = COLON ~> `type`

    def `type`: Parser[Tessla.Type] =
      identifier ~ typeArguments.? ^^! {
        case (_, (name, None)) => Tessla.SimpleType(name)
        case (loc, (name, Some(args))) => Tessla.TypeApplication(name, args, Location(loc, path))
      } |
        (LPAREN ~> repsep(`type`, COMMA) <~ RPAREN) ~ (ROCKET ~> `type`) ^^! {
          case (loc, (parameterTypes, returnType)) =>
            Tessla.FunctionType(parameterTypes, returnType, Location(loc, path))
        }


    def typeArguments: Parser[Seq[Tessla.Type]] = LBRACKET ~> rep1sep(`type`, COMMA) <~ RBRACKET

    def typeParameters: Parser[Seq[Tessla.Identifier]] = LBRACKET ~> rep1sep(identifier, COMMA) <~ RBRACKET

    def expression: Parser[Tessla.Expression] = ifThenElse | staticIfThenElse | infixExpression

    def staticIfThenElse = (STATIC ~> IF ~> expression) ~ (THEN ~> expression) ~ (ELSE ~> expression) ^^! {
      case (loc, ((cond, thenCase), elseCase)) =>
        Tessla.StaticIfThenElse(cond, thenCase, elseCase, Location(loc, path))
    }

    def ifThenElse = (IF ~ expression) ~ (THEN ~> expression) ~ (ELSE ~> expression).? ^^! {
      case (loc, (((ifToken, cond), thenCase), Some(elseCase))) =>
        Tessla.MacroCall(Tessla.Identifier("if then else", Location(ifToken.loc, path)),
          Seq(),
          Seq(Tessla.PositionalArgument(cond), Tessla.PositionalArgument(thenCase), Tessla.PositionalArgument(elseCase)),
          Location(loc, path))
      case (loc, (((ifToken, cond), thenCase), None)) =>
        Tessla.MacroCall(
          Tessla.Identifier("if then", Location(ifToken.loc, path)),
          Seq(),
          Seq(Tessla.PositionalArgument(cond), Tessla.PositionalArgument(thenCase)), Location(loc, path))
    }

    def infixOp(loc: compacom.Location, lhs: Tessla.Expression, rhss: Seq[(WithLocation[Token], Tessla.Expression)]) = {
      rhss.foldLeft(lhs) {
        case (l, (op, r)) =>
          Tessla.MacroCall(Tessla.Identifier(op.value.string, Location(op.loc, path)),
            Seq(),
            Seq(Tessla.PositionalArgument(l), Tessla.PositionalArgument(r)),
            Location(loc, path))
      }
    }

    def infixExpression: Parser[Tessla.Expression] = conjunction ~ (PIPEPIPE ~ conjunction).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def conjunction: Parser[Tessla.Expression] = comparison ~ (ANDAND ~ comparison).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def comparisonOperator: Parser[WithLocation[Token]] = EQEQ | LT | GT | LEQ | GEQ | NEQ

    def comparison: Parser[Tessla.Expression] = bitOrExpression ~ (comparisonOperator ~ bitOrExpression).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitOrExpression: Parser[Tessla.Expression] = bitAndExpression ~ ((PIPE | HAT) ~ bitAndExpression).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitAndExpression: Parser[Tessla.Expression] = bitShiftExpression ~ (AND ~ bitShiftExpression).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def bitShiftExpression: Parser[Tessla.Expression] = additiveExpression ~ ((LSHIFT | RSHIFT) ~ additiveExpression).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def additiveExpression: Parser[Tessla.Expression] = multiplicativeExpression ~ ((PLUS | MINUS) ~ multiplicativeExpression).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def multiplicativeExpression: Parser[Tessla.Expression] = unaryExpression ~ ((STAR | SLASH) ~ unaryExpression).* ^^! {
      case (loc, (lhs, rhss)) => infixOp(loc, lhs, rhss)
    }

    def unaryExpression: Parser[Tessla.Expression] =
      BANG ~ atomicExpression ^^! {
        case (loc, (op, expr)) =>
          Tessla.MacroCall(Tessla.Identifier("!", Location(op.loc, path)),
            Seq(),
            Seq(Tessla.PositionalArgument(expr)),
            Location(loc, path))
      } |
        TILDE ~ atomicExpression ^^! {
          case (loc, (op, expr)) =>
            Tessla.MacroCall(Tessla.Identifier("~", Location(op.loc, path)),
              Seq(),
              Seq(Tessla.PositionalArgument(expr)),
              Location(loc, path))
        } |
        MINUS ~ atomicExpression ^^! {
          case (loc, (op, expr)) =>
            Tessla.MacroCall(Tessla.Identifier("unary -", Location(op.loc, path)),
              Seq(),
              Seq(Tessla.PositionalArgument(expr)),
              Location(loc, path))
        } |
        atomicExpression

    def atomicExpression: Parser[Tessla.Expression] = literal | group | block | variableOrMacroCall

    def group: Parser[Tessla.Expression] = (LPAREN ~> expression.? <~ RPAREN) ^^! {
      case (_, Some(expr)) => expr
      case (loc, None) => Tessla.Literal(Tessla.Unit, Location(loc, path))
    }

    def block = (LBRACE ~> definition.* ~ expression <~ RBRACE) ^^! {
      case (loc, (statements, expr)) =>
        Tessla.Block(statements, expr, Location(loc, path))
    }

    def variableOrMacroCall: Parser[Tessla.Expression] = identifier ~ typeArguments.? ~ arguments.? ^^! {
      case (_, ((name, None), None)) =>
        Tessla.Variable(name)
      case (loc, ((name, typeArgs), args)) =>
        Tessla.MacroCall(name, typeArgs.getOrElse(Seq()), args.getOrElse(Seq()), Location(loc, path))
    }

    def literal: Parser[Tessla.Expression] = (intLiteral | stringLiteral | boolLiteral) ^^! {
      case (loc, lit) => Tessla.Literal(lit, Location(loc, path))
    }

    def boolLiteral: Parser[Tessla.BoolLiteral] =
      TRUE ^^^ Tessla.BoolLiteral(true) |
      FALSE ^^^ Tessla.BoolLiteral(false)

    def intLiteral: Parser[Tessla.LiteralValue] = matchToken("integer", Set("<integer>")) {
      case WithLocation(_, INT(value)) => BigInt(value)
    } ~ timeUnit.? ^^ {
      case (value, None) => Tessla.IntLiteral(value)
      case (value, Some(unit)) => Tessla.TimeLiteral(value, unit)
    }

    def timeUnit: Parser[TimeUnit] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => TimeUnit.fromString(name, Location(loc, path))
    }

    def stringLiteral: Parser[Tessla.StringLiteral] = matchToken("string", Set("<string>")) {
      case WithLocation(_, STRING(value)) => Tessla.StringLiteral(value)
    }

    def arguments: Parser[Seq[Tessla.Argument]] = LPAREN ~> rep1sep(argument, COMMA) <~ RPAREN

    def argument: Parser[Tessla.Argument] = expression ~^ {
      case (x@Tessla.Variable(name)) =>
        EQ ~> expression ^^ (Tessla.NamedArgument(name, _)) | success(Tessla.PositionalArgument(x))
      case x => success(Tessla.PositionalArgument(x))
    }
  }
}

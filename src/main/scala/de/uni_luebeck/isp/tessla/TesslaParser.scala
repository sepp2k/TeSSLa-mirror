package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.compacom
import de.uni_luebeck.isp.tessla.Errors.{MissingBody, ParserError}

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

    case object TYPE extends Token("type")

    case object OUT extends Token("out")

    case object IN extends Token("in")

    case object STATIC extends Token("static")

    case object IF extends Token("if")

    case object THEN extends Token("then")

    case object ELSE extends Token("else")

    case object AS extends Token("as")

    case object TRUE extends Token("true")

    case object FALSE extends Token("false")

    case object INCLUDE extends Token("include")

    case object COLON extends Token(":")

    case object PERCENT extends Token("%")

    case object COLONEQ extends Token(":=")

    case object COMMA extends Token(",")

    case object LPAREN extends Token("(")

    case object RPAREN extends Token(")")

    case object LBRACKET extends Token("[")

    case object RBRACKET extends Token("]")

    case object DOLLARBRACE extends Token("${")

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

    case object DOT extends Token(".")

  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens

    import tokens._

    override val keywords = List(DEFINE, DEF, TYPE, OUT, IN, STATIC, IF, THEN, ELSE, TRUE, FALSE, AS, INCLUDE)
    override val symbols = List(COLONEQ, COLON, COMMA, LPAREN, RPAREN, LBRACKET, RBRACKET, DOLLARBRACE, LBRACE, RBRACE,
      PERCENT, LSHIFT, RSHIFT, GEQ, LEQ, NEQ, EQEQ, ROCKET, EQ, LT, GT, ANDAND, PIPEPIPE, TILDE, AND, PIPE, HAT, PLUS,
      MINUS, STAR, SLASH, BANG, AT, DOT)
    override val comments = List("--" -> "\n", "#" -> "\n")

    override def isIdentifierCont(c: Char): Boolean = super.isIdentifierCont(c)
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
      DEFINE ^^! { (loc, tok) =>
        warn(Location(loc, path), "The keyword 'define' is deprecated, use 'def' instead.")
        tok
      } | DEF

    def annotation = AT ~> identifier ^^! { (loc, id) => Tessla.Identifier(id.name, Location(loc, path)) }

    def definitionHeader =
      (annotation.* ~ define) ~ identifier ~ typeParameters.? ~ parameters.? ~ typeAnnotation.? ^^! {(loc, ast) =>
        (loc, ast)
      }

    def definition =
      (definitionHeader ~ (COLONEQ ~> expression)) ^^ {
        case ((brokenHeaderLoc, (((((annotations, defToken), name), typeParams), params), returnType)), expr) =>
          val startLoc = annotations match {
            case Seq() => Location(defToken.loc, path)
            case firstAnnotation +: _ => firstAnnotation.loc
          }
          val loc = startLoc.merge(expr.loc)
          val headerLoc = startLoc.merge(Location(compacom.Location(brokenHeaderLoc.to, brokenHeaderLoc.to), path))
          Tessla.Definition(annotations, name, typeParams.getOrElse(Seq()), params.getOrElse(Seq()),
            returnType, headerLoc, expr, loc)
      }

    def typeDefinition = TYPE ~> (identifier ~ typeParameters.?) ~ (COLONEQ ~> `type`) ^^! {
      case (loc, ((id, typeParametersOpt), body)) =>
        Tessla.TypeDefinition(id, typeParametersOpt.getOrElse(Seq()), body, Location(loc, path))
    }

    def statement: Parser[Tessla.Statement] =
      definition |
        typeDefinition |
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
        (LPAREN ~> repsep(`type`, COMMA) <~ RPAREN) ~ (ROCKET ~> `type`).? ^^! {
          case (loc, (parameterTypes, Some(returnType))) =>
            Tessla.FunctionType(parameterTypes, returnType, Location(loc, path))
          case (loc, (elementTypes, None)) =>
            Tessla.TupleType(elementTypes, Location(loc, path))
        } |
        DOLLARBRACE ~> repsep(identifier ~ (COLON ~> `type`), COMMA) <~ RBRACE ^^! {
          (loc, members) =>
            Tessla.ObjectType(members, Location(loc, path))
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
        Tessla.MacroCall(
          Tessla.Variable(Tessla.Identifier("if then else", Location(ifToken.loc, path))),
          Seq(),
          Seq(Tessla.PositionalArgument(cond), Tessla.PositionalArgument(thenCase), Tessla.PositionalArgument(elseCase)),
          Location(loc, path))
      case (loc, (((ifToken, cond), thenCase), None)) =>
        Tessla.MacroCall(
          Tessla.Variable(Tessla.Identifier("if then", Location(ifToken.loc, path))),
          Seq(),
          Seq(Tessla.PositionalArgument(cond), Tessla.PositionalArgument(thenCase)), Location(loc, path))
    }

    def infixOp(lhs: Tessla.Expression, rhss: Seq[(WithLocation[Token], Tessla.Expression)]) = {
      rhss.foldLeft(lhs) {
        case (l, (op, r)) =>
          Tessla.MacroCall(Tessla.Variable(Tessla.Identifier(op.value.string, Location(op.loc, path))),
            Seq(),
            Seq(Tessla.PositionalArgument(l), Tessla.PositionalArgument(r)),
            l.loc.merge(r.loc))
      }
    }

    def infixExpression: Parser[Tessla.Expression] = conjunction ~ (PIPEPIPE ~ conjunction).* ^^ {
      case (lhs, rhss) => infixOp(lhs, rhss)
    }

    def conjunction: Parser[Tessla.Expression] = comparison ~ (ANDAND ~ comparison).* ^^ {
      case (lhs, rhss) => infixOp(lhs, rhss)
    }

    def comparisonOperator: Parser[WithLocation[Token]] = EQEQ | LT | GT | LEQ | GEQ | NEQ

    def comparison: Parser[Tessla.Expression] = bitOrExpression ~ (comparisonOperator ~ bitOrExpression).* ^^ {
      case (lhs, rhss) => infixOp(lhs, rhss)
    }

    def bitOrExpression: Parser[Tessla.Expression] = bitAndExpression ~ ((PIPE | HAT) ~ bitAndExpression).* ^^ {
      case (lhs, rhss) => infixOp(lhs, rhss)
    }

    def bitAndExpression: Parser[Tessla.Expression] = bitShiftExpression ~ (AND ~ bitShiftExpression).* ^^ {
      case (lhs, rhss) => infixOp(lhs, rhss)
    }

    def bitShiftExpression: Parser[Tessla.Expression] = additiveExpression ~ ((LSHIFT | RSHIFT) ~ additiveExpression).* ^^ {
      case (lhs, rhss) => infixOp(lhs, rhss)
    }

    def additiveExpression: Parser[Tessla.Expression] = multiplicativeExpression ~ ((PLUS | MINUS) ~ multiplicativeExpression).* ^^ {
      case (lhs, rhss) => infixOp(lhs, rhss)
    }

    def multiplicativeExpression: Parser[Tessla.Expression] = unaryExpression ~ ((STAR | SLASH) ~ unaryExpression).* ^^ {
      case (lhs, rhss) => infixOp(lhs, rhss)
    }

    def unaryExpression: Parser[Tessla.Expression] =
      BANG ~ unaryExpression ^^! {
        case (loc, (op, expr)) =>
          Tessla.MacroCall(Tessla.Variable(Tessla.Identifier("!", Location(op.loc, path))),
            Seq(),
            Seq(Tessla.PositionalArgument(expr)),
            Location(loc, path))
      } |
        TILDE ~ unaryExpression ^^! {
          case (loc, (op, expr)) =>
            Tessla.MacroCall(Tessla.Variable(Tessla.Identifier("~", Location(op.loc, path))),
              Seq(),
              Seq(Tessla.PositionalArgument(expr)),
              Location(loc, path))
        } |
        MINUS ~ unaryExpression ^^! {
          case (loc, (op, expr)) =>
            Tessla.MacroCall(Tessla.Variable(Tessla.Identifier("unary -", Location(op.loc, path))),
              Seq(),
              Seq(Tessla.PositionalArgument(expr)),
              Location(loc, path))
        } |
        postfixExpression

    def postfixExpression: Parser[Tessla.Expression] = atomicExpression ~ postfixOperator.* ^^ {
      case (exp, postOps) =>
        postOps.foldLeft(exp) { (exp, op) => op(exp) }
    }

    def postfixOperator =
      typeArguments ~ arguments.? ^^! {
        case (loc, (typeArgs, argsOpt)) => (exp: Tessla.Expression) =>
            Tessla.MacroCall(exp, typeArgs, argsOpt.getOrElse(Seq()), exp.loc.merge(Location(loc, path)))
      } |
        arguments ^^! {
          (loc, args) => (exp: Tessla.Expression) =>
            Tessla.MacroCall(exp, Seq(), args, exp.loc.merge(Location(loc, path)))
        } |
        DOT ~> identifier ^^ {
          id => (exp: Tessla.Expression) =>
            Tessla.MemberAccess(exp, id, exp.loc.merge(id.loc))
        }

    def atomicExpression: Parser[Tessla.Expression] = literal | group | block | objectLiteral | variable

    def group: Parser[Tessla.Expression] = LPAREN ~> (repsep(expression, COMMA) ~ COMMA.?) <~ RPAREN ^^! {
      case (_, (Seq(expr), None)) => expr
      case (loc, (Seq(), _)) => Tessla.Literal(Tessla.Unit, Location(loc, path))
      case (loc, (elements, _)) => Tessla.Tuple(elements, Location(loc, path))
    }

    def block = (LBRACE ~> definition.* ~ expression <~ RBRACE) ^^! {
      case (loc, (statements, expr)) =>
        Tessla.Block(statements, expr, Location(loc, path))
    }

    def objectLiteral = DOLLARBRACE ~> repsep(memberDefinition, COMMA) <~ COMMA.? <~ RBRACE ^^! { (loc, members) =>
        Tessla.ObjectLiteral(members, Location(loc, path))
    }

    def memberHeader = annotation.* ~ identifier ~ typeParameters.? ~ parameters.? ~ typeAnnotation.? ^^! { (loc, ast) =>
      (loc, ast)
    }

    def memberDefinition = memberHeader ~ (EQ ~> expression).? ^^! {
      case (loc, ((headerLoc, ((((annotations, id), typeParams), params), typeOpt)), Some(body))) =>
        Tessla.MemberDefinition.Full(Tessla.Definition(
          annotations, id, typeParams.getOrElse(Seq()), params.getOrElse(Seq()), typeOpt, Location(headerLoc, path),
          body, Location(loc, path)))
      case (_, ((_, ((((Seq(), id), None), None), None)), None)) =>
        Tessla.MemberDefinition.Simple(id)
      case (_, ((_, ((((_, id), _), _), _)), _)) =>
        error(MissingBody(id))
        null
    }

    def variable: Parser[Tessla.Expression] = identifier ^^ Tessla.Variable

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
      case x@Tessla.Variable(name) =>
        EQ ~> expression ^^ (Tessla.NamedArgument(name, _)) | success(Tessla.PositionalArgument(x))
      case x => success(Tessla.PositionalArgument(x))
    }
  }
}

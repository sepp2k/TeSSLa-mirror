package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Location, TesslaCore, TesslaSource, TimeUnit}
import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.tessla.Errors.{NotAnEventError, ParserError}
import de.uni_luebeck.isp.tessla.TimeUnit._

object TraceParser extends Parsers {
  def parseTrace(tesslaSource: TesslaSource): Trace = {
    def eventsOnly(lines: Iterator[Either[TimeUnit, Trace.Event]]): Iterator[Trace.Event] =
      lines.map{
        case Left(tu) => throw NotAnEventError(tu.toString, tu.loc)
        case Right(ev) => ev
      }

    val input = parseMany(new Parsers(tesslaSource.path).line, tesslaSource.src).map{
      case Success(_, line, _, _) => line
      case fail: Failure => throw ParserError(fail.message, Location(fail.loc, tesslaSource.path))
    }

    input.take(1).toList.headOption match {
      case Some(Left(tu)) => new Trace(Some(tu), eventsOnly(input))
      case Some(Right(ev)) => new Trace(None, Iterator(ev) ++ eventsOnly(input))
      case None => new Trace(None, Iterator())
    }
  }

  object Tokens extends SimpleTokens {

    case object COLON extends Token(":")

    case object EQ extends Token("=")

    case object DOLLAR extends Token("$")

    case object TRUE extends Token("true")

    case object FALSE extends Token("false")

    case object LPAREN extends Token("(")

    case object RPAREN extends Token(")")

    case object MINUS extends Token("-")

  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens

    import tokens._

    override val keywords = List(TRUE, FALSE)
    override val symbols = List(COLON, EQ, DOLLAR, LPAREN, RPAREN, MINUS)
    override val comments = List("--" -> "\n")

    override def isIdentifierCont(c: Char): Boolean = {
      super.isIdentifierCont(c)
    }
  }

  import Tokens._

  override val tokenizer = Tokenizer


  class Parsers(path: String) {
    def line: Parser[Either[TimeUnit, Trace.Event]] = (timeUnit | event) ^^ {
      case tu: TimeUnit => Left(tu)
      case ev: Trace.Event => Right(ev)
    }

    def event: Parser[Trace.Event] =
      (((timeStamp <~ COLON) ~ identifier) ~ (EQ ~> value).?) ^^! {
        case (loc, ((time, id), v)) =>
          Trace.Event(Location(loc, path), time, id, v.getOrElse(TesslaCore.Unit(Location(loc, path))))
      }

    def value: Parser[TesslaCore.LiteralValue] =
      TRUE ^^^! {
        loc => TesslaCore.BoolLiteral(true, Location(loc, path))
      } |
        FALSE ^^^! {
          loc => TesslaCore.BoolLiteral(false, Location(loc, path))
        } |
        LPAREN ~ RPAREN ^^^! {
          loc => TesslaCore.Unit(Location(loc, path))
        } |
        string ^^! {
          case (loc, value) => TesslaCore.StringLiteral(value, Location(loc, path))
        } |
        bigInt ^^! {
          case (loc, value) => TesslaCore.IntLiteral(value, Location(loc, path))
        }

    def timeUnit: Parser[TimeUnit.TimeUnit] = DOLLAR ~> ID("timeunit") ~> EQ ~> matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(name)) => TimeUnit.fromString(name, Location(loc, path))
    }

    def timeStamp = bigInt ^^! {
      case (loc, i) =>
        Trace.TimeStamp(Location(loc, path), i)
    }

    def bigInt: Parser[BigInt] =
      MINUS.? ~ bigNat ^^ {
        case (Some(_), nat) => -nat
        case (None, nat) => nat
      }

    def bigNat: Parser[BigInt] =
      matchToken("integer", Set("<integer>")) {
        case WithLocation(_, INT(value)) => BigInt(value)
      }

    def identifier: Parser[Trace.Identifier] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => Trace.Identifier(Location(loc, path), name)
    }

    def string: Parser[String] = matchToken("string", Set("<string>")) {
      case WithLocation(_, STRING(value)) => value
    }
  }

}

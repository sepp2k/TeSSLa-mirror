package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{SourceLoc, TesslaCore, TesslaSource, TimeUnit}
import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.tessla.Errors.{NotAnEventError, ParserError}
import de.uni_luebeck.isp.tessla.TimeUnit._

import scala.io.Source

object TracesParser extends Parsers {
  def parseTraces(tesslaSource: TesslaSource): Traces = {
    def eventsOnly(lines: Iterator[Traces.Line]): Iterator[Traces.Event] = lines.map {
      case e: Traces.Event => e
      case l => throw NotAnEventError(l)
    }

    val input = parseMany(new Parsers(tesslaSource.path).line, tesslaSource.src).map({
      case Success(_, line, _, _) => line
      case fail: Failure => throw ParserError(fail.message, SourceLoc(fail.loc, tesslaSource.path))
    })

    input.take(1).toList.headOption match {
      case Some(tu: Traces.TimeUnit) => new Traces(Some(tu), eventsOnly(input))
      case Some(ev: Traces.Event) => new Traces(None, eventsOnly(Iterator(ev) ++ input))
      case None => new Traces(None, Iterator())
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
    def line: Parser[Traces.Line] = event | timeUnitDecl

    def event: Parser[Traces.Event] =
      (((bigInt <~ COLON) ~ identifier) ~ (EQ ~> value).?) ^^! {
        case (loc, ((time, id), v)) =>
          Traces.Event(SourceLoc(loc, path), time, id, v.getOrElse(TesslaCore.Unit(SourceLoc(loc, path))))
      }

    def value: Parser[TesslaCore.LiteralValue] =
      TRUE ^^^! {
        loc => TesslaCore.BoolLiteral(true, SourceLoc(loc, path))
      } |
        FALSE ^^^! {
          loc => TesslaCore.BoolLiteral(false, SourceLoc(loc, path))
        } |
        LPAREN ~ RPAREN ^^^! {
          loc => TesslaCore.Unit(SourceLoc(loc, path))
        } |
        string ^^! {
          case (loc, value) => TesslaCore.StringLiteral(value, SourceLoc(loc, path))
        } |
        bigInt ^^! {
          case (loc, value) => TesslaCore.IntLiteral(value, SourceLoc(loc, path))
        }

    def timeUnitDecl: Parser[Traces.TimeUnit] =
      DOLLAR ~> ID("timeunit") ~> EQ ~> timeUnit ^^! {
        case (loc, unit) => Traces.TimeUnit(SourceLoc(loc, path), unit)
      }

    def timeUnit: Parser[TimeUnit.TimeUnit] = matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(name)) => TimeUnit.fromString(name, SourceLoc(loc, path))
    }

    def bigInt: Parser[BigInt] =
      MINUS.? ~ bigNat ^^ {
        case (Some(_), nat) => -nat
        case (None, nat) => nat
      }

    def bigNat: Parser[BigInt] =
      matchToken("integer", Set("<integer>")) {
        case WithLocation(loc, INT(value)) => BigInt(value)
      }

    def identifier: Parser[Traces.Identifier] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => Traces.Identifier(SourceLoc(loc, path), name)
    }

    def string: Parser[String] = matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(value)) => value
    }
  }

}

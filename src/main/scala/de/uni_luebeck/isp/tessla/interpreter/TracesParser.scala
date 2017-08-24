package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{CompilationError, SourceLoc, TesslaCore, TimeUnit}
import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.tessla.TimeUnit._
import de.uni_luebeck.isp.tessla.interpreter.Traces.NotAnEventError

import scala.io.Source

object TracesParser extends Parsers {
  case class ParserError(parserFailure: Failure) extends CompilationError {
    override def loc = SourceLoc(parserFailure.loc)

    override def message = parserFailure.message
  }

  def parseTraces(source: Source): Traces = {
    def eventsOnly(lines: Iterator[Traces.Line]): Iterator[Traces.Event] = lines.map {
      case e: Traces.Event => e
      case l => throw NotAnEventError(l)
    }

    val input = parseMany(line, source).map({
      case Success(_, line, _, _) => line
      case fail: Failure => throw ParserError(fail)
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

  def line: Parser[Traces.Line] = event | timeUnitDecl

  def event: Parser[Traces.Event] =
    (((bigInt <~ COLON) ~ identifier) ~ (EQ ~> value).?) ^^! {
      case (loc, ((time, id), v)) =>
        Traces.Event(SourceLoc(loc), time, id, v.getOrElse(TesslaCore.Unit(SourceLoc(loc))))
    }

  def value: Parser[TesslaCore.LiteralValue] =
    TRUE ^^^! {
      loc => TesslaCore.BoolLiteral(true, SourceLoc(loc))
    } |
      FALSE ^^^! {
        loc => TesslaCore.BoolLiteral(false, SourceLoc(loc))
      } |
      LPAREN ~ RPAREN ^^^! {
        loc => TesslaCore.Unit(SourceLoc(loc))
      } |
      string ^^! {
        case (loc, value) => TesslaCore.StringLiteral(value, SourceLoc(loc))
      } |
      bigInt ^^! {
        case (loc, value) => TesslaCore.IntLiteral(value, SourceLoc(loc))
      }

  def timeUnitDecl: Parser[Traces.TimeUnit] =
    DOLLAR ~> ID("timeunit") ~> EQ ~> timeUnit ^^! {
      case (loc, unit) => Traces.TimeUnit(SourceLoc(loc), unit)
    }

  def timeUnit: Parser[TimeUnit.TimeUnit] =
    STRING("ns") ^^^ {
      Nanos
    } |
      STRING("us") ^^^ {
        Micros
      } |
      STRING("ms") ^^^ {
        Millis
      } |
      STRING("s") ^^^ {
        Seconds
      } |
      STRING("min") ^^^ {
        Minutes
      } |
      STRING("h") ^^^ {
        Hours
      } |
      STRING("d") ^^^ {
        Days
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
    case WithLocation(loc, ID(name)) => Traces.Identifier(SourceLoc(loc), name)
  }

  def string: Parser[String] = matchToken("string", Set("<string>")) {
    case WithLocation(loc, STRING(value)) => value
  }
}

package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{CompilationError, SourceLoc, TesslaCore, TimeUnit}
import de.uni_luebeck.isp.compacom.{SimpleTokenizer, SimpleTokens, WithLocation, Parsers}
import de.uni_luebeck.isp.tessla.TimeUnit._

import scala.io.Source

/**
  * Created by Larissa on 19.08.2017.
  */
class TraceParser {

  case class ParserError(parserFailure: Parsers.Failure) extends CompilationError {
    override def loc = SourceLoc(parserFailure.loc)

    override def message = parserFailure.message
  }

  def translateSpec(source: Source): Seq[Input.Line] = {
    Parsers.parseMany(Parsers.line, source).map({
      case Parsers.Success(_, line, _, _) => line
      case fail: Parsers.Failure => throw ParserError(fail)
    }).toSeq
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

object Parsers extends Parsers {

  import scala.language.implicitConversions
  import Tokens._

  override val tokenizer = Tokenizer

  implicit def tokenToParser(t: Token): Parser[WithLocation[Token]] = token(t)

  def line: Parser[Input.Line] = event | timeUnitDecl

  def event: Parser[Input.Event] =
    (((bigInt <~ COLON) ~ identifier) ~ (EQ ~> value).?) ^^! {
      case (loc, ((time, id), v)) =>
        Input.Event(SourceLoc(loc), time, id, v.getOrElse(TesslaCore.Unit(SourceLoc(loc))))
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

  def timeUnitDecl: Parser[Input.TimeUnit] =
    DOLLAR ~> ID("timeunit") ~> EQ ~> timeUnit ^^! {
      case (loc, unit) => Input.TimeUnit(SourceLoc(loc), unit)
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

  def identifier: Parser[Input.Identifier] = matchToken("identifier", Set("<identifier>")) {
    case WithLocation(loc, ID(name)) => Input.Identifier(SourceLoc(loc), name)
  }

  def string: Parser[String] = matchToken("string", Set("<string>")) {
    case WithLocation(loc, STRING(value)) => value
  }
}

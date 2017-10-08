package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{SourceLoc, TesslaCore, TesslaSource, TimeUnit}
import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.tessla.Errors.{NotAnEventError, ParserError}
import de.uni_luebeck.isp.tessla.TimeUnit._
import de.uni_luebeck.isp.tessla.interpreter.Traces.Event

object TracesParser extends Parsers {
  def parseTraces(tesslaSource: TesslaSource): Traces = {
    def eventsOnly(lines: Iterator[Either[TimeUnit, Event]]): Iterator[Event] =
      lines.map {
        case Left(tu) => throw NotAnEventError(tu.toString, tu.loc)
        case Right(ev) => ev
      }

    val input = parseMany(new Parsers(tesslaSource.path).line, tesslaSource.src).map {
      case Success(_, line, _, _) => line
      case fail: Failure => throw ParserError(fail.message, SourceLoc(fail.loc, tesslaSource.path))
    }

    input.take(1).toList.headOption match {
      case Some(Left(tu)) => new Traces(Some(tu), eventsOnly(input))
      case Some(Right(ev)) => new Traces(None, Iterator(ev) ++ eventsOnly(input))
      case None => new Traces(None, Iterator())
    }
  }

  object Tokens extends SimpleTokens {

    case object COLON extends Token(":")

    case object SEMICOLON extends Token(";")

    case object EQ extends Token("=")

    case object DOLLAR extends Token("$")

    case object TRUE extends Token("true")

    case object FALSE extends Token("false")

    case object IN extends Token("in")

    case object LPAREN extends Token("(")

    case object RPAREN extends Token(")")

    case object MINUS extends Token("-")

    case object DDOT extends Token("..")

    case object LT extends Token("<")

    case object GT extends Token(">")

    case object LEQ extends Token("<=")

    case object GEQ extends Token(">=")

    case object UNDERSCORE extends Token("_")

    case object PLUSEQ extends Token("+=")

  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens

    import tokens._

    override val keywords = List(TRUE, FALSE, IN)
    override val symbols = List(COLON, SEMICOLON, EQ, DOLLAR, LPAREN, RPAREN, MINUS, DDOT, LEQ, GEQ, LT, GT, UNDERSCORE, PLUSEQ)
    override val comments = List("--" -> "\n")

    override def isIdentifierCont(c: Char): Boolean = {
      super.isIdentifierCont(c)
    }
  }

  import Tokens._

  override val tokenizer = Tokenizer


  class Parsers(path: String) {
    def line: Parser[Either[TimeUnit, Traces.Event]] = (timeUnit | event) ^^ {
      case tu: TimeUnit => Left(tu)
      case ev: Event => Right(ev)
    }

    def event: Parser[Traces.Event] =
      (((timeRange <~ COLON) ~ identifier) ~ (EQ ~> value).?) ^^! {
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

    def timeUnit: Parser[TimeUnit.TimeUnit] = DOLLAR ~> ID("timeunit") ~> EQ ~> matchToken("string", Set("<string>")) {
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

    def identifierOrWildcard: Parser[Traces.Identifier] =
      UNDERSCORE ^^^! {
        loc => Traces.Identifier(SourceLoc(loc, path), "_")
      } |
        identifier

    def timeRange: Parser[BigInt] = {
      def checkStep(id: Traces.Identifier, stepOpt: Option[(Traces.Identifier, BigInt)]) =
        stepOpt match {
          case Some((id2, value)) =>
            if (id2.name != id.name) {
              throw new Exception("TODO")
            }
            value
          case None => 1
        }

      (((bigNat ~ (LEQ | LT)) ~ identifierOrWildcard) ~ ((LEQ | LT) ~ bigNat).?) ~ step1.? ^^! {
        case (loc, (((lhs, id), rhs), stepOpt)) =>
          /*from < id < to
          from < id <= to
          from <= id < to
          from <= id <= to
          from < id (infinite)
          from <= id (infinite)*/
          val from = lhs match {
            case (value, WithLocation(_, LT)) => value + 1
            case (value, WithLocation(_, LEQ)) => value
          }
          val to = rhs match {
            case None => None
            case Some((WithLocation(_, LT), value)) => Some(value - 1)
            case Some((WithLocation(_, LEQ), value)) => Some(value)
          }
          val step = checkStep(id, stepOpt)
          println(s"from $from to ${to.getOrElse("infinity")} with step $step.")
          BigInt(1)
      }|
        (identifierOrWildcard ~ ((LEQ | LT | GEQ | GT) ~ bigNat)) ~ step1.? ^^! {
        case (loc, ((id, rhs), stepOpt)) =>
          /*id < to
          id <= to
          id > from
          id >= from*/
          val (from, to) = rhs match {
            case (WithLocation(_, LT), value) => (1, Some(value - 1))
            case (WithLocation(_, LEQ), value) => (1, Some(value))
            case (WithLocation(_, GT), value) => (value + 1, None)
            case (WithLocation(_, GEQ), value) => (value, None)
          }
          val step = checkStep(id, stepOpt)
          println(s"from $from to ${to.getOrElse("infinity")} with step $step.")
          BigInt(1)
      }|
        (((identifierOrWildcard <~ IN) ~ bigNat) <~ DDOT) ~ bigNat ^^! {
        case (loc, ((id, from), to)) =>
          /*id in from .. to*/
          BigInt(1)
      }|
        (identifierOrWildcard.? <~ DDOT) ~ bigNat ^^! {
        case (loc, (Some(id), to)) =>
          /* id .. to*/
          BigInt(1)
        case (loc, (None, to)) =>
          /* .. to*/
          BigInt(1)
      }|
        bigNat ~ (identifierOrWildcard.? <~ DDOT) ^^! {
        case (loc, (from, Some(id))) =>
          /* from .. id*/
          BigInt(1)
        case (loc, (from, None)) =>
          /* from ..*/
          BigInt(1)
      }
    }


    def step1: Parser[(Traces.Identifier, BigInt)] = (SEMICOLON ~> identifierOrWildcard) ~ (PLUSEQ ~> bigNat)

    def string: Parser[String] = matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(value)) => value
    }
  }

}

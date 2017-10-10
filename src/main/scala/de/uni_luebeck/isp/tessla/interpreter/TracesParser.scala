package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{SourceLoc, TesslaCore, TesslaSource, TimeUnit}
import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.tessla.Errors.{NotAnEventError, ParserError, TesslaError}
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

    case object COMMA extends Token(",")

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
    override val symbols = List(COLON, SEMICOLON, COMMA, EQ, DOLLAR, LPAREN, RPAREN, MINUS, DDOT, LEQ, GEQ, LT, GT, UNDERSCORE, PLUSEQ)
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
          println("TIME: "+time)
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

    def timeRange: Parser[Traces.TimeRange] = {
      bigNat ~ (((COMMA ~> bigNat) ~ (DDOT ~> identifierOrWildcard.?)) | (DDOT ~> ((bigNat ~ step2.?) | identifierOrWildcard.?)) | ((((LEQ | LT) ~ identifierOrWildcard) ~ ((LEQ | LT) ~ bigNat).?) ~ step1.?)).? ^^! {
        case (loc, (value, None)) =>
          /*value*/
          Traces.TimeRange(None, value, Some(value), 1)
        case (loc, (from: BigInt, Some((rhs: BigInt, rrhs: Option[BigInt])))) =>
          /*
          from .. to
          from .. to-step, to
          */
          Traces.TimeRange(None, from, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
        case (loc, (from: BigInt, Some(idOpt: Option[Traces.Identifier]))) =>
          /*
          from .. (infinite)
          from .. id (infinite)
          */
          Traces.TimeRange(idOpt, from, None, 1)
        case (loc, (valLeft, Some((valLeft2: BigInt, Some(id: Traces.Identifier))))) =>
          /*
          from, from+step .. id (infinite)
          */
          Traces.TimeRange(Some(id), valLeft, None, valLeft2 - valLeft)
        case (loc, (valLeft, Some((valLeft2: BigInt, None)))) =>
          /*
          from, from+step ..
          */
          Traces.TimeRange(None, valLeft, None, valLeft2 - valLeft)
        case (loc, (valLeft, Some((((op1: WithLocation[_], id: Traces.Identifier), rhs: Option[(WithLocation[_], BigInt)]), stepOpt: Option[(Option[Traces.Identifier], BigInt)])))) =>
          /*
          from < id < to
          from < id <= to
          from <= id < to
          from <= id <= to
          from < id (infinite)
          from <= id (infinite)
          Optional ; id += step
          */
          val from = op1 match {
            case WithLocation(_, LT) => valLeft + 1
            case WithLocation(_, LEQ) => valLeft
          }
          val to = rhs match {
            case None => None
            case Some((WithLocation(_, LT), value)) => Some(value - 1)
            case Some((WithLocation(_, LEQ), value)) => Some(value)
          }
          if (stepOpt.isDefined && id.name == "_"){
            /*Definition of a step while using a wildcard (e.g 1 < _ < 20; += 3) is not allowed.*/
            throw ParserError(s"Declaration of a step is not allowed when using '_'.", SourceLoc(loc, path))
          }
          val step : BigInt = stepOpt match {
            case None =>
              /*no step given, implicitly use 1*/
              1
            case Some((Some(id2), value)) =>
              /*step with notation 't += 5'*/
              if (id.name != id2.name && id2.name != "_"){
                throw ParserError(s"Mismatching identifiers: ${id.name}, ${id2.name}", SourceLoc(loc, path))
              }
              value
            case Some((None, value)) =>
              /*step with notation '+= 5' (a usage like '1 < _ < 20; += 2' is allowed)*/
              if (id.name != "_"){
                throw ParserError(s"Notation '+= $value' is only allowed when no identifier is used.", SourceLoc(loc, path))
              }
              value
          }
          Traces.TimeRange(Some(id), from, to, step)
      } | identifierOrWildcard ~ ((((LEQ | LT | GEQ | GT) ~ bigNat) ~ step1.?) | (DDOT ~> (bigNat ~ step2.?)) | ((((IN ~> bigNat) <~ DDOT) ~ bigNat) ~ step2.?)) ^^! {
        case (loc, (id: Traces.Identifier, (rhs: BigInt, rrhs: Option[BigInt]))) =>
          /*
          id .. to
          id .. to-step, to
          * */
          Traces.TimeRange(Some(id), 1, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
        case (loc, (id: Traces.Identifier, ((from: BigInt, rhs: BigInt), rrhs: Option[BigInt]))) =>
          /*
          id in from .. to
          id in from .. to-step, to
          */
          Traces.TimeRange(Some(id), from, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
        case (loc, (id: Traces.Identifier, (rhs: (WithLocation[_], BigInt), stepOpt: Option[(Option[Traces.Identifier], BigInt)]))) =>
          /*
          id < to
          id <= to
          id > from
          id >= from
          Optional ; id += step
          */
          val (from, to): (BigInt, Option[BigInt]) = rhs match {
            case (WithLocation(_, LT), value) => (1, Some(value - 1))
            case (WithLocation(_, LEQ), value) => (1, Some(value))
            case (WithLocation(_, GT), value) => (value + 1, None)
            case (WithLocation(_, GEQ), value) => (value, None)
          }
          if (stepOpt.isDefined && id.name == "_"){
            /*Definition of a step while using a wildcard (e.g 1 < _ < 20; += 3) is not allowed.*/
            throw ParserError(s"Declaration of a step is not allowed when using '_'.", SourceLoc(loc, path))
          }
          val step: BigInt = stepOpt match {
          case None =>
            /*no step given, implicitly use 1*/
            1
          case Some((Some(id2), value)) =>
            /*step with notation 't += 5'*/
            if (id.name != id2.name && id2.name != "_"){
              throw ParserError(s"Mismatching identifiers: ${id.name}, ${id2.name}", SourceLoc(loc, path))
            }
            value
          case Some((None, value)) =>
            /*step with notation '+= 5' (a usage like '1 < _; += 2' is allowed)*/
            if (id.name != "_"){
              throw ParserError(s"Notation '+= $value' is only allowed when no identifier is used.", SourceLoc(loc, path))
            }
            value
        }
          Traces.TimeRange(Some(id), from, to, step)
      } | ((((LEQ | LT | GEQ | GT) ~ bigNat) ~ step1.?) | (DDOT ~> (bigNat ~ step2.?))) ^^! {
        case (loc, (rhs: (WithLocation[_], BigInt), stepOpt: Option[(Option[Traces.Identifier], BigInt)])) =>
          /*
          < to
          <= to
          > from
          >= from
          Optional ; id += step
          */
          val (from, to): (BigInt, Option[BigInt]) = rhs match {
            case (WithLocation(_, LT), value) => (1, Some(value - 1))
            case (WithLocation(_, LEQ), value) => (1, Some(value))
            case (WithLocation(_, GT), value) => (value + 1, None)
            case (WithLocation(_, GEQ), value) => (value, None)
          }
          val step : BigInt = stepOpt match {
            case None =>
              /*no step given, implicitly use 1*/
              1
            case Some((Some(id2), value)) =>
              /*step with notation 't += 5'*/
              if (id2.name != "_") {
                throw ParserError(s"Notation '$id2 += $value' is only allowed when identifiers are used.", SourceLoc(loc, path))
              }
              value
            case Some((None, value)) =>
              /*step with notation '+= 5'*/
              value
          }
          Traces.TimeRange(None, from, to, step)
        case (loc, (rhs: BigInt, rrhs: Option[BigInt])) =>
          /*
          .. to
          .. to-step, to
          */
          Traces.TimeRange(None, 1, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
      }
    }


    /*first variant of a step: (used in notations with <, <=, >, >=)
    * ; t += 2
    * ; += 2 (only valid if no timestamp variable is used)*/
    def step1: Parser[(Option[Traces.Identifier], BigInt)] = (SEMICOLON ~> identifierOrWildcard.?) ~ (PLUSEQ ~> bigNat)

    /*Second variant of a step: (used in notations with ..)
    * , 20*/
    def step2: Parser[BigInt] = COMMA ~> bigNat

    def string: Parser[String] = matchToken("string", Set("<string>")) {
      case WithLocation(loc, STRING(value)) => value
    }
  }

}

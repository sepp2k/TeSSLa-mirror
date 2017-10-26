package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{TesslaCore, TesslaSource, TimeUnit}
import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.tessla.Errors.{NotAnEventError, ParserError}
import de.uni_luebeck.isp.tessla.TimeUnit._
import de.uni_luebeck.isp.tessla.interpreter.Trace._
import de.uni_luebeck.isp.tessla.Location

object TraceParser extends Parsers {
  def parseTrace(tesslaSource: TesslaSource): RawTrace = {
    def eventsOnly(lines: Iterator[Either[TimeUnit, EventRange]]): Iterator[EventRange] =
      lines.map {
        case Left(tu) => throw NotAnEventError(tu.toString, tu.loc)
        case Right(ev) => ev
      }

    val input = parseMany(new Parsers(tesslaSource.path).line, tesslaSource.src).map {
      case Success(_, line, _, _) => line
      case fail: Failure => throw ParserError(fail.message, Location(fail.loc, tesslaSource.path))
    }

    input.take(1).toList.headOption match {
      case Some(Left(tu)) => new RawTrace(Some(tu), eventsOnly(input))
      case Some(Right(ev)) => new RawTrace(None, Iterator(ev) ++ eventsOnly(input))
      case None => new RawTrace(None, Iterator())
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

    case object EXCLMARK extends Token("!")

    case object PLUS extends Token("+")

    case object STAR extends Token("*")

    case object SLASH extends Token("/")

    case object PERCENT extends Token("%")

    case object DAMPERSAND extends Token("&&")

    case object DPIPE extends Token("||")

    case object RARROW extends Token("->")

    case object LRARROW extends Token("<->")

  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens

    import tokens._

    override val keywords = List(TRUE, FALSE, IN)
    override val symbols = List(COLON, SEMICOLON, COMMA, EQ, DOLLAR, LPAREN, RPAREN, MINUS, DDOT, LEQ, GEQ, LT, GT,
      UNDERSCORE, PLUSEQ, PLUS, STAR, SLASH, PERCENT, DAMPERSAND, DPIPE, RARROW, LRARROW, EXCLMARK)
    override val comments = List("--" -> "\n")

    override def isIdentifierCont(c: Char): Boolean = {
      super.isIdentifierCont(c)
    }
  }

  import Tokens._

  override val tokenizer = Tokenizer


  class Parsers(path: String) {
    def line: Parser[Either[TimeUnit, Trace.EventRange]] = (timeUnit | event) ^^ {
      case tu: TimeUnit => Left(tu)
      case ev: EventRange => Right(ev)
    }

    def event: Parser[Trace.EventRange] =
      (((timeRange <~ COLON) ~ identifier) ~ (EQ ~> equivalence).?) ^^! {
        case (loc, ((time, id), v)) =>
          Trace.EventRange(Location(loc, path), time, id, v.getOrElse(Trace.Literal(TesslaCore.Unit(Location(loc, path)), Location(loc, path))))
      }

    def equivalence: Parser[Trace.TraceOp] =
      rep1sep(implication, LRARROW) ^^! {
        case (loc, operands) => operands.reduceLeft {
          (acc, elem) => Trace.Equiv(acc, elem, Location(loc, path))
        }
      }

    def implication: Parser[Trace.TraceOp] =
      rep1sep(disjunction, RARROW) ^^! {
        case (loc, operands) => operands.reduceLeft {
          (acc, elem) => Trace.Equiv(acc, elem, Location(loc, path))
        }
      }

    def disjunction: Parser[Trace.TraceOp] =
      rep1sep(conjunction, DPIPE) ^^! {
        case (loc, operands) => operands.reduceLeft {
          (acc, elem) => Trace.Equiv(acc, elem, Location(loc, path))
        }
      }

    def conjunction: Parser[Trace.TraceOp] =
      rep1sep(mult, DAMPERSAND) ^^! {
        case (loc, operands) => operands.reduceLeft {
          (acc, elem) => Trace.Equiv(acc, elem, Location(loc, path))
        }
      }

    def mult: Parser[Trace.TraceOp] =
      add ~ (multArithOp ~ add).* ^^! {
        case (loc, (lhs, ops)) =>
          ops match {
            case Seq() => lhs
            case _ => ops.foldLeft(lhs) {
              case (l, (op, r)) => op(l, r, Location(loc, path))
            }
          }
      }

    def add: Parser[Trace.TraceOp] =
      unaryOp ~ (addArithOp ~ unaryOp).* ^^! {
        case (loc, (lhs, ops)) =>
          ops match {
            case Seq() => lhs
            case _ => ops.foldLeft(lhs) {
              case (l, (op, r)) => op(l, r, Location(loc, path))
            }
          }
      }

    def multArithOp: Parser[(TraceOp, TraceOp, Location) => TraceOp] =
      STAR ^^^ {
        (lhs: TraceOp, rhs: TraceOp, loc: Location) => Trace.Mult(lhs, rhs, loc)
      } |
        SLASH ^^^ {
          (lhs: TraceOp, rhs: TraceOp, loc: Location) => Trace.Div(lhs, rhs, loc)
        } |
        PERCENT ^^^ {
          (lhs: TraceOp, rhs: TraceOp, loc: Location) => Trace.Mod(lhs, rhs, loc)
        }

    def addArithOp: Parser[(TraceOp, TraceOp, Location) => TraceOp] =
      PLUS ^^^ {
        (lhs: TraceOp, rhs: TraceOp, loc: Location) => Trace.Add(lhs, rhs, loc)
      } |
        MINUS ^^^ {
          (lhs: TraceOp, rhs: TraceOp, loc: Location) => Trace.Sub(lhs, rhs, loc)
        }

    def unaryOp: Parser[Trace.TraceOp] =
      EXCLMARK ~> atomic ^^! {
        case (loc, v) => Trace.Not(v, Location(loc, path))
      } |
        MINUS ~> atomic ^^! {
          case (loc, v) => Trace.Neg(v, Location(loc, path))
        } |
        LPAREN ~> equivalence.? <~ RPAREN ^^! {
          case (loc, None) => Trace.Literal(TesslaCore.Unit(Location(loc, path)), Location(loc, path))
          case (_, Some(exp)) => exp
        } |
        atomic

    def atomic: Parser[Trace.TraceOp] =
      literal ^^! {
        (loc, v) => Trace.Literal(v, Location(loc, path))
      } | identifier

    def literal: Parser[TesslaCore.LiteralValue] =
      TRUE ^^^! {
        loc => TesslaCore.BoolLiteral(true, Location(loc, path))
      } |
        FALSE ^^^! {
          loc => TesslaCore.BoolLiteral(false, Location(loc, path))
        } |
        string ^^! {
          case (loc, value) => TesslaCore.StringLiteral(value, Location(loc, path))
        } |
        bigInt ^^! {
          case (loc, value) => TesslaCore.IntLiteral(value, Location(loc, path))
        }

    def unit: Parser[TesslaCore.LiteralValue] =
      LPAREN ~ RPAREN ^^^! {
        loc => TesslaCore.Unit(Location(loc, path))
      }

    def literalWithUnit: Parser[TesslaCore.LiteralValue] = literal | unit

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

    def identifierOrWildcard: Parser[Trace.Identifier] =
      UNDERSCORE ^^^! {
        loc => Trace.Identifier(Location(loc, path), "_")
      } |
        identifier

    def timeRange: Parser[Trace.TimeRange] = {
      bigNat ~ (((COMMA ~> bigNat) ~ (DDOT ~> identifierOrWildcard.?)) | (DDOT ~> ((bigNat ~ step2.?) | identifierOrWildcard.?)) | ((((LEQ | LT) ~ identifierOrWildcard) ~ ((LEQ | LT) ~ bigNat).?) ~ step1.?)).? ^^! {
        case (loc, (value, None)) =>
          /*value*/
          Trace.TimeRange(None, value, Some(value), 1)
        case (loc, (from: BigInt, Some((rhs: BigInt, rrhs: Option[BigInt])))) =>
          /*
          from .. to
          from .. to-step, to
          */
          Trace.TimeRange(None, from, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
        case (loc, (from: BigInt, Some(idOpt: Option[Trace.Identifier]))) =>
          /*
          from .. (infinite)
          from .. id (infinite)
          */
          Trace.TimeRange(idOpt, from, None, 1)
        case (loc, (valLeft, Some((valLeft2: BigInt, Some(id: Trace.Identifier))))) =>
          /*
          from, from+step .. id (infinite)
          */
          Trace.TimeRange(Some(id), valLeft, None, valLeft2 - valLeft)
        case (loc, (valLeft, Some((valLeft2: BigInt, None)))) =>
          /*
          from, from+step ..
          */
          Trace.TimeRange(None, valLeft, None, valLeft2 - valLeft)
        case (loc, (valLeft, Some((((op1: WithLocation[_], id: Trace.Identifier), rhs: Option[(WithLocation[_], BigInt)]), stepOpt: Option[(Option[Trace.Identifier], BigInt)])))) =>
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
          if (stepOpt.isDefined && id.name == "_") {
            /*Definition of a step while using a wildcard (e.g 1 < _ < 20; += 3) is not allowed.*/
            throw ParserError(s"Declaration of a step is not allowed when using '_'.", Location(loc, path))
          }
          val step: BigInt = stepOpt match {
            case None =>
              /*no step given, implicitly use 1*/
              1
            case Some((Some(id2), value)) =>
              /*step with notation 't += 5'*/
              if (id.name != id2.name && id2.name != "_") {
                throw ParserError(s"Mismatching identifiers: ${id.name}, ${id2.name}", Location(loc, path))
              }
              value
            case Some((None, value)) =>
              /*step with notation '+= 5' (a usage like '1 < _ < 20; += 2' is allowed)*/
              if (id.name != "_") {
                throw ParserError(s"Notation '+= $value' is only allowed when no identifier is used.", Location(loc, path))
              }
              value
          }
          Trace.TimeRange(Some(id), from, to, step)
      } | identifierOrWildcard ~ ((((LEQ | LT | GEQ | GT) ~ bigNat) ~ step1.?) | (DDOT ~> (bigNat ~ step2.?)) | ((((IN ~> bigNat) <~ DDOT) ~ bigNat) ~ step2.?)) ^^! {
        case (loc, (id: Trace.Identifier, (rhs: BigInt, rrhs: Option[BigInt]))) =>
          /*
          id .. to
          id .. to-step, to
          * */
          Trace.TimeRange(Some(id), 1, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
        case (loc, (id: Trace.Identifier, ((from: BigInt, rhs: BigInt), rrhs: Option[BigInt]))) =>
          /*
          id in from .. to
          id in from .. to-step, to
          */
          Trace.TimeRange(Some(id), from, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
        case (loc, (id: Trace.Identifier, (rhs: (WithLocation[_], BigInt), stepOpt: Option[(Option[Trace.Identifier], BigInt)]))) =>
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
          if (stepOpt.isDefined && id.name == "_") {
            /*Definition of a step while using a wildcard (e.g 1 < _ < 20; += 3) is not allowed.*/
            throw ParserError(s"Declaration of a step is not allowed when using '_'.", Location(loc, path))
          }
          val step: BigInt = stepOpt match {
            case None =>
              /*no step given, implicitly use 1*/
              1
            case Some((Some(id2), value)) =>
              /*step with notation 't += 5'*/
              if (id.name != id2.name && id2.name != "_") {
                throw ParserError(s"Mismatching identifiers: ${id.name}, ${id2.name}", Location(loc, path))
              }
              value
            case Some((None, value)) =>
              /*step with notation '+= 5' (a usage like '1 < _; += 2' is allowed)*/
              if (id.name != "_") {
                throw ParserError(s"Notation '+= $value' is only allowed when no identifier is used.", Location(loc, path))
              }
              value
          }
          Trace.TimeRange(Some(id), from, to, step)
      } | ((((LEQ | LT | GEQ | GT) ~ bigNat) ~ step1.?) | (DDOT ~> (bigNat ~ step2.?))) ^^! {
        case (loc, (rhs: (WithLocation[_], BigInt), stepOpt: Option[(Option[Trace.Identifier], BigInt)])) =>
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
          val step: BigInt = stepOpt match {
            case None =>
              /*no step given, implicitly use 1*/
              1
            case Some((Some(id2), value)) =>
              /*step with notation 't += 5'*/
              if (id2.name != "_") {
                throw ParserError(s"Notation '$id2 += $value' is only allowed when identifiers are used.", Location(loc, path))
              }
              value
            case Some((None, value)) =>
              /*step with notation '+= 5'*/
              value
          }
          Trace.TimeRange(None, from, to, step)
        case (loc, (rhs: BigInt, rrhs: Option[BigInt])) =>
          /*
          .. to
          .. to-step, to
          */
          Trace.TimeRange(None, 1, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
      }
    }


    /*first variant of a step: (used in notations with <, <=, >, >=)
    * ; t += 2
    * ; += 2 (only valid if no timestamp variable is used)*/
    def step1: Parser[(Option[Trace.Identifier], BigInt)] = (SEMICOLON ~> identifierOrWildcard.?) ~ (PLUSEQ ~> bigNat)

    /*Second variant of a step: (used in notations with ..)
    * , 20*/
    def step2: Parser[BigInt] = COMMA ~> bigNat

    def string: Parser[String] = matchToken("string", Set("<string>")) {
      case WithLocation(_, STRING(value)) => value
    }
  }

}

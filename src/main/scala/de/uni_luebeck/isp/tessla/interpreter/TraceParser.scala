package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.compacom
import de.uni_luebeck.isp.tessla.{TesslaCore, TesslaSource, TimeUnit}
import de.uni_luebeck.isp.compacom.{Parsers, SimpleTokenizer, SimpleTokens, WithLocation}
import de.uni_luebeck.isp.tessla.Errors.{NotAnEventError, ParserError}
import de.uni_luebeck.isp.tessla.TimeUnit._
import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.interpreter.RawTrace._

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
    def line: Parser[Either[TimeUnit, EventRange]] = (timeUnit | event) ^^ {
      case tu: TimeUnit => Left(tu)
      case ev: EventRange => Right(ev)
    }

    def event: Parser[EventRange] =
      (((timeRange <~ COLON) ~ identifier) ~ (EQ ~> equivalence).?) ^^! {
        case (loc, ((time, id), v)) =>
          EventRange(Location(loc, path), time, id, v.getOrElse(RawTrace.Literal(TesslaCore.Unit(Location(loc, path)), Location(loc, path))))
      }

    def equivalence: Parser[RawTrace.TraceOp] =
      rep1sep(implication, LRARROW) ^^! {
        case (loc, operands) => operands.reduceLeft {
          (acc, elem) => RawTrace.Equiv(acc, elem, Location(loc, path))
        }
      }

    def implication: Parser[RawTrace.TraceOp] =
      rep1sep(disjunction, RARROW) ^^! {
        case (loc, operands) => operands.reduceLeft {
          (acc, elem) => RawTrace.Equiv(acc, elem, Location(loc, path))
        }
      }

    def disjunction: Parser[RawTrace.TraceOp] =
      rep1sep(conjunction, DPIPE) ^^! {
        case (loc, operands) => operands.reduceLeft {
          (acc, elem) => RawTrace.Equiv(acc, elem, Location(loc, path))
        }
      }

    def conjunction: Parser[RawTrace.TraceOp] =
      rep1sep(mult, DAMPERSAND) ^^! {
        case (loc, operands) => operands.reduceLeft {
          (acc, elem) => RawTrace.Equiv(acc, elem, Location(loc, path))
        }
      }

    def mult: Parser[RawTrace.TraceOp] =
      add ~ (multArithOp ~ add).* ^^! {
        case (loc, (lhs, ops)) =>
          ops match {
            case Seq() => lhs
            case _ => ops.foldLeft(lhs) {
              case (l, (op, r)) => op(l, r, Location(loc, path))
            }
          }
      }

    def add: Parser[RawTrace.TraceOp] =
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
        (lhs: TraceOp, rhs: TraceOp, loc: Location) => RawTrace.Mult(lhs, rhs, loc)
      } |
        SLASH ^^^ {
          (lhs: TraceOp, rhs: TraceOp, loc: Location) => RawTrace.Div(lhs, rhs, loc)
        } |
        PERCENT ^^^ {
          (lhs: TraceOp, rhs: TraceOp, loc: Location) => RawTrace.Mod(lhs, rhs, loc)
        }

    def addArithOp: Parser[(TraceOp, TraceOp, Location) => TraceOp] =
      PLUS ^^^ {
        (lhs: TraceOp, rhs: TraceOp, loc: Location) => RawTrace.Add(lhs, rhs, loc)
      } |
        MINUS ^^^ {
          (lhs: TraceOp, rhs: TraceOp, loc: Location) => RawTrace.Sub(lhs, rhs, loc)
        }

    def unaryOp: Parser[RawTrace.TraceOp] =
      EXCLMARK ~> atomic ^^! {
        case (loc, v) => RawTrace.Not(v, Location(loc, path))
      } |
        MINUS ~> atomic ^^! {
          case (loc, v) => RawTrace.Neg(v, Location(loc, path))
        } |
        LPAREN ~> equivalence.? <~ RPAREN ^^! {
          case (loc, None) => RawTrace.Literal(TesslaCore.Unit(Location(loc, path)), Location(loc, path))
          case (_, Some(exp)) => exp
        } |
        atomic

    def atomic: Parser[RawTrace.TraceOp] =
      literal ^^! {
        (loc, v) => RawTrace.Literal(v, Location(loc, path))
      } | identifier

    def literal: Parser[TesslaCore.Value] =
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

    def unit: Parser[TesslaCore.Value] =
      LPAREN ~ RPAREN ^^^! {
        loc => TesslaCore.Unit(Location(loc, path))
      }

    def literalWithUnit: Parser[TesslaCore.Value] = literal | unit

    def timeUnit: Parser[TimeUnit] = DOLLAR ~> ID("timeunit") ~> EQ ~> matchToken("string", Set("<string>")) {
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

    def identifierWildcardOrNumber: Parser[(Option[Trace.Identifier], Option[BigInt])] =
      identifierOrWildcard ^^ {
        value => (Some(value), None)
      } | bigNat ^^ {
        value => (None, Some(value))
      }

    def leftBoundRangeTail: Parser[(compacom.Location, BigInt) => RawTrace.TimeRange] = {
      def leftSideOp = LEQ ^^^ {
        0
      } | LT ^^^ {
        1
      }

      def rightSideOp = LEQ ~> bigNat ^^ {
        case value => value
      } | LT ~> bigNat ^^ {
        case value => value - 1
      }

      ((COMMA ~> bigNat) ~ (DDOT ~> identifierWildcardOrNumber.?)) ^^ {
        /*
        from, from+step .. id (infinite)
        from, from+step .. (infinite)
        from, from+step .. to
        */
        case (fromStep, None) =>
          (loc: compacom.Location, from: BigInt) =>
            RawTrace.TimeRange(Location(loc, path), None, from, None, fromStep - from)
        case (fromStep, Some((idOpt, toOpt))) =>
          (loc: compacom.Location, from: BigInt) =>
            RawTrace.TimeRange(Location(loc, path), idOpt, from, toOpt, fromStep - from)
      } | DDOT ~> (bigNat^^ {
        /*
        from .. to
        */
        to =>
          (loc: compacom.Location, from: BigInt) => RawTrace.TimeRange(Location(loc, path), None, from, Some(to), 1)
      } | identifierOrWildcard.? ^^ {
        /*
        from .. (infinite)
        from .. id (infinite)
        */
        case (idOpt) =>
          (loc: compacom.Location, from: BigInt) => RawTrace.TimeRange(Location(loc, path), idOpt, from, None, 1)
      }) | (((leftSideOp ~ identifierOrWildcard) ~ rightSideOp.?) ~ step1.?) ^^ {
        /*
        from < id < to
        from < id <= to
        from <= id < to
        from <= id <= to
        from < id (infinite)
        from <= id (infinite)
        Optional ; id += step*/
        case (((offset, id), to), stepOpt) =>
          (loc: compacom.Location, lhs: BigInt) =>
            val from = lhs + offset
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
            RawTrace.TimeRange(Location(loc, path), Some(id), from, to, step)
      }
    }

    def identifierRangeTail: Parser[(compacom.Location, RawTrace.Identifier) => RawTrace.TimeRange] = {
      def leftSideOp: Parser[(BigInt, Option[BigInt])] =
        LEQ ~> bigNat ^^ {
          value => (BigInt(1), Some(value))
        } |
          LT ~> bigNat ^^ {
            value => (BigInt(1), Some(value - 1))
          } |
          GEQ ~> bigNat ^^ {
            value => (value, None)
          } |
          GT ~> bigNat ^^ {
            value => (value + 1, None)
          }

      leftSideOp ~ step1.? ^^ {
        case ((from, to), stepOpt) =>
          (loc: compacom.Location, id: RawTrace.Identifier) =>
            /*
            id < to
            id <= to
            id > from
            id >= from
            Optional ; id += step
            */
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
            RawTrace.TimeRange(Location(loc, path), Some(id), from, to, step)
      } | DDOT ~> (bigNat ~ step2.?) ^^ {
        case (rhs, rrhs) =>
          (loc: compacom.Location, id: RawTrace.Identifier) =>
            /*
           id .. to
           id .. to-step, to
           * */
            RawTrace.TimeRange(Location(loc, path), Some(id), 1, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
      } | (((IN ~> bigNat) <~ DDOT) ~ bigNat) ~ step2.? ^^ {
        case ((from, rhs), rrhs) =>
          (loc: compacom.Location, id: RawTrace.Identifier) =>
            /*
            id in from .. to
            id in from .. to-step, to
            */
            RawTrace.TimeRange(Location(loc, path), Some(id), from, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
      }
    }

    def timeRange: Parser[RawTrace.TimeRange] = {
      bigNat ~ leftBoundRangeTail.? ^^! {
        case (loc, (from, f)) =>
          /*value*/
          f.map(_.apply(loc, from)).getOrElse(RawTrace.TimeRange(Location(loc, path), None, from, Some(from), 1))
      } | identifierOrWildcard ~ identifierRangeTail ^^! {
        case (loc, (id, f)) => f(loc, id)
      } | ((LEQ | LT | GEQ | GT) ~ bigNat) ~ step1.? ^^! {
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
          RawTrace.TimeRange(Location(loc, path), None, from, to, step)
      } | DDOT ~> (bigNat ~ step2.?) ^^! {
        case (loc, (rhs: BigInt, rrhs: Option[BigInt])) =>
          /*
          .. to
          .. to-step, to
          */
          RawTrace.TimeRange(Location(loc, path), None, 1, rrhs.orElse(Some(rhs)), rrhs.getOrElse(rhs + 1) - rhs)
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

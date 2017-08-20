package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{CompilationError, SourceLoc, _}
import de.uni_luebeck.isp.compacom.{SimpleTokenizer, SimpleTokens, WithLocation, _}
import de.uni_luebeck.isp.tessla.TimeUnit.{Days, Hours, TimeUnit => _, _}


/**
  * Created by Larissa on 19.08.2017.
  */
class Parser{

  case class ParserError(parserFailure: Parsers.Failure) extends CompilationError {
    override def loc = SourceLoc(parserFailure.loc)

    override def message = parserFailure.message
  }

  def translateSpec(source: TesslaSource) = {
    Parsers.parseAll(Parsers.spec, source.src) match {
      case Parsers.Success(_, spec, _, _) => spec
      case fail: Parsers.Failure => throw ParserError(fail)
    }
  }

}


  object Tokens extends SimpleTokens {

    case object COLON extends Token(":")
    case object EQ extends Token("=")
    case object DOLLAR extends Token("$")
    case object TRUE extends Token("true")
    case object FALSE extends Token("false")
    case object LPAREN extends Token ("(")
    case object RPAREN extends Token (")")
    case object DQUOTE extends Token("\"")






  }

  object Tokenizer extends SimpleTokenizer {
    override val tokens = Tokens

    import tokens._

    override val keywords = List(TRUE, FALSE)
    override val symbols = List(COLON, EQ, DOLLAR, LPAREN, RPAREN, DQUOTE)
    override val comments = List("--" -> "\n")

    override def isIdentifierCont(c: Char): Boolean = super.isIdentifierCont(c) || c == '.'
  }

  object Parsers extends Parsers {

    import scala.language.implicitConversions
    import Tokens._

    override val tokenizer = Tokenizer

    implicit def tokenToParser(t: Token): Parser[WithLocation[Token]] = token(t)

    def spec: Parser[Input.Spec] = timeUnitDecl.? ~ event.* ^^ {
      case (Some(time), events) => Input.Spec(time +: events)
      case (None, events) => Input.Spec(events)
    }

    def event : Parser[Input.Event] =
      ((((bigInt <~ COLON) ~ identifier) <~ EQ) ~ value.?) ^^!{
        case (loc, ((time, id), v)) => Input.Event(loc, time, id, v.getOrElse(TesslaCore.Unit(SourceLoc(loc))))
      }


    def bigInt : Parser[BigInt] =
      matchToken("integer", Set("<integer>")){
        case WithLocation(loc, INT(value)) => BigInt(value)
      }

    def identifier: Parser[Input.Identifier] = matchToken("identifier", Set("<identifier>")) {
      case WithLocation(loc, ID(name)) => Input.Identifier(loc, name)
    }

    def value: Parser[TesslaCore.LiteralValue] =
      TRUE ^^^!{
        loc => TesslaCore.BoolLiteral(true, SourceLoc(loc))
      } |
        FALSE ^^^!{
          loc => TesslaCore.BoolLiteral(false, SourceLoc(loc))
        } |
      LPAREN ~ RPAREN ^^^!{
        loc => TesslaCore.Unit(SourceLoc(loc))
      } |
        (DQUOTE ~> string)<~ DQUOTE ^^!{
          case (loc, value) => TesslaCore.StringLiteral(value, SourceLoc(loc))
      } |
      bigInt ^^!{
        case (loc, value) => TesslaCore.IntLiteral(value, SourceLoc(loc))
      }



    def string: Parser[String] = matchToken("string", Set("<string>")){
      case WithLocation(loc, STRING(value)) => value
    }




    def timeUnitDecl : Parser[Input.TimeUnit] =
      DOLLAR ~> (ID("timeunit") ~> timeUnit) ^^! {
        case (loc,unit) => Input.TimeUnit(loc, unit)
      }

    def timeUnit: Parser[TimeUnit.TimeUnit] =
      ID("ns") ^^^ {
        Nanos
      } |
        ID("us") ^^^ {
          Micros
        } |
        ID("ms") ^^^ {
          Millis
        } |
        ID("s") ^^^ {
          Seconds
        } |
        ID("m") ^^^ {
          Minutes
        } |
        ID("h") ^^^ {
          Hours
        } |
        ID("d") ^^^ {
          Days
        }
  }

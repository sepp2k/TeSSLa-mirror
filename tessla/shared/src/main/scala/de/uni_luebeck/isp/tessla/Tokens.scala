package de.uni_luebeck.isp.tessla

import scala.io.Source
import de.uni_luebeck.isp.compacom.{Tokens => CompacomTokens}
import scala.collection.mutable.Queue

object Tokens extends CompacomTokens {
  case class ID(name: String) extends Token(name)
  case class LIT_INT(value: Int) extends Token(value.toString())
  case object DEFINE extends Token("define")
  case object OUT extends Token("out")
  case object OF_TYPE extends Token(":")
  case object DEFINE_AS extends Token(":=")
  case object COMMA extends Token(",")
  case object LPAREN extends Token("(")
  case object RPAREN extends Token(")")
  
  class TesslaTokenizer(s: Source) extends AbstractTokenizer(s) {
    
    // these are needed because isLetter is not working in scalaJS
    private def isLetterOrDigit(c: Character) = isLetter(c) || isDigit(c)
    private def isDigit(c: Character) = c >= '0' && c <= '9'
    private def isLetter(c: Character) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
   
    
    def defaultTokenParser: Option[Token] = {
      consume() match {
        case Some(',') => Some(COMMA)
        case Some('(') => Some(LPAREN)
        case Some(')') => Some(RPAREN)
        case Some(':') if peek() == Some('=') => consume(); Some(DEFINE_AS)
        case Some(':') => Some(OF_TYPE)
        case Some('-') if peek() == Some('-') => 
          consume()
          while (peek() match {
            case Some(c) if c != '\n' => consume(); true
            case _ => false
          }) {}
          None
        case Some(x) if x.isWhitespace => None
        case Some(x) if isLetter(x) =>
          val lexeme = new StringBuilder
          lexeme += x
          while (peek() match {
            case Some(x) if isLetterOrDigit(x) => consume(); lexeme += x; true
            case _ => false
          }) {}
          lexeme.result() match {
            case "define" => Some(DEFINE)
            case "out" => Some(OUT)
            case id => Some(ID(id))
          }
        case Some(x) if isDigit(x) =>
          val lexeme = new StringBuilder
          lexeme += x
          while (peek() match {
            case Some(x) if isDigit(x) => consume(); lexeme += x; true
            case _ => false
          }) {}
          return Some(LIT_INT(lexeme.result().toInt))
        case None => Some(EOF)
        case Some(x) => Some(InvalidToken(x.toString))
      }
    }
  }
  
  def tokenize(s: Source) = new TesslaTokenizer(s)
}
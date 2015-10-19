package de.uni_luebeck.isp.tessla

import scala.io.Source
import de.uni_luebeck.isp.compacom.{SimpleTokens, SimpleTokenizer}
import scala.collection.mutable.Queue

object Tokens extends SimpleTokens {
  //case class LIT_INT(value: Int) extends Token(value.toString())
  //val LIT_INT = INT
  //val LIT_STRING = STRING
  case object DEFINE extends Token("define")
  case object OUT extends Token("out")
  case object OF_TYPE extends Token(":")
  case object PERCENT extends Token("%")
  case object DEFINE_AS extends Token(":=")
  case object COMMA extends Token(",")
  case object LPAREN extends Token("(")
  case object RPAREN extends Token(")")
  case object MONITOR extends Token("monitor")
}


object Tokenizer extends SimpleTokenizer {
  override val tokens = Tokens
  import tokens._

  override val keywords = List(DEFINE, OUT, MONITOR)
  override val symbols = List(DEFINE_AS, OF_TYPE, COMMA, LPAREN, RPAREN, PERCENT)
  override val comments = List("--" -> "\n")
}

package de.uni_luebeck.isp.tessla

import scala.io.Source

import org.scalatest._

import Tokens._

class TokensSpec extends FlatSpec with Matchers {
  "A tessla tokenizer" should "tokenize the empty string" in {
    val tokens = Tokenizer.tokenize(Source.fromString(""));
    tokens.moveNext()
    tokens.current should be (EOF)
  }
  
  it should "tokenize identifiers" in {
    val tokens = Tokenizer.tokenize(Source.fromString("foo bar baz"));
    var idx = -1
    for (token <- List(ID("foo"), ID("bar"), ID("baz"), EOF)) {
      tokens.moveNext()
      tokens.current should be (token)
      //tokens.idx should be > idx
      //idx = tokens.idx
    }
  }
  
  it should "tokenize punctuation tokens" in {
    val tokens = Tokenizer.tokenize(Source.fromString("():,:="));
    var idx = -1
    for (token <- List(LPAREN, RPAREN, OF_TYPE, COMMA, DEFINE_AS, EOF)) {
      tokens.moveNext()
      tokens.current should be (token)
      //tokens.idx should be > idx
      //idx = tokens.idx
    }
  }
  
  it should "tokenize keywords" in {
    val tokens = Tokenizer.tokenize(Source.fromString("define out"));
    var idx = -1
    for (token <- List(DEFINE, OUT, EOF)) {
      tokens.moveNext()
      tokens.current should be (token)
      //tokens.idx should be > idx
      //idx = tokens.idx
    }
  }
  
  it should "tokenize identifiers that are keyword prefixes" in {
    val tokens = Tokenizer.tokenize(Source.fromString("definedVar outputValue"));
    var idx = -1
    for (token <- List(ID("definedVar"), ID("outputValue"), EOF)) {
      tokens.moveNext()
      tokens.current should be (token)
      //tokens.idx should be > idx
      //idx = tokens.idx
    }
  }
  
  it should "tokenize integer literals" in {
    val tokens = Tokenizer.tokenize(Source.fromString("123 456"));
    var idx = -1
    for (token <- List(LIT_INT(123), LIT_INT(456), EOF)) {
      tokens.moveNext()
      tokens.current should be (token)
      //tokens.idx should be > idx
      //idx = tokens.idx
    }
  }
  
  it should "ignore comments" in {
    val tokens = Tokenizer.tokenize(Source.fromString("42 -- this is a comment @\n 23"));
    var idx = -1
    for (token <- List(LIT_INT(42), LIT_INT(23), EOF)) {
      tokens.moveNext()
      tokens.current should be (token)
      //tokens.idx should be > idx
      //idx = tokens.idx
    }
  }
  
  it should "detect invalid tokens" in {
    val tokens = Tokenizer.tokenize(Source.fromString("@"));
    var idx = -1
    for (token <- List(Invalid("@"), EOF)) {
      tokens.moveNext()
      tokens.current should be (token)
      //tokens.idx should be > idx
      //idx = tokens.idx
    }
  }
  
  it should "tokenize a more complex example" in {
    val tokens = Tokenizer.tokenize(Source.fromString("define foo := bar(baz, blubb : int)"));
    var idx = -1
    for (token <- List(
        DEFINE, ID("foo"), DEFINE_AS, ID("bar"), LPAREN,
        ID("baz"), COMMA, ID("blubb"), OF_TYPE, ID("int"), RPAREN, EOF)
    ) {
      tokens.moveNext()
      tokens.current should be (token)
      //tokens.idx should be > idx
      //idx = tokens.idx
    }
  }
}
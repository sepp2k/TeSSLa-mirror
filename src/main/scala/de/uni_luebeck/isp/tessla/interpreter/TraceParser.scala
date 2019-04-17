package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Location, InputTraceLexer, InputTraceParser}
import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._

class TraceParser(input: Iterator[String], fileName: String) {
  def parseTrace(): Iterator[InputTraceParser.EventRangeContext] = {
    input.flatMap { line =>
      parseLine(line) match {
        case Some(l) => Iterator(l)
        case None => Iterator()
      }
    }
  }

  def parseLine(line: String): Option[InputTraceParser.EventRangeContext] = {
    parseLine(CharStreams.fromString(line, fileName))
  }

  var lineNumber = 0

  def parseLine(line: CharStream): Option[InputTraceParser.EventRangeContext] = {
    lineNumber += 1
    val lexer = new InputTraceLexer(line)
    lexer.setLine(lineNumber)
    val tokens = new CommonTokenStream(lexer)
    val parser = new InputTraceParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(r: Recognizer[_, _], offendingToken: Any, l: Int, c: Int, msg: String, e: RecognitionException) = {
        throw ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token]))
      }
    })
    Option(parser.line().eventRange)
  }
}

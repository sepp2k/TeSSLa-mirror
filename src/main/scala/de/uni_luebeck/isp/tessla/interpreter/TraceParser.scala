package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Location, InputTraceLexer, InputTraceParser}
import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._

class TraceParser {
  def parseTrace(input: CharStream): Iterator[InputTraceParser.EventRangeContext] = {
    val tokens = new CommonTokenStream(new InputTraceLexer(input))
    val parser = new InputTraceParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(r: Recognizer[_, _], offendingToken: Any, l: Int, c: Int, msg: String, e: RecognitionException) = {
        throw ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token]))
      }
    })
    parser.skipEmptyLines
    new TraceIterator(parser)
  }

  private class TraceIterator(parser: InputTraceParser) extends Iterator[InputTraceParser.EventRangeContext] {
    override def hasNext = !parser.isMatchedEOF

    override def next() = parser.line().eventRange()
  }
}

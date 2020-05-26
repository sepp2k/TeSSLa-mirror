package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{InputTraceLexer, InputTraceParser, Location}
import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._
import scala.jdk.CollectionConverters._

case class ParserEventRange(
  streamName: Token,
  expression: InputTraceParser.ExpressionContext,
  timeRange: InputTraceParser.TimeRangeContext,
  loc: Location
)

object TraceParser {
  def createInputTraceParser(line: CharStream, lineNumber: Int): InputTraceParser = {
    val lexer = new InputTraceLexer(line)
    lexer.setLine(lineNumber)
    val tokens = new CommonTokenStream(lexer)
    val parser = new InputTraceParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(
        r: Recognizer[_, _],
        offendingToken: Any,
        l: Int,
        c: Int,
        msg: String,
        e: RecognitionException
      ) = {
        throw ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token]))
      }
    })
    parser
  }
}

class TraceParser(input: Iterator[String], fileName: String) {
  def parseTrace(): Iterator[ParserEventRange] = {
    input.flatMap { line =>
      parseLine(line) match {
        case Some(l) => Iterator(l)
        case None    => Iterator()
      }
    }
  }

  def parseLine(line: String): Option[ParserEventRange] = {
    parseLine(CharStreams.fromString(line, fileName))
  }

  var lineNumber = 0

  def parseLine(line: CharStream): Option[ParserEventRange] = {
    lineNumber += 1
    val parser = TraceParser.createInputTraceParser(line, lineNumber)

    Option(parser.line().eventRange).map(ctx =>
      ParserEventRange(ctx.streamName, ctx.expression(), ctx.timeRange(), Location.fromNode(ctx))
    )
  }
}

class CsvTraceParser(input: Iterator[String], fileName: String) {
  var streamNames = List[Token]()

  def parseTrace(): Iterator[ParserEventRange] = {
    if (!input.hasNext) {
      throw ParserError("No header row found", Location.unknown)
    }
    parseHeader(input.next()) match {
      case Some(h) =>
        streamNames = h.streamNames.asScala.toList.tail
      case None =>
        throw ParserError("No header row found", Location.unknown)
    }

    input.flatMap { line => parseLine(line) }
  }

  def parseLine(line: String): Iterator[ParserEventRange] = {
    parseLine(CharStreams.fromString(line, fileName))
  }

  def parseHeader(line: String): Option[InputTraceParser.CsvHeaderContext] = {
    parseHeader(CharStreams.fromString(line, fileName))
  }

  var lineNumber = 0

  def parseHeader(line: CharStream): Option[InputTraceParser.CsvHeaderContext] = {
    lineNumber += 1
    val parser = TraceParser.createInputTraceParser(line, lineNumber)
    Option(parser.csvHeader())
  }

  def parseLine(line: CharStream): Iterator[ParserEventRange] = {
    lineNumber += 1
    val parser = TraceParser.createInputTraceParser(line, lineNumber)
    Option(parser.csvLine()) match {
      case Some(lineContext) =>
        lineContext
          .commaExpression()
          .asScala
          .zipWithIndex
          .filter { case (ctx, _) => ctx.expression() != null }
          .map {
            case (ctx, index) =>
              ParserEventRange(
                streamNames(index),
                ctx.expression(),
                lineContext.timeRange(),
                Location.fromNode(ctx.expression())
              )
          }
          .iterator
      case None => Iterator()
    }
  }
}

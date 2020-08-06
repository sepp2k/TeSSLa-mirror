package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{InputTraceLexer, InputTraceParser, Location}
import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._

import scala.jdk.CollectionConverters._

/**
 * Provides parsing for traces in csv or standard format.
 */

object TraceParser {

  /** Representation of a parsed event.
   *
    * @param streamName the token representing the stream name
   * @param expression the parsed expression
   * @param timestamp the parsed timestamp
   * @param loc location information for this event
   *
    * @see [[EventIterator]]
   * @see [[TraceExpressionEvaluator]] for evaluation of [[expression]]
   */
  case class Event(
    streamName: Token,
    expression: InputTraceParser.ExpressionContext,
    timestamp: InputTraceParser.TimestampContext,
    loc: Location
  )

  /**
   * Parse a trace in the standard format
   * @param input the trace as line iterator
   * @param fileName the file name, used for location information
   * @return the resulting trace of parsed events
   */
  def parseTrace(input: Iterator[String], fileName: String): Iterator[TraceParser.Event] = {
    val parsers = lineParsers(input, fileName)
    parsers.flatMap(parser => Option(parser.line().event())).map { ctx =>
      val loc = Location.fromNode(ctx)
      TraceParser.Event(ctx.streamName, ctx.expression(), ctx.timestamp(), loc)
    }
  }

  /**
   * Parse a trace in csv format
   * @param input the trace as line iterator
   * @param fileName the file name, used for location information
   * @return the resulting trace of parsed events
   */
  def parseCsvTrace(input: Iterator[String], fileName: String): Iterator[TraceParser.Event] = {
    val parsers = lineParsers(input, fileName)

    val streamNames = parsers.nextOption().flatMap(parser => Option(parser.csvHeader())) match {
      case Some(h) =>
        h.streamNames.asScala.toList.tail
      case None =>
        throw ParserError("No header row found", Location.forWholeFile("", fileName))
    }

    parsers.flatMap(parser => Option(parser.csvLine())).flatMap { lineContext =>
      lineContext
        .commaExpression()
        .asScala
        .zipWithIndex
        .collect {
          case (ctx, index) if ctx.expression() != null =>
            val loc = Location.fromNode(ctx.expression())
            TraceParser.Event(streamNames(index), ctx.expression(), lineContext.timestamp(), loc)
        }
        .iterator
    }
  }

  private def lineParsers(input: Iterator[String], fileName: String): Iterator[InputTraceParser] = {
    input.zipWithIndex.map {
      case (line, id) =>
        val charStream = CharStreams.fromString(line, fileName)
        TraceParser.createInputTraceParser(charStream, id)
    }
  }

  private def createInputTraceParser(line: CharStream, lineNumber: Int): InputTraceParser = {
    val lexer = new InputTraceLexer(line)
    lexer.setLine(lineNumber + 1)
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
      ): Unit = {
        throw ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token]))
      }
    })
    parser
  }
}

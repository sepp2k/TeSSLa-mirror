package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{InputTraceLexer, InputTraceParser, Location}
import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._

import scala.jdk.CollectionConverters._

object TraceParser {

  case class Event(
    streamName: Token,
    expression: InputTraceParser.ExpressionContext,
    timestamp: InputTraceParser.TimestampContext,
    loc: Location
  )

  def parseTrace(input: Iterator[String], fileName: String): Iterator[TraceParser.Event] = {
    val parsers = lineParsers(input, fileName)
    parsers.flatMap(parser => Option(parser.line().event())).map { ctx =>
      val loc = Location.fromNode(ctx)
      TraceParser.Event(ctx.streamName, ctx.expression(), ctx.timestamp(), loc)
    }
  }

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

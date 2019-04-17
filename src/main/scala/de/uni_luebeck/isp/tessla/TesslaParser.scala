package de.uni_luebeck.isp.tessla

import java.nio.file.Paths
import java.util.{IllegalFormatException, MissingFormatArgumentException}

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import org.antlr.v4.runtime._

import scala.collection.JavaConverters._

class TesslaParser(src: CharStream, expandIncludes: Boolean) extends TranslationPhase.Translator[TesslaSyntax.SpecContext] {
  val lexer = new TesslaLexer(src)
  val tokens = new CommonTokenStream(lexer)
  val sourcePair: misc.Pair[TokenSource, CharStream] = new misc.Pair(lexer, src)

  override def translateSpec() = {
    val parser = new TesslaSyntax(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(r: Recognizer[_, _], offendingToken: Any, l: Int, c: Int, msg: String, e: RecognitionException) = {
        error(ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token])))
      }
    })
    val spec = parser.spec()
    if (parser.getNumberOfSyntaxErrors > 0) {
      abortOnError()
    }

    if (expandIncludes) {
      spec.statements.addAll(0, spec.includes.asScala.flatMap(translateInclude).asJava)
    }
    spec.statements.asScala.foreach {
      case out: TesslaSyntax.OutContext =>
        if (out.ID == null) {
          val expr = out.expression
          val start = expr.start.getStartIndex
          val stop = expr.stop.getStopIndex
          val line = expr.start.getLine
          val column = expr.start.getCharPositionInLine
          val token = lexer.getTokenFactory.create(sourcePair, TesslaLexer.ID, null, 0, start, stop, line, column)
          val node = parser.createTerminalNode(out, token)
          out.addChild(node)
        }
      case _ => // Do nothing
    }
    spec
  }

  protected def translateInclude(include: TesslaSyntax.IncludeContext): Seq[TesslaSyntax.StatementContext] = {
    val currentFile = getFile(include)
    // getParent returns null for relative paths without subdirectories (i.e. just a file name), which is
    // annoying and stupid. So we wrap the call in an option and fall back to "." as the default.
    val dir = Option(Paths.get(currentFile).getParent).getOrElse(Paths.get("."))
    val includePath = dir.resolve(getIncludeString(include.file))
    unwrapResult(TesslaParser.translate(CharStreams.fromFileName(includePath.toString))).statements.asScala
  }

  private def getIncludeString(stringLit: TesslaSyntax.StringLitContext): String = {
    stringLit.stringContents.asScala.map {
      case text: TesslaSyntax.TextContext => text.getText
      case escapeSequence: TesslaSyntax.EscapeSequenceContext =>
        parseEscapeSequence(escapeSequence.getText, Location.fromNode(escapeSequence))
      case part =>
        error(StringInterpolationOrFormatInInclude(Location.fromNode(part)))
        ""
    }.mkString
  }

  protected def getFile(node: ParserRuleContext) = {
    node.getStart.getTokenSource.getSourceName
  }

  protected def parseEscapeSequence(sequence: String, loc: Location): String = {
    TesslaParser.parseEscapeSequence(sequence).getOrElse {
      error(InvalidEscapeSequence(sequence, loc))
      sequence
    }
  }
}

object TesslaParser extends TranslationPhase[CharStream, TesslaSyntax.SpecContext] {
  override def translate(spec: CharStream): Result[TesslaSyntax.SpecContext] = {
    parse(spec)
  }

  def parse(spec: CharStream, expandIncludes: Boolean = true): Result[TesslaSyntax.SpecContext] = {
    new TesslaParser(spec, expandIncludes = expandIncludes).translate()
  }

  def parseEscapeSequence(sequence: String): Option[String] = sequence match {
    case "\\r" => Some("\r")
    case "\\n" => Some("\n")
    case "\\t" => Some("\t")
    case "\\a" => Some("\u0007")
    case "\\\\" => Some("\\")
    case "\\\"" => Some("\"")
    case "\\$" => Some("$")
    case "\\%" => Some("%")
    case _ => None
  }

  sealed abstract class FormatSpecifierInfo
  case class InvalidFormat(err: TesslaError) extends FormatSpecifierInfo
  case class NoArgFormat(processedString: String) extends FormatSpecifierInfo
  case class SingleArgFormat(formatFunction: String) extends FormatSpecifierInfo

  def parseFormatString(format: String, loc: Location): FormatSpecifierInfo = {
    try {
      val processedString = String.format(format)
      // If no exception is thrown that means that we have a zero-argument format-specifier
      NoArgFormat(processedString)
    } catch {
      case _: MissingFormatArgumentException =>
        // If a MissingFormatArgumentException is thrown that means that the format string was syntactically correct
        // and takes one argument
        format.last match {
          case 'h' | 'H' | 's' | 'S' => SingleArgFormat("format")
          case 'd' | 'o' | 'x' | 'X' => SingleArgFormat("formatInt")
          case 'e' | 'E' | 'f' | 'g' | 'G' | 'a' | 'A' => SingleArgFormat("formatFloat")
          case _ => InvalidFormat(UnsupportedConversion(format.last, loc))
        }
      case err: IllegalFormatException =>
        InvalidFormat(StringFormatError(err, loc))
    }
  }
}
package de.uni_luebeck.isp.tessla

import java.nio.file.Paths

import de.uni_luebeck.isp.tessla.Errors._
import org.antlr.v4.runtime._

import scala.collection.JavaConverters._

abstract class AbstractTesslaParser[Item, Result] extends TranslationPhase[CharStream, Result] {
  protected def translateStatement(statement: TesslaSyntax.StatementContext): Item
  protected def aggregateItems(items: Seq[Item]): Result

  override def translateSpec(input: CharStream) = {
    aggregateItems(translateFile(input))
  }

  private def translateFile(input: CharStream): Seq[Item] = {
    val tokens = new CommonTokenStream(new TesslaLexer(input))
    val parser = new TesslaSyntax(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(r: Recognizer[_, _], offendingToken: Any, l: Int, c: Int, msg: String, e: RecognitionException) = {
        error(ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token])))
      }
    })
    val spec = parser.spec()
    if (parser.getNumberOfSyntaxErrors > 0) {
      val lastError = errors.remove(errors.length - 1)
      throw lastError
    }
    spec.includes.asScala.flatMap(translateInclude) ++ spec.statements.asScala.map(translateStatement)
  }

  private def translateInclude(include: TesslaSyntax.IncludeContext): Seq[Item] = {
    val currentFile = getFile(include)
    // getParent returns null for relative paths without subdirectories (i.e. just a file name), which is
    // annoying and stupid. So we wrap the call in an option and fall back to "." as the default.
    val dir = Option(Paths.get(currentFile).getParent).getOrElse(Paths.get("."))
    val includePath = dir.resolve(getIncludeString(include.file))
    translateFile(CharStreams.fromFileName(includePath.toString))
  }

  private def getIncludeString(stringLit: TesslaSyntax.StringLitContext): String = {
    stringLit.stringContents.asScala.map { part =>
      if (part.TEXT != null) part.TEXT.getText
      else if (part.ESCAPE_SEQUENCE != null) {
        parseEscapeSequence(part.ESCAPE_SEQUENCE.getText, Location.fromNode(part))
      } else {
        error(StringInterpolationInInclude(Location.fromNode(part)))
        ""
      }
    }.mkString
  }

  protected def getFile(node: ParserRuleContext) = {
    node.getStart.getTokenSource.getSourceName
  }

  protected def parseEscapeSequence(sequence: String, loc: Location): String = sequence match {
    case "\\r" => "\r"
    case "\\n" => "\n"
    case "\\t" => "\t"
    case "\\a" => "\u0007"
    case "\\\\" => "\\"
    case "\\\"" => "\""
    case other =>
      error(InvalidEscapeSequence(other, loc))
      other
  }
}

/*

 */

package de.uni_luebeck.isp.tessla.core

import java.nio.file.Paths
import java.util.{IllegalFormatException, MissingFormatArgumentException}

import de.uni_luebeck.isp.tessla.core.Errors._
import de.uni_luebeck.isp.tessla.core.TranslationPhase._
import org.antlr.v4.runtime._

import scala.jdk.CollectionConverters._

import scala.collection.mutable

/**
 * Parses given TeSSLa code using ANTLR4
 */

object TesslaParser {

  /** The result of the parser
   *
    * @param fileName the file name of the source
   * @param tokens   the generated tokens
   * @param tree     the parsed specification
   */
  case class ParseResult(
    fileName: String,
    tokens: CommonTokenStream,
    tree: TesslaSyntax.SpecContext
  )

  private def parse(src: CharStream): (ParseResult, Seq[TesslaError]) = {
    val lexer = new TesslaLexer(src)
    val tokens = new CommonTokenStream(lexer)
    val parser = new TesslaSyntax(tokens)
    val errors = mutable.ArrayBuffer[TesslaError]()
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
        errors += ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token]))
      }
    })

    val tree = parser.spec()
    (ParseResult(src.getSourceName, tokens, tree), errors.toSeq)
  }

  object SingleFile extends TranslationPhase[CharStream, ParseResult] {
    override def translate(src: CharStream): Result[ParseResult] = {
      val (result, errors) = parse(src)
      if (errors.isEmpty) {
        Success(result, Seq())
      } else {
        Failure(errors, Seq())
      }
    }
  }

  /**
   * Translation phase which parses a source while resolving its includes with the provided resolver.
   *
    * @param resolveInclude the include resolver to use
   * @see [[WithIncludesTranslator]]
   */
  class WithIncludes(resolveInclude: String => Option[CharStream])
      extends TranslationPhase[CharStream, IndexedSeq[ParseResult]] {
    override def translate(src: CharStream) = {
      new WithIncludesTranslator(src, resolveInclude).translate()
    }
  }

  /**
   * Parse a given source file while resolving its includes with the provided resolver.
   *
    * @param src            the source
   * @param resolveInclude the include resolver to use
   */
  class WithIncludesTranslator(src: CharStream, resolveInclude: String => Option[CharStream])
      extends TranslationPhase.Translator[IndexedSeq[ParseResult]]
      with CanParseConstantString {
    override def translateSpec(): IndexedSeq[ParseResult] = {
      parseWithIncludes(src)
    }

    private def parseWithIncludes(src: CharStream): IndexedSeq[ParseResult] = {
      val (mainResult, mainErrors) = parse(src)
      val includes = mainResult.tree.includes.asScala.toVector.flatMap { include =>
        val fileName = getConstantString(include.file)
        val loc = Location.fromNode(include.file)
        lookupInclude(fileName, mainResult.fileName, loc).map(parseWithIncludes).getOrElse {
          error(FileNotFound(fileName, Location.fromNode(include.file)))
          Seq()
        }
      }
      mainErrors.foreach(error)
      includes :+ mainResult
    }

    private def lookupInclude(
      includee: String,
      includer: String,
      loc: Location
    ): Option[CharStream] = {
      val includePath = Paths.get(includee)
      if (includePath.isAbsolute) {
        error(AbsoluteIncludePath(loc))
        None
      } else {
        val path = Paths.get(includer).resolveSibling(includePath)
        val pathString = Option(path.getRoot).map(_.toString.replace("\\", "/")).getOrElse("") +
          path.iterator().asScala.mkString("/")
        resolveInclude(pathString).orElse(resolveInclude(s"$pathString.tessla"))
      }
    }
  }

  trait CanParseConstantString {
    self: TranslationPhase.Translator[_] =>
    def getConstantString(stringLit: TesslaSyntax.StringLitContext): String = {
      stringLit.stringContents.asScala.map {
        case text: TesslaSyntax.TextContext => text.getText
        case escapeSequence: TesslaSyntax.EscapeSequenceContext =>
          parseEscapeSequence(escapeSequence.getText).getOrElse {
            error(InvalidEscapeSequence(escapeSequence.getText, Location.fromNode(escapeSequence)))
            escapeSequence.getText
          }
        case part =>
          error(StringInterpolationOrFormatInConstantString(Location.fromNode(part)))
          part.getText
      }.mkString
    }
  }

  def parseEscapeSequence(sequence: String): Option[String] = sequence match {
    case "\\r"  => Some("\r")
    case "\\n"  => Some("\n")
    case "\\t"  => Some("\t")
    case "\\a"  => Some("\u0007")
    case "\\\\" => Some("\\")
    case "\\\"" => Some("\"")
    case "\\$"  => Some("$")
    case "\\%"  => Some("%")
    case _      => None
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
          case 'h' | 'H' | 's' | 'S'                   => SingleArgFormat("format")
          case 'd' | 'o' | 'x' | 'X'                   => SingleArgFormat("formatInt")
          case 'e' | 'E' | 'f' | 'g' | 'G' | 'a' | 'A' => SingleArgFormat("formatFloat")
          case _                                       => InvalidFormat(UnsupportedConversion(format.last, loc))
        }
      case err: IllegalFormatException =>
        InvalidFormat(StringFormatError(err, loc))
    }
  }
}

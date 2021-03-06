package de.uni_luebeck.isp.tessla

import java.nio.file.Paths
import java.util.{IllegalFormatException, MissingFormatArgumentException}

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Result, Success}
import org.antlr.v4.runtime._

import scala.collection.JavaConverters._
import scala.collection.mutable

object TesslaParser {
  case class ParseResult(fileName: String, tokens: CommonTokenStream, tree: TesslaSyntax.SpecContext)

  private def parse(src: CharStream): (ParseResult, Seq[TesslaError]) = {
    val lexer = new TesslaLexer(src)
    val tokens = new CommonTokenStream(lexer)
    val parser = new TesslaSyntax(tokens)
    val errors = mutable.ArrayBuffer[TesslaError]()
    parser.removeErrorListeners()
    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(r: Recognizer[_, _], offendingToken: Any, l: Int, c: Int, msg: String, e: RecognitionException) = {
        errors += ParserError(msg, Location.fromToken(offendingToken.asInstanceOf[Token]))
      }
    })

    val tree = parser.spec()
    (ParseResult(src.getSourceName, tokens, tree), errors)
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

  class WithIncludesTranslator(src: CharStream, resolveInclude: String => Option[CharStream])
      extends TranslationPhase.Translator[IndexedSeq[ParseResult]] with CanParseConstantString {
    override def translateSpec(): IndexedSeq[ParseResult] = {
      parseWithIncludes(src)
    }

    private def parseWithIncludes(src: CharStream): IndexedSeq[ParseResult] = {
      val (mainResult, mainErrors) = parse(src)
      val includes = mainResult.tree.includes.asScala.toVector.flatMap {include =>
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

    private def lookupInclude(includee: String, includer: String, loc: Location): Option[CharStream] = {
      val includePath = Paths.get(includee)
      if (includePath.isAbsolute) {
        error(AbsoluteIncludePath(loc))
        None
      } else {
        val path = Paths.get(includer).resolveSibling(includePath).toString
        resolveInclude(path).orElse(resolveInclude(s"$path.tessla"))
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

  class WithIncludes(resolveInclude: String => Option[CharStream]) extends TranslationPhase[CharStream, IndexedSeq[ParseResult]] {
    override def translate(src: CharStream) = {
      new WithIncludesTranslator(src, resolveInclude).translate()
    }
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
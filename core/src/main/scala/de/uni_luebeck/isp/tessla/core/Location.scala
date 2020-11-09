/*

 */

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Location.SourceRange
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.antlr.v4.runtime.tree.TerminalNode

/**
 * Describes a location in a source
 */
sealed abstract class Location {

  /**
   * Merge two locations into one.
   * @param other the location to merge with
   * @return the newly merged location
   */
  def merge(other: Location): Location

  /**
   * The range denoting what part of the given source (e.g. line / column) this location describes.
   */
  def range: Option[SourceRange]

  /**
   * The path describing where the source originates from. E.g. a path to a file.
   */
  def path: String
}

object Location {

  /**
   * Trait for classes with locations.
   */
  trait HasLoc {
    def loc: Location
  }

  /**
   * Descibes a range within a source.
   * @param fromLine beginning line
   * @param fromColumn beginning column
   * @param toLine ending line
   * @param toColumn ending column
   */
  case class SourceRange(fromLine: Int, fromColumn: Int, toLine: Int, toColumn: Int) {
    override def toString = s"($fromLine,$fromColumn - $toLine,$toColumn)"

    def merge(other: SourceRange): SourceRange = {
      require(
        fromLine < other.fromLine || fromLine == other.fromLine && fromColumn <= other.fromColumn
      )
      require(toLine < other.toLine || toLine == other.toLine && toColumn <= other.toColumn)
      SourceRange(fromLine, fromColumn, other.toLine, other.toColumn)
    }
  }

  private case class SourceLoc(sourceRange: SourceRange, path: String) extends Location {
    override def merge(other: Location) = other match {
      case SourceLoc(range2, path2) =>
        require(path2 == path)
        SourceLoc(sourceRange.merge(range2), path)
      case Unknown => this
      case _       => throw new IllegalArgumentException
    }

    override def toString = {
      s"$path$sourceRange"
    }

    override def range = Some(sourceRange)
  }

  /**
   * Generate a location for a provided range and path.
   * @return the resulting location
   */
  def apply(fromLine: Int, fromColumn: Int, toLine: Int, toColumn: Int, path: String): Location = {
    SourceLoc(SourceRange(fromLine, fromColumn, toLine, toColumn), path)
  }

  /**
   * Return a location generated from the provided ANTLR token.
   * @param token the token to generate the location for
   * @return the resulting location
   */
  def fromToken(token: Token): Location = {
    val lineBreaks = token.getText.count(_ == '\n')
    val lastLineLength = token.getText.split("\n", -1).last.length
    val fromColumn = token.getCharPositionInLine + 1
    val toColumn = if (lineBreaks > 0) lastLineLength else fromColumn + lastLineLength
    val range = SourceRange(
      fromLine = token.getLine,
      fromColumn = fromColumn,
      toLine = token.getLine + lineBreaks,
      toColumn = toColumn
    )
    SourceLoc(range, token.getInputStream.getSourceName)
  }

  /**
   * Return a location generated from the provided ANTLR node.
   * @param node the node to generate the location for
   * @return the resulting location
   */
  def fromToken(node: TerminalNode): Location = fromToken(node.getSymbol)

  def fromNode(node: ParserRuleContext): Location = {
    fromToken(node.start).merge(fromToken(node.stop))
  }

  /**
   * Generates a location covering an entire file.
   * @param fileContents the contents of the file
   * @param path the path of the source
   * @return the resulting location
   */
  def forWholeFile(fileContents: String, path: String): Location = {
    val lines = fileContents.split("\\r?\\n")
    Location(
      fromLine = 0,
      fromColumn = 0,
      toLine = lines.length - 1,
      toColumn = lines.last.length,
      path = path
    )
  }

  private case object Unknown extends Location {
    override def merge(other: Location) = other

    override def toString = path

    override def path = "<unknown location>"

    override def range = None
  }

  /**
   * A constant for an unknown location.
   */
  def unknown: Location = Unknown

  private case object BuiltIn extends Location {
    override def merge(other: Location) = other

    override def toString = path

    override def path = "<built-in>"

    override def range = None
  }

  /**
   * A constant for a built-in location.
   */
  def builtIn: Location = BuiltIn

  private case class Opt(name: String) extends Location {
    override def merge(other: Location) = other

    override def toString = path

    override def path = s"option '$name'"

    override def range = None
  }

  def option(name: String): Location = Opt(name)
}

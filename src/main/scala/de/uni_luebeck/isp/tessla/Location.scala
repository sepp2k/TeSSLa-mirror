package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom
import Location._
import org.antlr.v4.runtime.{ParserRuleContext, Token}

sealed abstract class Location {
  def merge(other: Location): Location
  def stackTrace: Seq[Location] = Seq()
  def withStackTrace(stackStrace: Seq[Location]) = LocationWithStackTrace(this, stackStrace)
  def range: Option[SourceRange]
  def path: String
}

case class SourceRange(fromLine: Int, fromColumn: Int, toLine: Int, toColumn: Int) {
  override def toString = s"($fromLine,$fromColumn - $toLine,$toColumn)"

  def merge(other: SourceRange): SourceRange = {
    require(fromLine < other.fromLine || fromLine == other.fromLine && fromColumn <= other.fromColumn)
    require(toLine < other.toLine || toLine == other.toLine && toColumn <= other.toColumn)
    SourceRange(fromLine, fromColumn, other.toLine, other.toColumn)
  }
}

object Location {
  case class LocationWithStackTrace(loc: Location, override val stackTrace: Seq[Location]) extends Location {
    def merge(other: Location) = {
      require(stackTrace == other.stackTrace)
      LocationWithStackTrace(loc.merge(other.asInstanceOf[LocationWithStackTrace].loc), stackTrace)
    }
    def path = loc.path
    def range = loc.range
  }

  private case class SourceLoc(sourceRange: SourceRange, path: String) extends Location {
    override def merge(other: Location) = other match {
      case SourceLoc(range2, path2) =>
        require(path2 == path)
        SourceLoc(sourceRange.merge(range2), path)
      case Unknown => this
      case _ => throw new IllegalArgumentException
    }

    override def toString = {
      s"$path$sourceRange"
    }

    override def range = Some(sourceRange)
  }

  def apply(fromLine: Int, fromColumn: Int, toLine: Int, toColumn: Int, path: String): Location = {
    SourceLoc(SourceRange(fromLine, fromColumn, toLine, toColumn), path)
  }

  def apply(loc: compacom.Location, path: String): Location = {
    Location(
      fromLine = loc.from.line,
      fromColumn = loc.from.column,
      toLine = loc.to.line,
      toColumn = loc.to.column,
      path = path
    )
  }

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

  def fromNode(node: ParserRuleContext): Location = {
    fromToken(node.start).merge(fromToken(node.stop))
  }

  def forWholeFile(fileContents: String, path: String): Location = {
    val lines = fileContents.split("\\n")
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

    def path = "<unknown location>"

    def range = None
  }

  def unknown: Location = Unknown

  private case object BuiltIn extends Location {
    override def merge(other: Location) = other

    override def toString = path

    def path = "<built-in>"

    def range = None
  }

  def builtIn: Location = BuiltIn
}
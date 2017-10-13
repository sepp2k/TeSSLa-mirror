package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom
import de.uni_luebeck.isp.compacom.Position

sealed abstract class Location {
  def merge(other: Location): Location
}

object Location {
  private case class SourceLoc(loc: compacom.Location, path: String) extends Location {
    override def merge(other: Location) = other match {
      case SourceLoc(loc2, path2) =>
        require(path2 == path)
        SourceLoc(loc.merge(loc2), path)
      case Unknown => this
      case _ => throw new IllegalArgumentException
    }

    override def toString = (if (path != "") {
      path
    } else {
      "<stdin>"
    }) + loc.toString
  }

  def apply(loc: compacom.Location, path: String): Location = {
    SourceLoc(loc, path)
  }

  def apply(from: Position, to: Position, path: String): Location = {
    SourceLoc(compacom.Location(from, to), path)
  }

  private case object Unknown extends Location {
    override def merge(other: Location) = other

    override def toString = "(unknown location)"
  }

  def unknown: Location = Unknown
}
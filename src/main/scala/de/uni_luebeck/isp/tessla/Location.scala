package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom

abstract class Location {
  def merge(other: Location): Location
}

case class SourceLoc(loc: compacom.Location, path: String) extends Location {
  override def merge(other: Location) = other match {
    case SourceLoc(loc2, path2) =>
      require(path2 == path)
      SourceLoc(loc.merge(loc2), path)
    case UnknownLoc => this
    case _ => throw new IllegalArgumentException
  }

  override def toString = (if (path != "") {
    path
  } else {
    "<stdin>"
  }) + loc.toString
}

case object UnknownLoc extends Location {
  override def merge(other: Location) = other

  override def toString = "(unknown location)"
}
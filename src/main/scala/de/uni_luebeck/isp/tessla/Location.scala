package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom

abstract class Location {
  def merge(other: Location): Location
}

case class SourceLoc(loc: compacom.Location, fileName: String) extends Location {
  override def merge(other: Location) = other match {
    case SourceLoc(loc2, fileName2) =>
      require(fileName2 == fileName)
      SourceLoc(loc.merge(loc2), fileName)
    case UnknownLoc => this
  }

  override def toString = loc.toString
}

case object UnknownLoc extends Location {
  override def merge(other: Location) = other

  override def toString = "(unknown location)"
}
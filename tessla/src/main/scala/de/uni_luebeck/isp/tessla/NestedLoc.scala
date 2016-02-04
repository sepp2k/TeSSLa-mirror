package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.Location

abstract class NestedLoc {
  def merge(other: NestedLoc): NestedLoc
}

case class SourceLoc(loc: Location) extends NestedLoc {
  override def merge(other: NestedLoc) = other match {
    case SourceLoc(loc2) => SourceLoc(loc.merge(loc2))
    case UnknownLoc => this
  }
}

case object UnknownLoc extends NestedLoc {
  override def merge(other: NestedLoc) = other
}
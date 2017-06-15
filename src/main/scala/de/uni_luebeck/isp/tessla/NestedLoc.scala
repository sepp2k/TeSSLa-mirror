package de.uni_luebeck.isp.tessla

case class NestedLoc(locs: List[Location]) {
    def +(loc: Location) = NestedLoc(loc :: locs)
}
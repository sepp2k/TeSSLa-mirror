package de.uni_luebeck.isp.tessla

object TesslaCoreLegacy {

  case class BuiltInOperator(name: String, loc: Location) {
    def value = name

    def withLoc(loc: Location): BuiltInOperator = copy(loc = loc)

    def typ = FunctionType
  }

  case object FunctionType {
    override def toString = "? => ?"
  }
}

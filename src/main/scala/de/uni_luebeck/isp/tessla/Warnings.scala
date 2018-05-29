package de.uni_luebeck.isp.tessla

object Warnings {
  case class ConflictingOut(loc: Location, previous: Location) extends Diagnostic {
    override def message = s"Conflicting out declaration (previous declaration at $previous)"
  }
}

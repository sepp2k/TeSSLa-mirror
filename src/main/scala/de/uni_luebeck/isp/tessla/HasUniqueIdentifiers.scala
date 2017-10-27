package de.uni_luebeck.isp.tessla

trait HasUniqueIdentifiers[NameType] {
  protected def identifierToString(id: Identifier): String

  class Identifier private[HasUniqueIdentifiers](val uid: Int, val name: NameType) {
    override def equals(other: Any) = other match {
      case o: Identifier if o.uid == uid => true
      case _ => false
    }

    override def hashCode() = uid.hashCode()

    override def toString = identifierToString(this)
  }

  class IdentifierFactory {
    var counter = 0
    def makeIdentifier(name: NameType) = {
      counter += 1
      new Identifier(counter, name)
    }
  }
}

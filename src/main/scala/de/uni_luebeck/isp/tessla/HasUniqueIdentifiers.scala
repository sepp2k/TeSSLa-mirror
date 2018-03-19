package de.uni_luebeck.isp.tessla

trait HasUniqueIdentifiers {
  class Identifier private[HasUniqueIdentifiers](val uid: Long, val nameOpt: Option[String]) {
    override def equals(other: Any) = other match {
      case o: Identifier if o.uid == uid => true
      case _ => false
    }

    override def hashCode() = uid.hashCode()

    override def toString = {
      val name = nameOpt.getOrElse("")
      name + "$" + uid
    }
  }

  trait IdentifierFactory {
    var identifierCounter = 0l
    def makeIdentifier(nameOpt: Option[String]): Identifier = {
      identifierCounter += 1
      new Identifier(identifierCounter, nameOpt)
    }

    def makeIdentifier(): Identifier = makeIdentifier(None)

    def makeIdentifier(name: String): Identifier = makeIdentifier(Some(name))
  }
}

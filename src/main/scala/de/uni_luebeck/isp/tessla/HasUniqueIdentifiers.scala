package de.uni_luebeck.isp.tessla

trait HasUniqueIdentifiers {
  class Identifier private[HasUniqueIdentifiers](val uid: Int, val nameOpt: Option[String]) {
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
    var counter = 0
    def makeIdentifier(nameOpt: Option[String]): Identifier = {
      counter += 1
      new Identifier(counter, nameOpt)
    }

    def makeIdentifier(): Identifier = makeIdentifier(None)

    def makeIdentifier(name: String): Identifier = makeIdentifier(Some(name))
  }
}

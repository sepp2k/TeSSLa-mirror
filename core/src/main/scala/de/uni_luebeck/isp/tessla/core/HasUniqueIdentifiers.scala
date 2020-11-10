/*

 */

package de.uni_luebeck.isp.tessla.core

/**
 * Provides an implementation for Identifiers and a factory for them, ensuring that they are unique.
 */
trait HasUniqueIdentifiers {

  class Identifier private[HasUniqueIdentifiers] (val uid: Long, val nameOpt: Option[String]) {
    override def equals(other: Any) = other match {
      case o: Identifier if o.uid == uid => true
      case _                             => false
    }

    override def hashCode() = uid.hashCode()

    override def toString = {
      val name = nameOpt.getOrElse("")
      name + "$" + uid
    }
  }

  /**
   * A factory for identifiers, which can create identifiers from a given name or without any name, appending them with
   * a UUID.
   * @param counter
   */
  abstract class IdentifierFactory(private var counter: Long = 0L) {
    protected def identifierCounter = counter

    protected def makeIdentifier(nameOpt: Option[String]): Identifier = {
      counter += 1
      new Identifier(counter, nameOpt)
    }

    protected def makeIdentifier(): Identifier = makeIdentifier(None)

    protected def makeIdentifier(name: String): Identifier = makeIdentifier(Some(name))
  }

}

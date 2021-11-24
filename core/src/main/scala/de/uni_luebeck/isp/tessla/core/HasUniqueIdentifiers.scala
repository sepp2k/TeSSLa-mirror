/*
 * Copyright 2021 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

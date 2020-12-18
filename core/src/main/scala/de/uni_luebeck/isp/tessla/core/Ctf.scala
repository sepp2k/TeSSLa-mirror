/*
 * Copyright 2020 The TeSSLa Community
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

import de.uni_luebeck.isp.tessla.core.Errors.{CtfInvalidPrefix, CtfKeyNotFound, CtfTypeError}
import org.eclipse.tracecompass.ctf.core.event.IEventDefinition
import org.eclipse.tracecompass.ctf.core.event.types.{
  ICompositeDefinition,
  IDefinition,
  IntegerDefinition,
  StringDefinition
}

/**
 * Helper functions for use with CTF traces.
 * */
object Ctf {

  /**
   * Find the value addressed by the given key and return it as a String (if valid).
   *
   * @param event the event definition
   * @param key the key to search
   * @param loc the location
   * @return
   */
  def getString(event: IEventDefinition, key: String, loc: Location): String = {
    getDefinition(event, key, loc) match {
      case Some(str: StringDefinition) => str.getValue
      case Some(_)                     => throw CtfTypeError(key, "String", loc)
      case _                           => throw CtfKeyNotFound(key, loc)
    }
  }

  /**
   * Find the value addressed by the given key and return it as a BigInt (if valid).
   *
   * @param event the event definition
   * @param key the key to search
   * @param loc the location
   * @return
   */
  def getInt(event: IEventDefinition, key: String, loc: Location): BigInt = {
    getDefinition(event, key, loc) match {
      case Some(i: IntegerDefinition) => BigInt(i.getIntegerValue)
      case Some(_)                    => throw CtfTypeError(key, "Int", loc)
      case _                          => throw CtfKeyNotFound(key, loc)
    }
  }

  /**
   * Find a definition for a given key name. The key can address a specific part of the event definition by
   * using a prefix:suffix notation. Valid prefixes are "context", "eventcontext", "eventheader", "packetcontext" and
   * "fields".
   *
   * @param event the event definition
   * @param key the key to search
   * @param loc the location
   * @return
   */
  def getDefinition(event: IEventDefinition, key: String, loc: Location): Option[IDefinition] = {
    if (key.contains(":")) {
      val Array(prefix, suffix) = key.split(":", 2)
      val fields = prefix match {
        case "context"       => event.getContext
        case "eventcontext"  => event.getEventContext
        case "eventheader"   => event.getEventHeader
        case "packetcontext" => event.getPacketContext
        case "fields"        => event.getFields
        case x               => throw CtfInvalidPrefix(prefix, loc)
      }
      getDefinition(fields, suffix)
    } else {
      getDefinition(event.getFields, key)
    }
  }

  /**
   * Find the definition for a given key name, where the path is separated by dots.
   *
   * @param composite the composite definition containing the fields
   * @param key the key to search
   * @return
   */
  def getDefinition(composite: ICompositeDefinition, key: String): Option[IDefinition] = {
    if (composite.getFieldNames.contains(key)) {
      Some(composite.getDefinition(key))
    } else {
      val path = key.split('.')
      val head = path.head
      val tail = path.tail.mkString(".")
      if (composite.getFieldNames.contains(head)) {
        composite.getDefinition(head) match {
          case c: ICompositeDefinition => getDefinition(c, tail)
          case _                       => None
        }
      } else {
        None
      }
    }
  }
}

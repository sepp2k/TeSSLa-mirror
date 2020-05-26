package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{CtfInvalidPrefix, CtfKeyNotFound, CtfTypeError}
import org.eclipse.tracecompass.ctf.core.event.IEventDefinition
import org.eclipse.tracecompass.ctf.core.event.types.{
  ICompositeDefinition,
  IDefinition,
  IntegerDefinition,
  StringDefinition
}

object Ctf {
  def getString(event: IEventDefinition, key: String, loc: Location): String = {
    getDefinition(event, key, loc) match {
      case Some(str: StringDefinition) => str.getValue
      case Some(_)                     => throw CtfTypeError(key, "String", loc)
      case _                           => throw CtfKeyNotFound(key, loc)
    }
  }

  def getInt(event: IEventDefinition, key: String, loc: Location): BigInt = {
    getDefinition(event, key, loc) match {
      case Some(i: IntegerDefinition) => BigInt(i.getIntegerValue)
      case Some(_)                    => throw CtfTypeError(key, "Int", loc)
      case _                          => throw CtfKeyNotFound(key, loc)
    }
  }

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

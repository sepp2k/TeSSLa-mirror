package de.uni_luebeck.isp.tessla

import org.eclipse.tracecompass.ctf.core.event.types.{ICompositeDefinition, IDefinition, IntegerDefinition, StringDefinition}

object Ctf {
  def getString(composite: ICompositeDefinition, key: String): String = {
    getDefinition(composite, key) match {
      case Some(str: StringDefinition) => str.getValue
      case Some(_) => throw new RuntimeException(s"""Value for key "$key" is not of type String""")
      case _ => throw new RuntimeException(s"""Key "$key" not found""")
    }
  }

  def getInt(composite: ICompositeDefinition, key: String): BigInt = {
    getDefinition(composite, key) match {
      case Some(i: IntegerDefinition) => BigInt(i.getIntegerValue)
      case Some(_) => throw new RuntimeException(s"""Value for key "$key" is not of type Int""")
      case _ => throw new RuntimeException(s"""Key "$key" not found""")
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
          case _ => None
        }
      } else {
        None
      }
    }
  }
}

package de.uni_luebeck.isp.tessla

abstract sealed class Type {
}

case class SimpleType(name: String) extends Type {
  override def toString = name
}

case class GenericType(name: String, args: Seq[Type]) extends Type {
  override def toString = name + "<" + args.map(_.toString).mkString(", ") + ">"
}

// This is not a case class as all TypeVars are distinct!
class TypeVar extends Type {
  override def toString = "#" + (System.identityHashCode(this) % 100000)
}

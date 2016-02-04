package de.uni_luebeck.isp.tessla

abstract class Type

case class SimpleType(name: String) extends Type
case class GenericType(name: String, args: Seq[Type]) extends Type
case class UInt(size: Int) extends Type
case class Int(size: Int) extends Type
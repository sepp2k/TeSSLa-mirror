package de.uni_luebeck.isp.tessla

abstract class Diagnostic extends Exception with Product {
  override def toString = productPrefix + productIterator.mkString("(", ",", ")")
}

abstract class Fatal extends Diagnostic

case class GiveUp() extends Fatal
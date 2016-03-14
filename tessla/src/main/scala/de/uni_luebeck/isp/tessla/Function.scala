package de.uni_luebeck.isp.tessla

// TODO This is preliminary

abstract class Function {
  val name: String
  val signature: FunctionSig
}

case class SimpleFunction(name: String, signature: FunctionSig) extends Function

case class TypeAscription(`type`: Type) extends Function {
  val name = "type: " + `type`.toString
  val signature = FunctionSig(`type`, Seq((None, `type`)))
}

object Function {
  val defaultFunctions = Seq(
    SimpleFunction("add", FunctionSig(SimpleType("Int"), Seq(
      (None, SimpleType("Int")), (None, SimpleType("Int"))))),
    SimpleFunction("sub", FunctionSig(SimpleType("Int"), Seq(
      (None, SimpleType("Int")), (None, SimpleType("Int")))))
  )
}
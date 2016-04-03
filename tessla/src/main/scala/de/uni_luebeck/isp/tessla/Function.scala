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

case class ConstantValue[T](`type`: Type, value: T) extends Function {
  val name = "constant: " + value.toString + ": " + `type`.toString
  val signature = FunctionSig(`type`, Seq())
}

object Function {

  private val a = new TypeVar

  val defaultFunctions = Seq(
    SimpleFunction("add", FunctionSig(SimpleType("Int"), Seq(
      (None, SimpleType("Int")), (None, SimpleType("Int"))))),
    SimpleFunction("add", FunctionSig(GenericType("Signal", Seq(SimpleType("Int"))), Seq(
      (None, GenericType("Signal", Seq(SimpleType("Int")))), (None, GenericType("Signal", Seq(SimpleType("Int"))))))),
    SimpleFunction("sub", FunctionSig(SimpleType("Int"), Seq(
      (None, SimpleType("Int")), (None, SimpleType("Int"))))),
    SimpleFunction("constantSignal", FunctionSig(GenericType("Signal", Seq(a)), Seq((None, a))))
  )
}
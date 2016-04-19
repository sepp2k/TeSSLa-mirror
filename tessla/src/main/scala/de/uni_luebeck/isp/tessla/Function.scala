package de.uni_luebeck.isp.tessla

// TODO This is preliminary

abstract class Function {
  val name: String
  val signature: FunctionSig
  //val eval: (Any) => Any = identity
}

trait Semantics {
  def apply(i: Seq[Any]): Any
}

case class SimpleFunction(name: String, signature: FunctionSig) extends Function {
  def withSemantics(f:(Seq[Any]) => Any): (SimpleFunction with Semantics) = {
    new SimpleFunction(name, signature) with Semantics {
      def apply(args: Seq[Any]): Any = f(args)
  }}
}

case class TypeAscription[T](`type`: Type) extends Function {
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
    SimpleFunction("add", FunctionSig(
      SimpleType("Int"),
      Seq((None, SimpleType("Int")), (None, SimpleType("Int")))
    )).withSemantics{
      args:Seq[Any] => args(0).asInstanceOf[BigInt] + args(1).asInstanceOf[BigInt]
    },
    SimpleFunction("add", FunctionSig(GenericType("Signal", Seq(SimpleType("Int"))), Seq(
      (None, GenericType("Signal", Seq(SimpleType("Int")))), (None, GenericType("Signal", Seq(SimpleType("Int"))))))),
    SimpleFunction("sub", FunctionSig(
      SimpleType("Int"),
      Seq((None, SimpleType("Int")), (None, SimpleType("Int")))
    )).withSemantics{
      args => args(0).asInstanceOf[BigInt] - args(1).asInstanceOf[BigInt]
    },
    SimpleFunction("constantSignal", FunctionSig(GenericType("Signal", Seq(a)), Seq((None, a))))
  )
}

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

  import de.uni_luebeck.isp.tessla.util.SimpleFunctionDSL._

  private val a,b = new TypeVar

  val defaultFunctions = Seq(
    /**** Literal functions (should come with semantics) ****/
    /*SimpleFunction("add", FunctionSig(
      SimpleType("Int"),
      Seq((None, SimpleType("Int")), (None, SimpleType("Int")))
    )).withSemantics{
      args:Seq[Any] => args(0).asInstanceOf[BigInt] + args(1).asInstanceOf[BigInt]
    },*/
    (Func("add") from "Int") × "Int" → "Int" withSemantics {args:Seq[Any] => args(0).asInstanceOf[BigInt] + args(1).asInstanceOf[BigInt]},
    (Func("sub").from("Int") × "Int" → "Int") withSemantics {args:Seq[Any] => args(0).asInstanceOf[BigInt] - args(1).asInstanceOf[BigInt]},

    /*SimpleFunction("sub", FunctionSig(
      SimpleType("Int"),
      Seq((None, SimpleType("Int")), (None, SimpleType("Int")))
    )).withSemantics{
      args => args(0).asInstanceOf[BigInt] - args(1).asInstanceOf[BigInt]
    },*/

    /**** Input/Constant functions ****/
    SimpleFunction("constantSignal", FunctionSig(GenericType("Signal", Seq(a)), Seq((None, a)))),
    SimpleFunction("instruction_executions", FunctionSig(GenericType("Events", Seq(SimpleType("Unit"))), Seq((None, SimpleType("String"))))),
    SimpleFunction("function_calls", FunctionSig(GenericType("Events", Seq(SimpleType("Unit"))), Seq((None, SimpleType("String"))))),


    /**** Stream operators ****/
    SimpleFunction("add", FunctionSig(
      GenericType("Signal", Seq(SimpleType("Int"))),
      Seq((None, GenericType("Signal", Seq(SimpleType("Int")))), (None, GenericType("Signal", Seq(SimpleType("Int"))))))),
    SimpleFunction("sub", FunctionSig(
      GenericType("Signal", Seq(SimpleType("Int"))),
      Seq((None, GenericType("Signal", Seq(SimpleType("Int")))), (None, GenericType("Signal", Seq(SimpleType("Int"))))))),
    SimpleFunction("eventCount", FunctionSig(
      GenericType("Signal", Seq(SimpleType("Int"))),
      Seq((None, GenericType("Events", Seq(a)))))),

    Func ("occursAll") from Events(a) and Events(b) to Events("Unit")


  )
}

/*
define writeElement := instruction_executions("main.c:49")
define processElement := function_calls("main.c:process_data")

define difference := eventCount(processElement) - eventCount(writeElement)
define error := on readElement if geq(difference,1)
*/
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
  
case class MonitorFunction(name: String, signature: FunctionSig) extends Function

case class StateMachineFunction(name: String, signature: FunctionSig, start:String, stateMap: Map[String,String], transitionList: List[(String,Set[Int],String)]) extends Function

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
    // Func("add").from ("Int") × "Int" → "Int" withSemantics {args:Seq[Any] => args(0).asInstanceOf[BigInt] + args(1).asInstanceOf[BigInt]},
    // Func("sub").from ("Int") × "Int" → "Int" withSemantics {args:Seq[Any] => args(0).asInstanceOf[BigInt] - args(1).asInstanceOf[BigInt]},

    /**** Input/Constant functions ****/
    // SimpleFunction("constantSignal", FunctionSig(GenericType("Signal", Seq(a)), Seq((None, a)))),
    // SimpleFunction("instruction_executions", FunctionSig(GenericType("Events", Seq(SimpleType("Unit"))), Seq((None, SimpleType("String"))))),
    //SimpleFunction("function_calls", FunctionSig(GenericType("Events", Seq(SimpleType("Unit"))), Seq((None, SimpleType("String"))))),
    // SimpleFunction("function_returns", FunctionSig(GenericType("Events", Seq(SimpleType("Unit"))), Seq((None, SimpleType("String"))))),
    SimpleFunction("literal", FunctionSig(GenericType("Signal", Seq(a)), Seq((None, a)))),
    Func("instruction_executions"). from("String")  → Events("Unit"),
    Func("function_returns").       from("String")  → Events("Unit"),
    Func("function_calls").         from("String")  → Events("Unit"),
    Func("variable_values").        from("String")  → Signal(a),
    // Func("input_vector_timestamps"). from () to Events("Int"), // TODO: will be Events("Time")
    // Func("input_vector_ownerships"). from () to Events("Int"),
    // Func("anyEvent") from () to Events("Unit"),
    Func ("out").        from (a) × "String"                              → "Unit",

    /**** Lifted ****/
    Func ("signalAbs").  from (Signal("Int"))                             → Signal("Int"),
    Func ("eventAbs").   from (Events("Int"))                             → Events("Int"),
    Func ("add").        from (Signal("Int"))       × Signal("Int")       → Signal("Int"),
    Func ("and").        from (Signal("Boolean"))   × Signal("Boolean")   → Signal("Boolean"),
    Func ("div").        from (Signal("Int"))       × Signal("Int")       → Signal("Int"),
    Func ("eq").         from (Signal(a))           × Signal(a)           → Signal("Boolean"),
    Func ("geq").        from (Signal("Int"))       × Signal("Int")       → Signal("Boolean"),
    Func ("gt").         from (Signal("Int"))       × Signal("Int")       → Signal("Boolean"),
    Func ("implies").    from (Signal("Boolean"))   × Signal("Boolean")   → Signal("Boolean"),
    Func ("leq").        from (Signal("Int"))       × Signal("Int")       → Signal("Boolean"),
    Func ("lt").         from (Signal("Int"))       × Signal("Int")       → Signal("Boolean"),
    Func ("max").        from (Signal("Int"))       × Signal("Int")       → Signal("Int"),
    Func ("min").        from (Signal("Int"))       × Signal("Int")       → Signal("Int"),
    Func ("mul").        from (Signal("Int"))       × Signal("Int")       → Signal("Int"),
    Func ("neg").        from (Signal("Int"))                             → Signal("Int"),
    Func ("signalNot").  from (Signal("Boolean"))                         → Signal("Boolean"),
    Func ("eventNot").   from (Events("Boolean"))                         → Events("Boolean"),
    Func ("or").         from (Signal("Boolean"))   × Signal("Boolean")   → Signal("Boolean"),
    Func ("sub").        from (Signal("Int"))       × Signal("Int")       → Signal("Int"),

    /**** Filter ****/
    Func ("merge").      from (Events(a))           × Events(a)           → Events(a),
    Func ("filter").     from (Events(a))           × Signal("Boolean")   → Events(a),
    Func ("ifThen").     from (Events(a))           × Signal(b)           → Events(b),
    Func ("ifThenElse"). from (Signal("Boolean"))   × Signal(a) × Signal(a) → Signal(a),
    Func ("changeOf").   from (Signal(a))                                 → Events(a),
    Func ("sample").     from (Signal(a))           × Events(b)           → Events(a),
    Func ("occurAll").   from (Events(a))           × Events(b)           → Events("Unit"),
    Func ("occurAny").   from (Events(a))           × Events(b)           → Events("Unit"),

    /**** Aggregation ****/
    Func ("signalMaximum"). from (Signal("Int"))                             → Signal("Int"), // TODO
    Func ("eventMaximum").  from (Events("Int"))       × "Int"               → Signal("Int"),
    Func ("signalMinimum"). from (Signal("Int"))                             → Signal("Int"),
    Func ("eventMinimum").  from (Events("Int"))       × "Int"               → Signal("Int"),
    Func ("sum").           from (Events("Int"))                             → Signal("Int"),
    Func ("eventCount").    from (Events(a))                                 → Signal("Int"),
    Func ("mrv").           from (Events(a))           × a                   → Signal(a),
    Func ("sma").           from (Events("Int"))       × "Int"               → Events("Int"),

    /**** Timing ****/
    Func ("timestamps").          from (Events(a))                                 → Events("Int"),
    Func ("delaySignalByTime").   from (Signal(a))           × "Int" × a           → Signal(a),
    Func ("delayEventByTime").    from (Events(a))           × "Int"               → Events(a),
    Func ("delayEventByCount").   from (Events(a))           × "Int"               → Events(a),
    // Func ("within").     from ("Int")               × "Int" × Events(a)   → Signal("Boolean"),  //Todo
    Func ("inPast").     from ("Int")               × Events(a)           → Signal("Boolean")
    // Func ("inFuture").   from ("Int")               × Events(a)           → Signal("Boolean")  //Todo

    /**** TODO Synchronization ****/
    // Func ("synchronise").from (Events(a))           × Events(b) × "Int"   → //Todo
  )
}

/*
define writeElement := instruction_executions("main.c:49")
define processElement := function_calls("main.c:process_data")

define difference := eventCount(processElement) - eventCount(writeElement)
define error := on readElement if geq(difference,1)
*/
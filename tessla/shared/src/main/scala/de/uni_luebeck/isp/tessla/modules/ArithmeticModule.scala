package modules

class ArithmeticModule(override val arguments: List[Module], val ao: ArithmeticOperation) extends AppModule(arguments)

sealed abstract class ArithmeticOperation
case class Add() extends ArithmeticOperation
case class And() extends ArithmeticOperation
case class Compare() extends ArithmeticOperation
case class Equal() extends ArithmeticOperation
case class LessThan() extends ArithmeticOperation
case class Multiply() extends ArithmeticOperation
case class Or() extends ArithmeticOperation
case class Shift() extends ArithmeticOperation
case class Sub() extends ArithmeticOperation
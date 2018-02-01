package de.uni_luebeck.isp.tessla

sealed abstract class BuiltIn {
  def name: String

  override def toString = name
}

object BuiltIn {
  case object Default extends BuiltIn {
    override def name = "default"
  }

  case object DefaultFrom extends BuiltIn {
    override def name = "defaultFrom"
  }

  case object Last extends BuiltIn {
    override def name = "last"
  }

  case object Time extends BuiltIn {
    override def name = "time"
  }

  case object DelayedLast extends BuiltIn {
    override def name = "delayedLast"
  }

  case object Const extends BuiltIn {
    override def name = "const"
  }

  case object Merge extends BuiltIn {
    override def name = "merge"
  }

  sealed abstract class PrimitiveOperator extends BuiltIn

  /**
    * Take a variable amount of arguments and return the first one
    */
  case object First extends PrimitiveOperator {
    override def name = "first"
  }

  /**
    * Marker "interface" that makes applications of this operator be printed as "$name $operand" instead of
    * "$name($operand)"
    */
  sealed abstract class PrefixOperator extends PrimitiveOperator

  /**
    * Marker "interface" that makes applications of this operator be printed as "$operand1 $name $operand2"
    * instead of "$name($operand1, $operand2)"
    */
  sealed abstract class InfixOperator extends PrimitiveOperator

  case object Add extends InfixOperator {
    override def name = "+"
  }

  case object Sub extends InfixOperator {
    override def name = "-"
  }

  case object Mul extends InfixOperator {
    override def name = "*"
  }

  case object Div extends InfixOperator {
    override def name = "/"
  }

  case object BitAnd extends InfixOperator {
    override def name = "&"
  }

  case object BitOr extends InfixOperator {
    override def name = "|"
  }

  case object BitXor extends InfixOperator {
    override def name = "^"
  }

  case object LeftShift extends InfixOperator {
    override def name = "<<"
  }

  case object RightShift extends InfixOperator {
    override def name = ">>"
  }

  case object BitFlip extends PrefixOperator {
    override def name = "~"
  }

  case object Negate extends PrefixOperator {
    override def name = "unary -"
  }

  case object Lt extends InfixOperator {
    override def name = "<"
  }

  case object Gt extends InfixOperator {
    override def name = ">"
  }

  case object Lte extends InfixOperator {
    override def name = "<="
  }

  case object Gte extends InfixOperator {
    override def name = ">="
  }

  case object Eq extends InfixOperator {
    override def name = "=="
  }

  case object Neq extends InfixOperator {
    override def name = "!="
  }

  case object And extends InfixOperator {
    override def name = "&&"
  }

  case object Or extends InfixOperator {
    override def name = "||"
  }

  case object Not extends PrefixOperator {
    override def name = "!"
  }

  case object IfThenElse extends PrimitiveOperator {
    override def name = "if then else"
  }

  case object IfThen extends PrimitiveOperator {
    override def name = "if then"
  }

  // Map operators
  case object MapEmpty extends PrimitiveOperator {
    override def name = "map_empty"
  }

  case object MapAdd extends PrimitiveOperator {
    override def name = "map_add"
  }

  case object MapGet extends PrimitiveOperator {
    override def name = "map_get"
  }

  case object MapContains extends PrimitiveOperator {
    override def name = "map_contains"
  }

  case object MapRemove extends PrimitiveOperator {
    override def name = "map_remove"
  }

  // Set operators
  case object SetEmpty extends PrimitiveOperator {
    override def name = "set_empty"
  }

  case object SetAdd extends PrimitiveOperator {
    override def name = "set_add"
  }

  case object SetContains extends PrimitiveOperator {
    override def name = "set_contains"
  }

  case object SetRemove extends PrimitiveOperator {
    override def name = "set_remove"
  }

  def builtIns: Map[String, BuiltIn] = Set(
    Default,
    DefaultFrom,
    Last,
    DelayedLast,
    Time,
    Const,
    Merge,
    Add,
    Sub,
    Negate,
    Mul,
    Div,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
    BitFlip,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
    And,
    Or,
    Not,
    First,
    IfThenElse,
    IfThen,
    MapEmpty,
    MapAdd,
    MapGet,
    MapContains,
    MapRemove,
    SetEmpty,
    SetAdd,
    SetContains,
    SetRemove
  ).map(op => op.name -> op).toMap
}
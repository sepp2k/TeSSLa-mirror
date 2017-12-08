package de.uni_luebeck.isp.tessla

object PrimitiveOperators {
  def ArityError =
    Errors.InternalError("Arity mismatch should have been caught during type checking")

  sealed abstract class PrimitiveOperator

  /**
    * Take a variable amount of arguments and return the first one
    */
  case object First extends PrimitiveOperator {
    override def toString = "first"
  }

  sealed abstract class PrefixOperator extends PrimitiveOperator

  sealed abstract class InfixOperator extends PrimitiveOperator

  case object Add extends InfixOperator {
    override def toString = "+"
  }

  case object Sub extends InfixOperator {
    override def toString = "-"
  }

  case object Mul extends InfixOperator {
    override def toString = "*"
  }

  case object Div extends InfixOperator {
    override def toString = "/"
  }

  case object BitAnd extends InfixOperator {
    override def toString = "&"
  }

  case object BitOr extends InfixOperator {
    override def toString = "|"
  }

  case object BitXor extends InfixOperator {
    override def toString = "^"
  }

  case object LeftShift extends InfixOperator {
    override def toString = "<<"
  }

  case object RightShift extends InfixOperator {
    override def toString = ">>"
  }

  case object BitFlip extends PrefixOperator {
    override def toString = "~"
  }

  case object Negate extends PrefixOperator {
    override def toString = "-"
  }

  case object Lt extends InfixOperator {
    override def toString = "<"
  }

  case object Gt extends InfixOperator {
    override def toString = ">"
  }

  case object Lte extends InfixOperator {
    override def toString = "<="
  }

  case object Gte extends InfixOperator {
    override def toString = ">="
  }

  case object Eq extends InfixOperator {
    override def toString = "=="
  }

  case object Neq extends InfixOperator {
    override def toString = "!="
  }

  case object And extends InfixOperator {
    override def toString = "&&"
  }

  case object Or extends InfixOperator {
    override def toString = "||"
  }

  case object Not extends PrefixOperator {
    override def toString = "!"
  }

  case object IfThenElse extends PrimitiveOperator

  case object IfThen extends PrimitiveOperator

  // Map operators
  case object MapEmpty extends PrimitiveOperator {
    override def toString = "map_empty"
  }

  case object MapAdd extends PrimitiveOperator {
    override def toString = "map_add"
  }

  case object MapGet extends PrimitiveOperator {
    override def toString = "map_get"
  }

  case object MapContains extends PrimitiveOperator {
    override def toString = "map_contains"
  }

  case object MapRemove extends PrimitiveOperator {
    override def toString = "map_remove"
  }

  // Set operators
  case object SetEmpty extends PrimitiveOperator {
    override def toString = "set_empty"
  }

  case object SetAdd extends PrimitiveOperator {
    override def toString = "set_add"
  }

  case object SetContains extends PrimitiveOperator {
    override def toString = "set_contains"
  }

  case object SetRemove extends PrimitiveOperator {
    override def toString = "set_remove"
  }
}

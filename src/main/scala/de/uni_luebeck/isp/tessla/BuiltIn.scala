package de.uni_luebeck.isp.tessla

sealed abstract class BuiltIn {
  def name: String

  override def toString = name
}

object BuiltIn {
  case object Nil extends BuiltIn {
    override def name = "nil"
  }

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

  case object Lift1 extends BuiltIn {
    override def name = "lift1"
  }

  case object Lift extends BuiltIn {
    override def name = "lift"
  }

  case object Lift3 extends BuiltIn {
    override def name = "lift3"
  }

  case object Delay extends BuiltIn {
    override def name = "delay"
  }

  case object Const extends BuiltIn {
    override def name = "const"
  }

  case object Merge extends BuiltIn {
    override def name = "merge"
  }

  case object Filter extends BuiltIn {
    override def name = "filter"
  }

  sealed abstract class PrimitiveOperator extends BuiltIn

  /**
    * Take a variable amount of arguments and return the first one
    */
  case object First extends PrimitiveOperator {
    override def name = "first"
  }

  /**
    * Marker "interface" that makes applications of this operator be printed as "\$name \$operand" instead of
    * "\$name(\$operand)"
    */
  sealed abstract class PrefixOperator extends PrimitiveOperator

  /**
    * Marker "interface" that makes applications of this operator be printed as "\$operand1 \$name \$operand2"
    * instead of "\$name(\$operand1, \$operand2)"
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

  case object Mod extends InfixOperator {
    override def name = "%"
  }

  case object FAdd extends InfixOperator {
    override def name = "+."
  }

  case object FSub extends InfixOperator {
    override def name = "-."
  }

  case object FMul extends InfixOperator {
    override def name = "*."
  }

  case object FDiv extends InfixOperator {
    override def name = "/."
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
    override def name = "unary ~"
  }

  case object Negate extends PrefixOperator {
    override def name = "unary -"
  }

  case object FNegate extends PrefixOperator {
    override def name = "unary -."
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

  case object FLt extends InfixOperator {
    override def name = "<."
  }

  case object FGt extends InfixOperator {
    override def name = ">."
  }

  case object FLte extends InfixOperator {
    override def name = "<=."
  }

  case object FGte extends InfixOperator {
    override def name = ">=."
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
    override def name = "unary !"
  }

  case object IfThenElse extends PrimitiveOperator {
    override def name = "if then else"
  }

  // Int functions
  case object Min extends PrimitiveOperator {
    override def name = "min"
  }

  case object Max extends PrimitiveOperator {
    override def name = "max"
  }

  // Float functions
  case object Pow extends PrimitiveOperator {
    override def name = "pow"
  }

  case object Log extends PrimitiveOperator {
    override def name = "log"
  }

  case object Sin extends PrimitiveOperator {
    override def name = "sin"
  }

  case object Cos extends PrimitiveOperator {
    override def name = "cos"
  }

  case object Tan extends PrimitiveOperator {
    override def name = "tan"
  }

  case object Atan extends PrimitiveOperator {
    override def name = "atan"
  }

  case object IntToFloat extends PrimitiveOperator {
    override def name = "intToFloat"
  }

  case object FloatToInt extends PrimitiveOperator {
    override def name = "floatToInt"
  }

  // Option operators
  case object None extends PrimitiveOperator {
    override def name = "None"
  }

  case object Some extends PrimitiveOperator {
    override def name = "Some"
  }

  case object IsNone extends PrimitiveOperator {
    override def name = "isNone"
  }

  case object GetSome extends PrimitiveOperator {
    override def name = "getSome"
  }

  // Map operators
  case object MapEmpty extends PrimitiveOperator {
    override def name = "Map_empty"
  }

  case object MapAdd extends PrimitiveOperator {
    override def name = "Map_add"
  }

  case object MapGet extends PrimitiveOperator {
    override def name = "Map_get"
  }

  case object MapContains extends PrimitiveOperator {
    override def name = "Map_contains"
  }

  case object MapRemove extends PrimitiveOperator {
    override def name = "Map_remove"
  }

  case object MapSize extends PrimitiveOperator {
    override def name = "Map_size"
  }

  // Set operators
  case object SetEmpty extends PrimitiveOperator {
    override def name = "Set_empty"
  }

  case object SetAdd extends PrimitiveOperator {
    override def name = "Set_add"
  }

  case object SetContains extends PrimitiveOperator {
    override def name = "Set_contains"
  }

  case object SetRemove extends PrimitiveOperator {
    override def name = "Set_remove"
  }

  case object SetSize extends PrimitiveOperator {
    override def name = "Set_size"
  }

  case object SetUnion extends PrimitiveOperator {
    override def name = "Set_union"
  }

  case object SetIntersection extends PrimitiveOperator {
    override def name = "Set_intersection"
  }

  case object SetFold extends PrimitiveOperator {
    override def name = "Set_fold"
  }

  // List operators
  case object ListEmpty extends PrimitiveOperator {
    override def name = "List_empty"
  }

  case object ListSize extends PrimitiveOperator {
    override def name = "List_size"
  }

  case object ListAppend extends PrimitiveOperator {
    override def name = "List_append"
  }

  case object ListPrepend extends PrimitiveOperator {
    override def name = "List_prepend"
  }

  case object ListHead extends PrimitiveOperator {
    override def name = "List_head"
  }

  case object ListTail extends PrimitiveOperator {
    override def name = "List_tail"
  }

  case object ListInit extends PrimitiveOperator {
    override def name = "List_init"
  }

  case object ListLast extends PrimitiveOperator {
    override def name = "List_last"
  }

  case object ListFold extends PrimitiveOperator {
    override def name = "List_fold"
  }

  case object String_concat extends PrimitiveOperator {
    override def name = "String_concat"
  }

  case object ToString extends PrimitiveOperator {
    override def name = "toString"
  }

  // CTF operators
  case object CtfGetString extends PrimitiveOperator {
    override def name = "CTF_getString"
  }

  case object CtfGetInt extends PrimitiveOperator {
    override def name = "CTF_getInt"
  }

  // TeSSLa info object
  case object TesslaInfo extends BuiltIn {
    override def name = "tessla"
  }

  // Standard Library Macros
  case object StdLibCount extends BuiltIn {
    override def name = "count"
  }

  case object StdLibSum extends BuiltIn {
    override def name = "sum"
  }

  case object StdLibMinimum extends BuiltIn {
    override def name = "minimum"
  }

  case object StdLibMaximum extends BuiltIn {
    override def name = "maximum"
  }

  def builtIns: Map[String, BuiltIn] = Set(
    Nil,
    Default,
    DefaultFrom,
    Last,
    Delay,
    Time,
    Lift1,
    Lift,
    Lift3,
    Const,
    Merge,
    Filter,
    Add,
    Sub,
    Negate,
    Mul,
    Div,
    Mod,
    FAdd,
    FSub,
    FMul,
    FDiv,
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
    FLt,
    FGt,
    FLte,
    FGte,
    Eq,
    Neq,
    And,
    Or,
    Not,
    First,
    IfThenElse,
    Min,
    Max,
    Pow,
    Log,
    Sin,
    Cos,
    Tan,
    Atan,
    IntToFloat,
    FloatToInt,
    IsNone,
    None,
    Some,
    GetSome,
    MapEmpty,
    MapAdd,
    MapGet,
    MapContains,
    MapRemove,
    MapSize,
    SetEmpty,
    SetAdd,
    SetContains,
    SetRemove,
    SetSize,
    SetUnion,
    SetIntersection,
    SetFold,
    ListAppend,
    ListEmpty,
    ListSize,
    ListFold,
    ListHead,
    ListInit,
    ListLast,
    ListPrepend,
    ListTail,
    String_concat,
    ToString,
    CtfGetString,
    CtfGetInt,
    TesslaInfo,
    StdLibCount,
    StdLibSum,
    StdLibMinimum,
    StdLibMaximum
  ).map(op => op.name -> op).toMap
}
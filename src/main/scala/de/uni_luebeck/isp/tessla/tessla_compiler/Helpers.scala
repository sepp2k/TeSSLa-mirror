package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{BoolType, DoubleType, FunctionType, LongType}

/**
  * Contains code useful at several locations in the project
  */
object Helpers {

  private val builtinFunctionTypes : Map[String, FunctionType] = Map(
    "__not__" -> FunctionType(Seq(BoolType), BoolType),
    "__negate__" -> FunctionType(Seq(LongType), LongType),
    "__fnegate__"  -> FunctionType(Seq(DoubleType), DoubleType),
    "__bitflip__" -> FunctionType(Seq(LongType), LongType),
    "__and__" -> FunctionType(Seq(BoolType, BoolType), BoolType),
    "__or__" -> FunctionType(Seq(BoolType, BoolType), BoolType),
    "__eq__" -> FunctionType(Seq(LongType, LongType), BoolType),
    "__neq__" -> FunctionType(Seq(LongType, LongType), BoolType),
    "__gt__" -> FunctionType(Seq(LongType, LongType), BoolType),
    "__lt__" -> FunctionType(Seq(LongType, LongType), BoolType),
    "__geq__" -> FunctionType(Seq(LongType, LongType), BoolType),
    "__leq__" -> FunctionType(Seq(LongType, LongType), BoolType),
    "__fgt__" -> FunctionType(Seq(DoubleType, DoubleType), BoolType),
    "__flt__" -> FunctionType(Seq(DoubleType, DoubleType), BoolType),
    "__fgeq__" -> FunctionType(Seq(DoubleType, DoubleType), BoolType),
    "__fleq__" -> FunctionType(Seq(DoubleType, DoubleType), BoolType),
    "__add__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__sub__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__mul__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__div__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__mod__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__bitand__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__bitor__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__bitxor__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__leftshift__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__rightshift__" -> FunctionType(Seq(LongType, LongType), LongType),
    "__fadd__" -> FunctionType(Seq(DoubleType, DoubleType), DoubleType),
    "__fsub__" -> FunctionType(Seq(DoubleType, DoubleType), DoubleType),
    "__fmul__" -> FunctionType(Seq(DoubleType, DoubleType), DoubleType),
    "__fdiv__" -> FunctionType(Seq(DoubleType, DoubleType), DoubleType)
  )

  def getBuiltinFunctionTypes(builtinName: String) : FunctionType = {
    if (builtinFunctionTypes.contains(builtinName)) {
      builtinFunctionTypes(builtinName)
    } else {
      throw new Errors.TranslationError(s"Builtin $builtinName cannot be translated since no type information exists")
    }
  }

}

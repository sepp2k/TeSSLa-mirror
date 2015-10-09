package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.TypeChecker._

object Functions {
  def resolvers(addressWidth: Int): Set[Compiler.Provider] = Set(
    //ConstantStream 
    new SimpleFunctionResolver("constant", simpleSignature(TypeVar(1), StreamType(TypeVar(1)))),
    //IfFunction 
    new SimpleFunctionResolver(
      "if", simpleSignature(StreamType(BoolType), StreamType(TypeVar(1)), StreamType(TypeVar(1)), StreamType(TypeVar(1)))),
    //UndefConstant
    new SimpleFunctionResolver("undef", simpleSignature(ToBeInferred)),

    //AddressOfConstant
    new SimpleFunctionResolver("addressOf", simpleSignature(StringType, IntType(addressWidth, false))),
    //AddFunction 
    new SimpleFunctionResolver(
      "add", simpleSignature(StreamType(IntType(32, false)), StreamType(IntType(32, false)), StreamType(IntType(32, false)))),

    //SubFunction 
    new SimpleFunctionResolver(
      "sub", simpleSignature(StreamType(IntType(32, false)), StreamType(IntType(32, false)), StreamType(IntType(32, false)))),

    //MultiplyFunction
    new SimpleFunctionResolver(
      "multiply", simpleSignature(StreamType(IntType(32, false)), StreamType(IntType(32, false)), StreamType(IntType(32, false)))),

    //ShiftFunction
    new SimpleFunctionResolver(
      "shift", simpleSignature(StreamType(IntType(32, false)), StreamType(IntType(32, false)), StreamType(IntType(32, false)))),

    //GEQComparator
    new SimpleFunctionResolver(
      "geq", simpleSignature(StreamType(IntType(32, false)), StreamType(IntType(32, false)), StreamType(BoolType))),

    //LessThanComparator
    new SimpleFunctionResolver(
      "lessThan", simpleSignature(StreamType(IntType(32, false)), StreamType(IntType(32, false)), StreamType(BoolType))),

    //IfThenFunction
    new SimpleFunctionResolver(
      "if", simpleSignature(StreamType(BoolType), StreamType(TypeVar(1)), StreamType(TypeVar(1)))) {
      override def provideFunction(fn: FunctionSignature) = Some(UnresolvedFunction("ifthen"))
    },
    //Monitor
    new SimpleFunctionResolver(
      "monitor", simpleSignature(StreamType(StringType), StreamType(BoolType), StreamType(BoolType))) {
      override def provideFunction(fn: FunctionSignature) = Some(UnresolvedFunction("monitor1"))
    },
    //Monitor
    new SimpleFunctionResolver(
      "monitor", simpleSignature(StreamType(StringType), StreamType(BoolType),StreamType(BoolType), StreamType(BoolType))) {
      override def provideFunction(fn: FunctionSignature) = Some(UnresolvedFunction("monitor2"))
    },
    new SimpleFunctionResolver(
      "synchronize", simpleSignature(StreamType(TypeVar(1)), StreamType(TypeVar(2)), IntType(32, false), StreamType(BoolType))) {
      override def provideFunction(fn: FunctionSignature) = Some(UnresolvedFunction("synchronize"))
    },
    new SimpleFunctionResolver("not", simpleSignature(StreamType(BoolType), StreamType(BoolType))),
    new SimpleFunctionResolver("and", simpleSignature(StreamType(BoolType), StreamType(BoolType), StreamType(BoolType))),
    new SimpleFunctionResolver("or", simpleSignature(StreamType(BoolType), StreamType(BoolType), StreamType(BoolType))),
    new SimpleFunctionResolver("implies", simpleSignature(StreamType(BoolType), StreamType(BoolType), StreamType(BoolType)))
  )
}
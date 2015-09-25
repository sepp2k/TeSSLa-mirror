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

    //GEQComparator
    new SimpleFunctionResolver(
      "geq", simpleSignature(StreamType(IntType(32, false)), StreamType(IntType(32, false)), StreamType(BoolType))),

    //IfThenFunction
    new SimpleFunctionResolver(
      "ifthen", simpleSignature(StreamType(BoolType), StreamType(TypeVar(1)), StreamType(TypeVar(1)))) {
      override def provideFunction(fn: FunctionSignature) = Some(UnresolvedFunction("ifthen"))
    })
}
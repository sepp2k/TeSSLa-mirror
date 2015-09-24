package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.TypeChecker._

object Functions {
  object ConstantStream extends SimpleFunctionResolver(
    "constant", simpleSignature(TypeVar(1), StreamType(TypeVar(1))))

  object IfFunction extends SimpleFunctionResolver(
    "if", simpleSignature(BoolType, TypeVar(1), TypeVar(1), TypeVar(1)))

  object UndefConstant extends SimpleFunctionResolver(
    "undef", simpleSignature(ToBeInferred))

}
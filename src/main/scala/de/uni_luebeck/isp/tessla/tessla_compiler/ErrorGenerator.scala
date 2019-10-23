package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{DoubleValue, Equal, FunctionCall, FunctionType, ImpLanExpr, ImpLanType, LongValue, NotEqual, TernaryExpression}

object ErrorGenerator {

  val DivZeroErrorCode = (1 << 0)

  def generateErrorCodeExpressionSLift(name: String, args: Seq[(ImpLanExpr, ImpLanType)]) : ImpLanExpr = {
    name match {
      case "__div__" => TernaryExpression(Seq(Seq(NotEqual(args(1)._1, LongValue(0)))), LongValue(0), LongValue(DivZeroErrorCode))
      case "__fdiv__" => TernaryExpression(Seq(Seq(NotEqual(args(1)._1, DoubleValue(0)))), LongValue(0), LongValue(DivZeroErrorCode))
      case "__mod__" => TernaryExpression(Seq(Seq(NotEqual(args(1)._1, LongValue(0)))), LongValue(0), LongValue(DivZeroErrorCode))
      case _ => LongValue(0)
    }
  }

  def generateErrorPreventingCallSLift(name: String, args: Seq[(ImpLanExpr, ImpLanType)], ret: ImpLanType) : ImpLanExpr = {
    val fcall = FunctionCall(name, args.map(_._1), FunctionType(args.map(_._2), ret))
    name match {
      case "__div__" => TernaryExpression(Seq(Seq(NotEqual(args(1)._1, LongValue(0)))), fcall, LongValue(-1))
      case "__fdiv__" => TernaryExpression(Seq(Seq(NotEqual(args(1)._1, DoubleValue(0)))), fcall, DoubleValue(-1))
      case "__mod__" => TernaryExpression(Seq(Seq(NotEqual(args(1)._1, LongValue(0)))), fcall, LongValue(-1))
      case _ => fcall
    }
  }

}

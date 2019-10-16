package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.TesslaCore._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeDSL._

import scala.collection.mutable

object FunctionGenerator {

  var functionIDs : mutable.Map[Any, Int] = mutable.Map()
  var functionImpl : mutable.Map[Int, (Seq[String], Seq[ImpLanType], ImpLanType, Seq[ImpLanStmt])] = mutable.Map()
  var nextFunctionID : Int = 0;

  def generateFunctionCode(f: Function) : String = {
    if (functionIDs.contains(f)) {
      s"intFunc_${functionIDs(f)}"
    } else {
      val currFuncID = nextFunctionID
      nextFunctionID += 1
      functionIDs += (f -> currFuncID)
      //TODO: Function params have no type annotation
      //FIXME: Sort!!!
      functionImpl += (currFuncID -> (f.parameters.map{id => s"intVar_${id.uid}"}, Seq(), LongType, f.body.flatMap(translateAssignment).toSeq))
      s"intFunc_${currFuncID}"
    }
  }

  def generateClosureCode(c : Closure) : String = {
    if (c.capturedEnvironment.size == 0) {
      generateFunctionCode(c.function)
    } else {
      println(c.capturedEnvironment)
      ""
    }
  }

  def translateAssignment(a: (Identifier, ValueExpressionDescription)) : Seq[IntermediateCode.ImpLanStmt] = {
    val id = a._1
    val ved = a._2

    val newStmt = (Seq()
        Assignment(s"intVar_${id.uid}", translateValueExpression(ved.exp), defaultValueForType(ved.typ), ved.typ)
      )

    newStmt
  }

  def translateValueExpression(e : ValueExpression) : ImpLanExpr = {
    e match {
      case _ : Function => throw new Errors.NotYetImplementedError("Function as Value expression is not implemented yet", e.loc)
      case IfThenElse(cond, thenCase, elseCase, loc) => TernaryExpression(Set(Set(translateValueArg(cond))), translateValueArg(thenCase.get), translateValueArg(elseCase.get))
      case Application(f, args, loc) => getValueArgFunc(f.get, args.map(translateValueArg), loc)
      case MemberAccess(obj, member, loc) => throw new Errors.NotYetImplementedError("Member access is not implemented yet", e.loc)
    }
  }

  def translateValueArg(v : ValueArg) : ImpLanExpr = {
    v match {
      case ValueExpressionRef(id) => Variable(s"intVar_${id.uid}")
      case ObjectCreation(members, loc) => throw new Errors.NotYetImplementedError("Object creation is not implemented yet", loc)
      case v: ValueOrError => v
    }
  }

  def getValueArgFunc(v: ValueArg, args: Seq[ImpLanExpr], loc: Location) : ImpLanExpr = {
    v match {
      case BuiltInOperator(name, loc) => getBuiltinOperator(name, args)
      case c: Closure => FunctionCall(generateClosureCode(c), args) //FIXME: Pass environment
      case Error(error) => throw new Errors.CoreASTError(error, error.loc)
      case _ => throw new Errors.CommandNotSupportedError("Application of unsupported value type", loc)
    }
  }

  //TODO: Is this really necessary? can't the function names just be passed into the FunctionCall nodes
  def getBuiltinOperator(name: String, args: Seq[ImpLanExpr]) : ImpLanExpr = {
    name match {
      case "__not__" => Negation(args(0))
      case "__ite__" => TernaryExpression(Set(Set(args(0))), args(1), args(2))
      case "__eq__" => Equal(args(0), args(1))
      case "__neq__" => NotEqual(args(0), args(1))
      case "__leq__" | "__fleq__" => GreaterEqual(args(1), args(0))
      case "__geq__" | "__fgeq__" => GreaterEqual(args(0), args(1))
      case "__lt__" | "__flt__" => Greater(args(1), args(0))
      case "__gt__" | "__fgt__" => Greater(args(0), args(1))
      case "__add__" | "__fadd__" => Addition(args(0), args(1))
      case _ => FunctionCall(name, args)
    }
  }

}

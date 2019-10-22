package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.Location
import de.uni_luebeck.isp.tessla.TesslaCore.{FunctionType, _}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeDSL._

import scala.collection.mutable

object FunctionGenerator {

  def generateLambda(f: Function, callName: String, stack: Map[Any, String] = Map()) : LambdaExpression = {
    generateLambdaInt(f, stack + (f -> callName))
  }

  def generateLambdaInt(f: Function, callNames: Map[Any, String]) : LambdaExpression = {

    def translateAssignment(a: (Identifier, ValueExpressionDescription)) : Seq[IntermediateCode.ImpLanStmt] = {
      val id = a._1
      val ved = a._2
      val e = ved.exp

      e match {
         case Application(f, args, loc)  => getValueArgFunc(s"intVar_${id.uid}", f.get, args.map(translateValueArg), ved.typ, loc)
         case _ => {
           val exp = e match {
             case IfThenElse(cond, thenCase, elseCase, loc) => TernaryExpression(Set(Set(translateValueArg(cond))), translateValueArg(thenCase.get), translateValueArg(elseCase.get))
             case f: Function => if (callNames.contains(f)) {
               generateLambda(f, s"intVar_${id.uid}", callNames)
             } else {
               Variable(callNames(f))
             }
             case MemberAccess(obj, member, loc) => throw new Errors.NotYetImplementedError("Member access is not implemented yet", e.loc)
           }
           Seq() Assignment(s"intVar_${id.uid}", exp, defaultValueForType(ved.typ), ved.typ)
         }
      }
    }

    def translateValueArg(v : ValueArg) : ImpLanExpr = {
      v match {
        case ValueExpressionRef(id) => Variable(s"intVar_${id.uid}")
        case ObjectCreation(members, loc) => throw new Errors.NotYetImplementedError("Object creation is not implemented yet", loc)
        case v: ValueOrError => v
      }
    }

    def getValueArgFunc(target: String, v: ValueArg, args: Seq[ImpLanExpr], t: ValueType, loc: Location) : Seq[ImpLanStmt] = {
      v match {
        case BuiltInOperator(name, loc) => {
          (
            Seq()
            Assignment(target, getBuiltinOperator(name, args), defaultValueForType(t), t)
          )
        }
        case c: Closure => { //FIXME: Pass environment
          if (callNames.contains(c.function)) {
            (
              Seq()
                Assignment(target, FunctionVarApplication(callNames(c.function), args), defaultValueForType(t), t)
              )
          } else {
            val ft = FunctionType
            (
              Seq()
                Assignment(s"${target}_f", generateLambda(c.function, s"${target}_f", callNames), defaultValueForType(ft), ft)
                Assignment(target, FunctionVarApplication(s"${target}_f", args), defaultValueForType(t), t)
              )
          }
        }
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

    def determineEvalOrder(vds: Map[Identifier, ValueExpressionDescription], end: ValueArg) : Seq[(Identifier, ValueExpressionDescription)] = {

      def getDependenciesVA(va: ValueArg) : Seq[Identifier] = {
        va match {
          case ValueExpressionRef(id) => (if (vds.contains(id)) getDependenciesVE(vds(id).exp) else Seq()) ++ Seq(id) //We are only interested in dependencies of the outermost scope
          case c: Closure => getDependenciesF(c.function) //?????
        }
      }

      def getDependenciesVE(ve: ValueExpression) : Seq[Identifier] = {
        ve match {
          case f: Function => getDependenciesF(f) //?????
          case IfThenElse(cond, thenCase, elseCase, loc) => getDependenciesVA(cond) ++ getDependenciesVA(thenCase.get) ++ getDependenciesVA(elseCase.get)
          case Application(f, args, loc) => getDependenciesVA(f.get) ++ args.flatMap(getDependenciesVA)
          case MemberAccess(obj, member, loc) => getDependenciesVA(obj)
        }
      }

      getDependenciesVA(end).filter{id => vds.contains(id)}.map{id => (id, vds(id))}
    }

    def getDependenciesF(f: Function) : Seq[Identifier] = {
      Seq()
    }

    val ret = ReturnStatement(translateValueArg(f.result))
    val stmts = determineEvalOrder(f.body, f.result).flatMap(translateAssignment).toSeq :+ ret
    //TODO: Function params have no type annotation
    LambdaExpression(f.parameters.map{id => s"intVar_${id.uid}"}, Seq(), LongType, stmts)
  }

}

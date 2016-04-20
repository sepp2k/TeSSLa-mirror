package de.uni_luebeck.isp.tessla.util

import de.uni_luebeck.isp.tessla._
import scala.language.implicitConversions

/**
  * Created by nd on 20/04/16.
  */
object SimpleFunctionDSL {

  object Signal extends {
    def apply(innerType: Type) = {
      GenericType("Signal", Seq(innerType))
    }
  }

  object Events {
    def apply(innerType: Type) = {
      GenericType("Events", Seq(innerType))
    }
  }

  implicit def toType(s: String): SimpleType = SimpleType(s)

  class FunctionBuilder(var name: String, var args: Seq[(Option[String], Type)]) {

    def to(returnType: Type): SimpleFunction = {
      SimpleFunction(name, FunctionSig(returnType, args))
    }

    def →(returnType: Type): SimpleFunction = to(returnType)

    def ->(returnType: Type): SimpleFunction = to(returnType)

    def and(`type`: Type): FunctionBuilder = {
      args = (None, `type`) +: args
      this
    }

    def ×(`type`: Type) = and(`type`)

    def x(`type`: Type) = and(`type`)

    def named(argName: String): FunctionBuilder = {
      args = if (args.nonEmpty) (Some(argName), args.head._2) +: (args.tail) else args
      this
    }
  }

  case class Func(name: String) {
    def from(`type`: Type): FunctionBuilder = {
      new FunctionBuilder(name, Seq((None, `type`)))
    }

    def :=(`type`: Type): FunctionBuilder = {
      new FunctionBuilder(name, Seq((None, `type`)))
    }
  }


  /*  val a,b = new TypeVar

    val x = SimpleFunction(
      "add",
      FunctionSig(
        GenericType("Signal", Seq(SimpleType("Int"))),
        Seq((None, GenericType("Signal", Seq(SimpleType("Int")))), (None, GenericType("Signal", Seq(SimpleType("Int")))))))

    val y = Func("occursAll") from Signal(a) and Signal(b) to Signal("Unit")*/

}






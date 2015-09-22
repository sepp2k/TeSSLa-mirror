package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.TypeChecker.FunctionSignature
import de.uni_luebeck.isp.tessla.TypeChecker.FunctionSignature._



object Functions {
  // TODO make this easier to implement for the common cases
  object ConstantStream extends TypeChecker.FunctionResolver {
    override def names: Set[String] = Set("constant")

    override def inferTypes(sig: FunctionSignature): FunctionSignature = {
      if (sig.namedTypes.nonEmpty || sig.argTypes.length != 1) {
        return FunctionSignature.none
      }
      var inputSig = sig.argTypes(0)
      var outputSig = sig.returnType

      outputSig = intersect(inputSig map StreamType, outputSig)
      inputSig = outputSig map {case StreamType(x) => x}

      FunctionSignature(Seq(inputSig), Map(), outputSig)
    }

    override def provideFunction(sig: FunctionSignature): Option[Function] = {
      // TODO implement this
      Some(UnresolvedFunction("constant"))
    }
  }

  object IfFunction extends TypeChecker.FunctionResolver {
    override def names: Set[String] = Set("if")

    override def inferTypes(sig: FunctionSignature): FunctionSignature = {
      if (sig.namedTypes.nonEmpty || sig.argTypes.length != 3) {
        return FunctionSignature.none
      }

      var t = intersect(intersect(sig.argTypes(1), sig.argTypes(2)), sig.returnType)

      FunctionSignature(Seq(Set(BoolType), t, t), Map(), t)
    }

    override def provideFunction(sig: FunctionSignature): Option[Function] = {
      // TODO implement this
      Some(UnresolvedFunction("if"))
    }
  }

  object UndefConstant extends TypeChecker.FunctionResolver {
    override def names: Set[String] = Set("undef")

    override def inferTypes(sig: FunctionSignature): FunctionSignature = {
      if (sig.namedTypes.nonEmpty || sig.argTypes.nonEmpty) {
        FunctionSignature.none
      } else {
        sig
      }
    }

    override def provideFunction(sig: FunctionSignature): Option[Function] = {
      // TODO implement this
      Some(UnresolvedFunction("undef"))
    }
  }
}

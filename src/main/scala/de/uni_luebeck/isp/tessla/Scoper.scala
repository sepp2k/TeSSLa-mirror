package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Scoper extends TranslationPhase[Tessla.Specification, ScopedTessla.Specification] {
  override def translateSpec(spec: Tessla.Specification) = {
    translateStatements(spec.statements)
  }

  def translateStatements(statements: Seq[Tessla.Statement]): ScopedTessla.Specification = {
    val globalScope = new ScopedTessla.Scope()
    val emptySpec = ScopedTessla.Specification(globalScope, Seq(), outAllLocation = None)
    statements.foldLeft(emptySpec) {
      case (result, outAll : Tessla.OutAll) =>
        if(result.outAll) warn(ConflictingOut(outAll.loc, previous = result.outAllLocation.get))
        result.copy(outAllLocation = Some(outAll.loc))

      case (result, out: Tessla.Out) =>
        result.outStreams.find(_.name == out.name).foreach {
          previous =>
            warn(ConflictingOut(out.loc, previous = previous.loc))
        }
        val newOut = ScopedTessla.OutStream(translateExpression(out.expr, globalScope), out.idOpt, out.loc)
        result.copy(outStreams = result.outStreams :+ newOut)

      case (result, definition: Tessla.Definition) =>
        val overload = definitionToOverload(definition, globalScope)
        result.addGlobalOverload(definition.id.name, overload)
        result

      case (result, in: Tessla.In) =>
        val overload = ScopedTessla.Overload(Seq(), ScopedTessla.InStream(in.id, in.streamType, in.loc), in.loc)
        result.addGlobalOverload(in.id.name, overload)
        result
    }
  }

  def definitionToOverload(definition: Tessla.Definition, parentScope: ScopedTessla.Scope): ScopedTessla.Overload = {
    val signature = definition.parameters.map { param =>
      (param.id, param.parameterType)
    }
    ScopedTessla.Overload(signature, translateExpression(definition.body, parentScope), definition.loc)
  }

  def translateExpression(expr: Tessla.Expression, parentScope: ScopedTessla.Scope): ScopedTessla.Expression = expr match {
    case variable: Tessla.Variable =>
      ScopedTessla.Variable(variable.id)

    case literal: Tessla.Literal =>
      ScopedTessla.Literal(literal.value, literal.loc)

    case call: Tessla.MacroCall =>
      val args = call.args.map {
        case arg: Tessla.NamedArgument =>
          ScopedTessla.NamedArgument(arg.id, translateExpression(arg.expr, parentScope))
        case arg: Tessla.PositionalArgument =>
          ScopedTessla.PositionalArgument(translateExpression(arg.expr, parentScope))
      }
      ScopedTessla.MacroCall(call.macroID, args, call.loc)

    case block: Tessla.Block =>
      val innerScope = new ScopedTessla.Scope(Some(parentScope))
      block.definitions.foreach { definition =>
        innerScope.addOverload(definition.id.name, definitionToOverload(definition, innerScope))
      }
      ScopedTessla.Block(innerScope, translateExpression(block.expression, innerScope), block.loc)
  }
}

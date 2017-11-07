package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Scoper extends TranslationPhase[Tessla.Specification, ScopedTessla.Specification] {
  def param(name: String) = {
    val id = Tessla.Identifier(name, Location.builtIn)
    ScopedTessla.Parameter(Tessla.Parameter(id, None))
  }

  def builtIn(op: ScopedTessla.BuiltInOperator, params: String*) = {
    ScopedTessla.Overload(params.map(param), op)
  }

  val stdlib = Seq(
    "default" -> builtIn(ScopedTessla.Default, "values", "default"),
    "last" -> builtIn(ScopedTessla.Last, "values", "clock"),
    "delayedLast" -> builtIn(ScopedTessla.DelayedLast, "values", "delays"),
    "time" -> builtIn(ScopedTessla.Time, "values")
    // TODO: Continue
  )

  override def translateSpec(spec: Tessla.Specification) = {
    val globalScope = new ScopedTessla.Scope()
    stdlib.foreach {
      case (name, overload) =>
        globalScope.addOverload(name, overload)
    }
    val emptySpec = ScopedTessla.Specification(globalScope, Seq(), outAllLocation = None)
    spec.statements.foldLeft(emptySpec) {
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
        addDefinition(globalScope, definition)
        result

      case (result, in: Tessla.In) =>
        result.addGlobalVariable(in.id, ScopedTessla.InStream(in.id, in.streamType, in.loc))
        result
    }
  }

  def addDefinition(scope: ScopedTessla.Scope, definition: Tessla.Definition): Unit = {
    val parameters = definition.parameters.map(ScopedTessla.Parameter)
    val parameterScope = new ScopedTessla.Scope(Some(scope))
    parameters.foreach { param =>
      parameterScope.addVariable(param.id, param)
    }
    val body = translateExpression(definition.body, parameterScope)
    if (parameters.isEmpty) {
      scope.addVariable(definition.id, body)
    } else {
      val mac = ScopedTessla.Macro(parameters, parameterScope, definition.returnType, body, definition.loc)
      scope.addOverload(definition.id.name, ScopedTessla.Overload(parameters, mac))
    }
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
        addDefinition(innerScope, definition)
      }
      ScopedTessla.Block(innerScope, translateExpression(block.expression, innerScope), block.loc)
  }
}

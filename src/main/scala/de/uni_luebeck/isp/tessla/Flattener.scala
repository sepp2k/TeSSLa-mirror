package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Flattener extends TranslationPhase[Tessla.Specification, FlatTessla.Specification] {
  def param(name: String) = {
    val id = Tessla.Identifier(name, Location.builtIn)
    FlatTessla.Parameter(Tessla.Parameter(id, None))
  }

  def builtIn(op: FlatTessla.BuiltInOperator, params: String*) = {
    FlatTessla.Overload(Seq(), params.map(param), op)
  }

  val stdlib = Seq(
    "default" -> builtIn(FlatTessla.Default, "values", "default"),
    "last" -> builtIn(FlatTessla.Last, "values", "clock"),
    "delayedLast" -> builtIn(FlatTessla.DelayedLast, "values", "delays"),
    "time" -> builtIn(FlatTessla.Time, "values")
    // TODO: Continue
  )

  override def translateSpec(spec: Tessla.Specification) = {
    val globalScope = new FlatTessla.Scope()
    stdlib.foreach {
      case (name, overload) =>
        globalScope.addOverload(name, overload)
    }
    val emptySpec = FlatTessla.Specification(globalScope, Seq(), outAllLocation = None)
    spec.statements.foldLeft(emptySpec) {
      case (result, outAll : Tessla.OutAll) =>
        if(result.outAll) warn(ConflictingOut(outAll.loc, previous = result.outAllLocation.get))
        result.copy(outAllLocation = Some(outAll.loc))

      case (result, out: Tessla.Out) =>
        result.outStreams.find(_.name == out.name).foreach {
          previous =>
            warn(ConflictingOut(out.loc, previous = previous.loc))
        }
        val newOut = FlatTessla.OutStream(translateExpression(out.expr, globalScope), out.idOpt, out.loc)
        result.copy(outStreams = result.outStreams :+ newOut)

      case (result, definition: Tessla.Definition) =>
        addDefinition(globalScope, definition)
        result

      case (result, in: Tessla.In) =>
        result.addGlobalVariable(in.id, FlatTessla.InStream(in.id, in.streamType, in.loc))
        result
    }
  }

  def addDefinition(scope: FlatTessla.Scope, definition: Tessla.Definition): Unit = {
    val parameters = definition.parameters.map(FlatTessla.Parameter)
    val parameterScope = new FlatTessla.Scope(Some(scope))
    parameters.foreach { param =>
      parameterScope.addVariable(param.id, param)
    }
    val body = translateExpression(definition.body, parameterScope)
    if (parameters.isEmpty) {
      scope.addVariable(definition.id, body)
    } else {
      val mac = FlatTessla.Macro(definition.typeParameters, parameters, parameterScope, definition.returnType, body, definition.loc)
      scope.addOverload(definition.id.name, FlatTessla.Overload(definition.typeParameters, parameters, mac))
    }
  }

  def translateExpression(expr: Tessla.Expression, parentScope: FlatTessla.Scope): FlatTessla.Expression = expr match {
    case variable: Tessla.Variable =>
      FlatTessla.Variable(variable.id)

    case literal: Tessla.Literal =>
      FlatTessla.Literal(literal.value, literal.loc)

    case call: Tessla.MacroCall =>
      val args = call.args.map {
        case arg: Tessla.NamedArgument =>
          FlatTessla.NamedArgument(arg.id, translateExpression(arg.expr, parentScope))
        case arg: Tessla.PositionalArgument =>
          FlatTessla.PositionalArgument(translateExpression(arg.expr, parentScope))
      }
      FlatTessla.MacroCall(call.macroID, args, call.loc)

    case block: Tessla.Block =>
      val innerScope = new FlatTessla.Scope(Some(parentScope))
      block.definitions.foreach { definition =>
        addDefinition(innerScope, definition)
      }
      FlatTessla.Block(innerScope, translateExpression(block.expression, innerScope), block.loc)
  }
}

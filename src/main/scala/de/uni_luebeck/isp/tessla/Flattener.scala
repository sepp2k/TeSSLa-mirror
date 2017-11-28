package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{MultipleDefinitionsError, UndefinedVariable}
import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Flattener extends FlatTessla.IdentifierFactory with TranslationPhase[Tessla.Specification, FlatTessla.Specification] {
  type IdMap = Map[String, FlatTessla.Identifier]

  val stdlib = Seq(
    "default" -> FlatTessla.VariableEntry(FlatTessla.Default, None),
    "defaultFrom" -> FlatTessla.VariableEntry(FlatTessla.DefaultFrom, None),
    "last" -> FlatTessla.VariableEntry(FlatTessla.Last, None),
    "delayedLast" -> FlatTessla.VariableEntry(FlatTessla.DelayedLast, None),
    "time" -> FlatTessla.VariableEntry(FlatTessla.Time, None)
    // TODO: Continue
  )

  def createIdMap(names: Seq[String]): IdMap = {
    names.map(name => name -> makeIdentifier(name)).toMap
  }

  def getId(statement: Tessla.Statement): Option[Tessla.Identifier] = statement match {
    case definition: Tessla.Definition => Some(definition.id)
    case in: Tessla.In => Some(in.id)
    case _ => None
  }

  def getName(statement: Tessla.Statement): Option[String] = getId(statement).map(_.name)

  def checkForDuplicates(statements: Seq[Tessla.Identifier]): Unit = {
    statements.groupBy(_.name).foreach {
      case (_, duplicates) if duplicates.size > 1 =>
        val firstLoc = duplicates.head.loc
        duplicates.tail.foreach { duplicate =>
          error(MultipleDefinitionsError(duplicate, firstLoc))
        }

      case _ => /* Do nothing */
    }
  }

  override def translateSpec(spec: Tessla.Specification) = {
    val stdlibScope = new FlatTessla.Scope(None)
    val stdlibIdMap = createIdMap(stdlib.map(_._1))
    stdlib.foreach {
      case (name, entry) =>
        stdlibScope.addVariable(stdlibIdMap(name), entry)
    }
    val globalScope = new FlatTessla.Scope(Some(stdlibScope))
    val globalIdMap = stdlibIdMap ++ createIdMap(spec.statements.flatMap(getName))
    val emptySpec = FlatTessla.Specification(globalScope, Seq(), outAllLocation = None)
    checkForDuplicates(spec.statements.flatMap(getId))
    spec.statements.foldLeft(emptySpec) {
      case (result, outAll : Tessla.OutAll) =>
        if(result.outAll) warn(ConflictingOut(outAll.loc, previous = result.outAllLocation.get))
        result.copy(outAllLocation = Some(outAll.loc))

      case (result, out: Tessla.Out) =>
        result.outStreams.find(_.name == out.name).foreach {
          previous =>
            warn(ConflictingOut(out.loc, previous = previous.loc))
        }
        val newOut = FlatTessla.OutStream(translateExpression(out.expr, globalScope, globalIdMap), out.idOpt.map(_.name), out.loc)
        result.copy(outStreams = result.outStreams :+ newOut)

      case (result, definition: Tessla.Definition) =>
        addDefinition(definition, globalScope, globalIdMap)
        result

      case (result, in: Tessla.In) =>
        val entry = FlatTessla.VariableEntry(FlatTessla.InStream(in.id.name, in.streamType, in.loc), Some(in.streamType))
        globalScope.addVariable(globalIdMap(in.id.name), entry)
        result
    }
  }

  def addDefinition(definition: Tessla.Definition, scope: FlatTessla.Scope, idMap: IdMap): Unit = {
    val parameters = definition.parameters.map(FlatTessla.Parameter)
    val (innerDefs, exp) = definition.body match {
      case block: Tessla.Block => (block.definitions, block.expression)
      case e => (Seq(), e)
    }
    checkForDuplicates(parameters.map(_.nameWithLoc) ++ innerDefs.map(_.id))
    val innerScope = new FlatTessla.Scope(Some(scope))
    val paramIdMap = createIdMap(parameters.map(_.name))
    val innerIdMap = idMap ++ paramIdMap ++ createIdMap(innerDefs.map(_.id.name))
    parameters.foreach { param =>
      innerScope.addVariable(paramIdMap(param.name), FlatTessla.VariableEntry(param, param.parameterType))
    }
    innerDefs.foreach { innerDef =>
      addDefinition(innerDef, innerScope, innerIdMap)
    }
    val body = translateExpression(exp, innerScope, innerIdMap)
    if (parameters.isEmpty) {
      scope.addVariable(idMap(definition.id.name), FlatTessla.VariableEntry(body, definition.returnType))
    } else {
      // TODO: Handle type parameters by implementing a proper environment for them (different namespace)
      val mac = FlatTessla.Macro(Seq(), parameters, innerScope, definition.returnType, body, definition.loc)
      scope.addVariable(idMap(definition.id.name), FlatTessla.VariableEntry(mac, None))
    }
  }

  val errorExpression = FlatTessla.Variable(makeIdentifier("<<error>>"), Location.unknown)

  def getExp(tesslaId: Tessla.Identifier, idMap: IdMap): FlatTessla.Variable = {
    idMap.get(tesslaId.name) match {
      case Some(id) =>
        FlatTessla.Variable(id, tesslaId.loc)
      case None =>
        error(UndefinedVariable(tesslaId))
        errorExpression
    }
  }

  def translateExpression(expr: Tessla.Expression, scope: FlatTessla.Scope, idMap: IdMap): FlatTessla.Expression = expr match {
    case variable: Tessla.Variable =>
      getExp(variable.id, idMap)

    case literal: Tessla.Literal =>
      FlatTessla.Literal(literal.value, literal.loc)

    case call: Tessla.MacroCall =>
      val args = call.args.map {
        case arg: Tessla.NamedArgument =>
          val id = makeIdentifier()
          scope.addVariable(id, FlatTessla.VariableEntry(translateExpression(arg.expr, scope, idMap), None))
          FlatTessla.NamedArgument(arg.id.name, id, arg.loc)
        case arg: Tessla.PositionalArgument =>
          val id = makeIdentifier()
          scope.addVariable(id, FlatTessla.VariableEntry(translateExpression(arg.expr, scope, idMap), None))
          FlatTessla.PositionalArgument(id, arg.loc)
      }
      FlatTessla.MacroCall(getExp(call.macroID, idMap).id, args, call.loc)

    case block: Tessla.Block =>
      val innerIdMap = idMap ++ createIdMap(block.definitions.map(_.id.name))
      block.definitions.foreach { definition =>
        addDefinition(definition, scope, innerIdMap)
      }
      val id = makeIdentifier()
      scope.addVariable(id, FlatTessla.VariableEntry(translateExpression(block.expression, scope, innerIdMap), None))
      FlatTessla.Variable(id, block.expression.loc)
  }
}

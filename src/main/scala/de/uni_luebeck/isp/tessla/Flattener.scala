package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{MultipleDefinitionsError, UndefinedVariable}
import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Flattener extends FlatTessla.IdentifierFactory with TranslationPhase[Tessla.Specification, FlatTessla.Specification] {
  type IdMap = Map[String, FlatTessla.Identifier]

  val stdlib = BuiltIn.builtIns.map {
    case (name, b) =>
      name -> FlatTessla.VariableEntry(makeIdentifier(name), FlatTessla.BuiltInOperator(b), None, Location.builtIn)
  } + ("nil" -> FlatTessla.VariableEntry(makeIdentifier("nil"), FlatTessla.Nil, None, Location.builtIn))

  def createIdMap(names: Iterable[String]): IdMap = {
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
      case (_, duplicates) if duplicates.lengthCompare(1) > 0 =>
        val firstLoc = duplicates.head.loc
        duplicates.tail.foreach { duplicate =>
          error(MultipleDefinitionsError(duplicate, firstLoc))
        }

      case _ => /* Do nothing */
    }
  }

  override def translateSpec(spec: Tessla.Specification) = {
    val stdlibScope = new FlatTessla.Scope(None)
    stdlib.foreach {
      case (_, entry) =>
        stdlibScope.addVariable(entry)
    }
    val globalScope = new FlatTessla.Scope(Some(stdlibScope))
    val globalIdMap = stdlib.mapValues(_.id) ++ createIdMap(spec.statements.flatMap(getName))
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
        val id = expToId(translateExpression(out.expr, globalScope, globalIdMap), globalScope)
        val newOut = FlatTessla.OutStream(id, out.name, out.loc)
        result.copy(outStreams = result.outStreams :+ newOut)

      case (result, definition: Tessla.Definition) =>
        addDefinition(definition, globalScope, globalIdMap)
        result

      case (result, in: Tessla.In) =>
        val inputStream = FlatTessla.InputStream(in.id.name, in.streamType, in.loc)
        val entry = FlatTessla.VariableEntry(globalIdMap(in.id.name), inputStream, Some(in.streamType), in.loc)
        globalScope.addVariable(entry)
        result
    }
  }

  def addDefinition(definition: Tessla.Definition, scope: FlatTessla.Scope, idMap: IdMap): Unit = {
    val parameters = definition.parameters.map(p => FlatTessla.Parameter(p, makeIdentifier(p.id.name)))
    val (innerDefs, exp) = definition.body match {
      case block: Tessla.Block => (block.definitions, block.expression)
      case e => (Seq(), e)
    }
    checkForDuplicates(parameters.map(_.nameWithLoc) ++ innerDefs.map(_.id))
    if (parameters.isEmpty) {
      // For parameterless definitions, inner definitions become part of the global scope in flat tessla.
      // Flat tessla only has distinct scope for macros with parameters (as those need to be instantiated
      // multiple times
      val innerIdMap = idMap ++ createIdMap(innerDefs.map(_.id.name))
      innerDefs.foreach { innerDef =>
        addDefinition(innerDef, scope, innerIdMap)
      }
      val body = translateExpression(exp, scope, innerIdMap)
      scope.addVariable(FlatTessla.VariableEntry(idMap(definition.id.name), body, definition.returnType, definition.loc))
    } else {
      val innerScope = new FlatTessla.Scope(Some(scope))
      val paramIdMap = createIdMap(parameters.map(_.name))
      val innerIdMap = idMap ++ paramIdMap ++ createIdMap(innerDefs.map(_.id.name))
      parameters.foreach { param =>
        innerScope.addVariable(FlatTessla.VariableEntry(paramIdMap(param.name), param, param.parameterType, param.loc))
      }
      innerDefs.foreach { innerDef =>
        addDefinition(innerDef, innerScope, innerIdMap)
      }
      val body = translateExpression(exp, innerScope, innerIdMap)
      // TODO: Handle type parameters by implementing a proper environment for them (different namespace)
      val mac = FlatTessla.Macro(Seq(), parameters, innerScope, definition.returnType, body, definition.loc)
      scope.addVariable(FlatTessla.VariableEntry(idMap(definition.id.name), mac, None, definition.loc))
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

  /**
    * Turn an arbitrary expression into an identifier by either creating a new identifier and making it point to the
    * expression or, if the expression is a variable reference, returning the referenced identifier.
    */
  def expToId(exp: FlatTessla.Expression, scope: FlatTessla.Scope) = exp match {
    case v: FlatTessla.Variable => v.id
    case _ =>
      val id = makeIdentifier()
      scope.addVariable(FlatTessla.VariableEntry(id, exp, None, exp.loc))
      id
  }

  def translateExpression(expr: Tessla.Expression, scope: FlatTessla.Scope, idMap: IdMap): FlatTessla.Expression = expr match {
    case variable: Tessla.Variable =>
      getExp(variable.id, idMap)

    case literal: Tessla.Literal =>
      FlatTessla.Literal(literal.value, literal.loc)

    case call: Tessla.MacroCall =>
      val args = call.args.map {
        case arg: Tessla.NamedArgument =>
          val id = expToId(translateExpression(arg.expr, scope, idMap), scope)
          FlatTessla.NamedArgument(arg.id.name, id, arg.loc)
        case arg: Tessla.PositionalArgument =>
          val id = expToId(translateExpression(arg.expr, scope, idMap), scope)
          FlatTessla.PositionalArgument(id, arg.loc)
      }
      val mac = getExp(call.macroID, idMap)
      FlatTessla.MacroCall(mac.id, mac.loc, args, call.loc)

    case block: Tessla.Block =>
      val innerIdMap = idMap ++ createIdMap(block.definitions.map(_.id.name))
      block.definitions.foreach { definition =>
        addDefinition(definition, scope, innerIdMap)
      }
      val id = expToId(translateExpression(block.expression, scope, innerIdMap), scope)
      FlatTessla.Variable(id, block.expression.loc)
  }
}

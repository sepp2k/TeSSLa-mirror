package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{MultipleDefinitionsError, UndefinedVariable}
import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Flattener extends FlatTessla.IdentifierFactory with TranslationPhase[Tessla.Specification, FlatTessla.Specification] {
  type IdMap = Map[String, FlatTessla.Identifier]

  val primitiveOperators = Seq(
    "+" -> PrimitiveOperators.Add,
    "-" -> PrimitiveOperators.Sub,
    "unary -" -> PrimitiveOperators.Negate,
    "*" -> PrimitiveOperators.Mul,
    "/" -> PrimitiveOperators.Div,
    "&" -> PrimitiveOperators.BitAnd,
    "|" -> PrimitiveOperators.BitOr,
    "^" -> PrimitiveOperators.BitXor,
    "<<" -> PrimitiveOperators.LeftShift,
    ">>" -> PrimitiveOperators.RightShift,
    "~" -> PrimitiveOperators.BitFlip,
    "<" -> PrimitiveOperators.Lt,
    ">" -> PrimitiveOperators.Gt,
    "<=" -> PrimitiveOperators.Lte,
    ">=" -> PrimitiveOperators.Gte,
    "==" -> PrimitiveOperators.Eq,
    "!=" -> PrimitiveOperators.Neq,
    "&&" -> PrimitiveOperators.And,
    "||" -> PrimitiveOperators.Or,
    "!" -> PrimitiveOperators.Not,
    "first" -> PrimitiveOperators.First,
    "if then else" -> PrimitiveOperators.IfThenElse,
    "if then" -> PrimitiveOperators.IfThen,
    "map_empty" -> PrimitiveOperators.MapEmpty,
    "map_add" -> PrimitiveOperators.MapAdd,
    "map_get" -> PrimitiveOperators.MapGet,
    "map_contains" -> PrimitiveOperators.MapContains,
    "map_remove" -> PrimitiveOperators.MapRemove,
    "set_empty" -> PrimitiveOperators.SetEmpty,
    "set_add" -> PrimitiveOperators.SetAdd,
    "set_contains" -> PrimitiveOperators.SetContains,
    "set_remove" -> PrimitiveOperators.SetRemove
  )

  val builtIns = Seq(
    "default" -> FlatTessla.Default,
    "defaultFrom" -> FlatTessla.DefaultFrom,
    "last" -> FlatTessla.Last,
    "delayedLast" -> FlatTessla.DelayedLast,
    "time" -> FlatTessla.Time,
    "const" -> FlatTessla.Const,
    "merge" -> FlatTessla.Merge
  ) ++ primitiveOperators.map {
    case (name, primOp) => name -> FlatTessla.PrimitiveOperator(primOp)
  }

  val stdlib = ("nil" -> FlatTessla.VariableEntry(FlatTessla.Nil, None)) +: builtIns.map {
    case (name, b) => name -> FlatTessla.VariableEntry(FlatTessla.BuiltInOperator(b), None)
  }

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
        val id = expToId(translateExpression(out.expr, globalScope, globalIdMap), globalScope)
        val newOut = FlatTessla.OutStream(id, out.name, out.loc)
        result.copy(outStreams = result.outStreams :+ newOut)

      case (result, definition: Tessla.Definition) =>
        addDefinition(definition, globalScope, globalIdMap)
        result

      case (result, in: Tessla.In) =>
        val inputStream = FlatTessla.InputStream(in.id.name, in.streamType, in.loc)
        val entry = FlatTessla.VariableEntry(inputStream, Some(in.streamType))
        globalScope.addVariable(globalIdMap(in.id.name), entry)
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
      scope.addVariable(idMap(definition.id.name), FlatTessla.VariableEntry(body, definition.returnType))
    } else {
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

  /**
    * Turn an arbitrary expression into an identifier by either creating a new identifier and making it point to the
    * expression or, if the expression is a variable reference, returning the referenced identifier.
    */
  def expToId(exp: FlatTessla.Expression, scope: FlatTessla.Scope) = exp match {
    case v: FlatTessla.Variable => v.id
    case _ =>
      val id = makeIdentifier()
      scope.addVariable(id, FlatTessla.VariableEntry(exp, None))
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

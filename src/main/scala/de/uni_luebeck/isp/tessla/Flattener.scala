package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Flattener extends FlatTessla.IdentifierFactory with TranslationPhase[Tessla.Specification, FlatTessla.Specification] {
  type IdMap = Map[String, FlatTessla.Identifier]

  case class Env(variables: IdMap, types: IdMap) {
    def ++(other: Env) = Env(variables ++ other.variables, types ++ other.types)
  }

  val stdlib = BuiltIn.builtIns.map {
    case (name, b) =>
      name -> FlatTessla.VariableEntry(makeIdentifier(name), FlatTessla.BuiltInOperator(b), None, Location.builtIn)
  }

  val eventsType = FlatTessla.TypeEntry(
    makeIdentifier("Events"), 1, { case Seq(t) => FlatTessla.StreamType(t)}, Location.builtIn
  )

  val mapType = FlatTessla.TypeEntry(
    makeIdentifier("Map"), 2, { case Seq(k, v) => FlatTessla.MapType(k, v)}, Location.builtIn
  )

  val setType = FlatTessla.TypeEntry(
    makeIdentifier("Set"), 1, { case Seq(t) => FlatTessla.SetType(t)}, Location.builtIn
  )

  val builtInTypes: Map[String, FlatTessla.TypeEntry] = Map(
    "Int" -> FlatTessla.IntType,
    "String" -> FlatTessla.StringType,
    "Unit" -> FlatTessla.UnitType,
    "Bool" -> FlatTessla.BoolType
  ).map {
    case (name, t) =>
      name -> FlatTessla.TypeEntry(makeIdentifier(name), 0, _ => t, Location.builtIn)
  } ++ Map("Events" -> eventsType, "Map" -> mapType, "Set" -> setType)

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
    stdlib.values.foreach { entry =>
      stdlibScope.addVariable(entry)
    }
    builtInTypes.values.foreach { entry =>
      stdlibScope.addType(entry)
    }
    val globalScope = new FlatTessla.Scope(Some(stdlibScope))
    val globalVariableIdMap = stdlib.mapValues(_.id) ++ createIdMap(spec.statements.flatMap(getName))
    // TODO: Once we have a way to define global types, those need to be handled here
    val globalTypeIdMap = builtInTypes.mapValues(_.id)
    val globalEnv = Env(globalVariableIdMap, globalTypeIdMap)
    val stdlibNames = stdlib.mapValues(_.id)
    val emptySpec = FlatTessla.Specification(globalScope, Seq(), outAllLocation = None, stdlibNames)
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
        val id = expToId(translateExpression(out.expr, globalScope, globalEnv), globalScope)
        val newOut = FlatTessla.OutStream(id, out.name, out.loc)
        result.copy(outStreams = result.outStreams :+ newOut)

      case (result, definition: Tessla.Definition) =>
        addDefinition(definition, globalScope, globalEnv)
        result

      case (result, in: Tessla.In) =>
        val streamType = translateType(in.streamType, globalScope, globalEnv)
        val inputStream = FlatTessla.InputStream(in.id.name, streamType, in.loc)
        val entry = FlatTessla.VariableEntry(globalEnv.variables(in.id.name), inputStream, Some(streamType), in.loc)
        globalScope.addVariable(entry)
        result
    }
  }

  def addDefinition(definition: Tessla.Definition, scope: FlatTessla.Scope, env: Env): Unit = {
    val (innerDefs, exp) = definition.body match {
      case block: Tessla.Block => (block.definitions, block.expression)
      case e => (Seq(), e)
    }
    checkForDuplicates(definition.parameters.map(_.id) ++ innerDefs.map(_.id))
    checkForDuplicates(definition.typeParameters)
    if (definition.parameters.isEmpty && definition.typeParameters.isEmpty) {
      // For parameterless definitions, inner definitions become part of the global scope in flat tessla.
      // Flat tessla only has distinct scope for macros with parameters (as those need to be instantiated
      // multiple times)
      val innerEnv = env ++ Env(variables = createIdMap(innerDefs.map(_.id.name)), types = Map())
      innerDefs.foreach { innerDef =>
        addDefinition(innerDef, scope, innerEnv)
      }
      val body = translateExpression(exp, scope, innerEnv)
      val typ = definition.returnType.map(translateType(_, scope, innerEnv))
      scope.addVariable(FlatTessla.VariableEntry(env.variables(definition.id.name), body, typ, definition.loc))
    } else {
      val innerScope = new FlatTessla.Scope(Some(scope))
      val paramIdMap = createIdMap(definition.parameters.map(_.id.name))
      val typeParamIdMap = createIdMap(definition.typeParameters.map(_.name))
      // Environment that contains the macro's parameters and type parameters, but not any of its inner definitions
      // This is used to process the type signature as we want to be able to use type arguments there,
      // but not any type definitions that are inside the macro (once we have those)
      val paramEnv = env ++ Env(variables = paramIdMap, types = typeParamIdMap)
      val innerEnv = paramEnv ++ Env(variables = createIdMap(innerDefs.map(_.id.name)), types = Map())
      definition.typeParameters.foreach { typeParameter =>
        val tp = FlatTessla.TypeParameter(typeParamIdMap(typeParameter.name), typeParameter.loc)
        innerScope.addType(FlatTessla.TypeEntry(tp.id, 0, _ => tp, tp.loc))
      }
      val parameters = definition.parameters.map { param =>
        val typ = param.parameterType.map(translateType(_, innerScope, paramEnv))
        FlatTessla.Parameter(param, typ, paramIdMap(param.id.name))
      }
      parameters.foreach { param =>
        val typ = param.parameterType
        innerScope.addVariable(FlatTessla.VariableEntry(paramIdMap(param.name), param, typ, param.loc))
      }
      innerDefs.foreach { innerDef =>
        addDefinition(innerDef, innerScope, innerEnv)
      }
      val body = translateExpression(exp, innerScope, innerEnv)
      // Get the values of the type map in the order in which they appeared in the type parameter list
      val typeParameters = definition.typeParameters.map(tp => paramEnv.types(tp.name))
      val returnType = definition.returnType.map(translateType(_, innerScope, paramEnv))
      val mac = FlatTessla.Macro(typeParameters, parameters, innerScope, returnType, body, definition.loc)
      scope.addVariable(FlatTessla.VariableEntry(env.variables(definition.id.name), mac, None, definition.loc))
    }
  }

  val errorExpression = FlatTessla.Variable(makeIdentifier("<<error>>"), Location.unknown)

  def getExp(tesslaId: Tessla.Identifier, env: Env): FlatTessla.Variable = {
    env.variables.get(tesslaId.name) match {
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

  def translateType(typ: Tessla.Type, scope: FlatTessla.Scope, env: Env): FlatTessla.Type = typ match {
    case Tessla.SimpleType(id) =>
      val typeEntry = env.types.get(id.name).flatMap(scope.resolveType).getOrElse(throw UndefinedType(id.name, id.loc))
      if (typeEntry.arity == 0) typeEntry.typeConstructor(Seq())
      else throw TypeArityMismatch(id.name, typeEntry.arity, 0, id.loc)

    case Tessla.TypeApplication(id, typeArgs, loc) =>
      val translatedArgs = typeArgs.map(translateType(_, scope, env))
      val typeEntry = scope.resolveType(env.types(id.name)).getOrElse(throw UndefinedType(id.name, id.loc))
      if (typeEntry.arity == translatedArgs.length) typeEntry.typeConstructor(translatedArgs)
      else throw TypeArityMismatch(id.name, typeEntry.arity, translatedArgs.length, id.loc)

    case Tessla.FunctionType(parameterTypes, returnType, loc) =>
      FlatTessla.FunctionType(
        // Explicitly written function types never have any type arguments because we don't support higher rank types
        Seq(),
        parameterTypes.map(translateType(_, scope, env)),
        translateType(returnType, scope, env)
      )
  }

  def translateExpression(expr: Tessla.Expression, scope: FlatTessla.Scope, env: Env): FlatTessla.Expression = expr match {
    case variable: Tessla.Variable =>
      getExp(variable.id, env)

    case literal: Tessla.Literal =>
      FlatTessla.Literal(literal.value, literal.loc)

    case call: Tessla.MacroCall =>
      val args = call.args.map {
        case arg: Tessla.NamedArgument =>
          val id = expToId(translateExpression(arg.expr, scope, env), scope)
          FlatTessla.NamedArgument(arg.id.name, id, arg.loc)
        case arg: Tessla.PositionalArgument =>
          val id = expToId(translateExpression(arg.expr, scope, env), scope)
          FlatTessla.PositionalArgument(id, arg.loc)
      }
      val mac = getExp(call.macroID, env)
      FlatTessla.MacroCall(mac.id, mac.loc, call.typeArgs.map(translateType(_, scope, env)), args, call.loc)

    case block: Tessla.Block =>
      val innerEnv = env ++ Env(variables = createIdMap(block.definitions.map(_.id.name)), types = Map())
      block.definitions.foreach { definition =>
        addDefinition(definition, scope, innerEnv)
      }
      val id = expToId(translateExpression(block.expression, scope, innerEnv), scope)
      FlatTessla.Variable(id, block.expression.loc)
  }
}

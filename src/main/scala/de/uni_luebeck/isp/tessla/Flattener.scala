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
    "Bool" -> FlatTessla.BoolType,
    "CTF" -> FlatTessla.CtfType
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

  def checkForDuplicates(identifiers: Seq[Tessla.Identifier]): Unit = {
    identifiers.groupBy(_.name).foreach {
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
    val globalTypeIdMap = builtInTypes.mapValues(_.id) ++ createIdMap(spec.statements.collect {
      case typeDef: Tessla.TypeDefinition => typeDef.id.name
    })
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

      case (result, typeDef: Tessla.TypeDefinition) =>
        addTypeDefinition(typeDef, globalScope, globalEnv)
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
    definition.annotations.foreach {annotation =>
      if (annotation.name != "liftable") {
        error(UndefinedAnnotation(annotation))
      }
    }
    val liftableAnnotation = definition.annotations.find(_.name == "liftable")
    if (definition.parameters.isEmpty && definition.typeParameters.isEmpty) {
      liftableAnnotation.foreach { annotationId =>
        throw MacroAnnotationOnNonMacro(annotationId, definition.id.name)
      }
      // For parameterless definitions, inner definitions become part of the global scope in flat tessla.
      // Flat tessla only has distinct scope for macros with parameters (as those need to be instantiated
      // multiple times)
      val innerEnv = env ++ Env(variables = createIdMap(innerDefs.map(_.id.name)), types = Map())
      innerDefs.foreach { innerDef =>
        addDefinition(innerDef, scope, innerEnv)
      }
      val body = translateExpression(exp, scope, innerEnv)
      val typ = definition.returnType.map(translateType(_, scope, innerEnv))
      scope.addVariable(FlatTessla.VariableEntry(env.variables(definition.id.name), body, typ, definition.headerLoc))
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
        val typ = param.parameterType match {
          case Some(t) =>
            translateType(t, innerScope, paramEnv)
          case None =>
            error(MissingTypeAnnotationParam(param.id.name, param.id.loc))
            // Since we call error here, the compilation will abort after this phase and the result will never be used.
            // Therefore it's okay to insert a null here (once we extend the system to keep compiling after errors, the
            // null should be replaced by a proper error node)
            null
        }
        FlatTessla.Parameter(param, typ, paramIdMap(param.id.name))
      }
      parameters.foreach { param =>
        val typ = param.parameterType
        innerScope.addVariable(FlatTessla.VariableEntry(paramIdMap(param.name), param, Some(typ), param.loc))
      }
      innerDefs.foreach { innerDef =>
        addDefinition(innerDef, innerScope, innerEnv)
      }
      val body = translateExpression(exp, innerScope, innerEnv)
      // Get the values of the type map in the order in which they appeared in the type parameter list
      val typeParameters = definition.typeParameters.map(tp => paramEnv.types(tp.name))
      val returnType = definition.returnType.map(translateType(_, innerScope, paramEnv))
      val mac = FlatTessla.Macro(
        typeParameters, parameters, innerScope, returnType, definition.headerLoc, body, definition.loc,
        isLiftable = liftableAnnotation.isDefined
      )
      scope.addVariable(FlatTessla.VariableEntry(env.variables(definition.id.name), mac, None, definition.headerLoc))
    }
  }

  def addTypeDefinition(definition: Tessla.TypeDefinition, scope: FlatTessla.Scope, env: Env): Unit = {
    checkForDuplicates(definition.typeParameters)
    def constructType(typeArgs: Seq[FlatTessla.Type]) = {
      val innerScope = new FlatTessla.Scope(Some(scope))
      val typeParamIdMap = createIdMap(definition.typeParameters.map(_.name))
      val innerEnv = env ++ Env(variables = Map(), types = typeParamIdMap)
      definition.typeParameters.zip(typeArgs).foreach {
        case (typeParameter, typeArg) =>
          val id = typeParamIdMap(typeParameter.name)
          innerScope.addType(FlatTessla.TypeEntry(id, 0, _ => typeArg, typeParameter.loc))
      }
      translateType(definition.body, innerScope, innerEnv)
    }
    scope.addType(FlatTessla.TypeEntry(env.types(definition.id.name), definition.typeParameters.length, constructType, definition.loc))
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

    case Tessla.TypeApplication(id, typeArgs, _) =>
      val translatedArgs = typeArgs.map(translateType(_, scope, env))
      val typeEntry = scope.resolveType(env.types.getOrElse(id.name, throw UndefinedType(id.name, id.loc))).getOrElse(throw UndefinedType(id.name, id.loc))
      if (typeEntry.arity == translatedArgs.length) typeEntry.typeConstructor(translatedArgs)
      else throw TypeArityMismatch(id.name, typeEntry.arity, translatedArgs.length, id.loc)

    case Tessla.FunctionType(parameterTypes, returnType, _) =>
      FlatTessla.FunctionType(
        // Explicitly written function types never have any type arguments because we don't support higher rank types
        Seq(),
        parameterTypes.map(translateType(_, scope, env)),
        translateType(returnType, scope, env),
        // Explicitly written function types are never liftable because we have no syntax for that
        isLiftable = false
      )

    case ot: Tessla.ObjectType =>
      checkForDuplicates(ot.memberTypes.map(_._1))
      val memberTypes = ot.memberTypes.map {
        case (id, t) =>
          id.name -> translateType(t, scope, env)
      }
      FlatTessla.ObjectType(memberTypes.toMap)

    case tt: Tessla.TupleType =>
      val memberTypes = tt.elementTypes.zipWithIndex.map {
        case (t, idx) =>
          s"_${idx+1}" -> translateType(t, scope, env)
      }
      FlatTessla.ObjectType(memberTypes.toMap)
  }

  def translateExpression(expr: Tessla.Expression, scope: FlatTessla.Scope, env: Env): FlatTessla.Expression = expr match {
    case variable: Tessla.Variable =>
      getExp(variable.id, env)

    case literal: Tessla.Literal =>
      FlatTessla.Literal(literal.value, literal.loc)

    case call: Tessla.MacroCall =>
      val mac = expToId(translateExpression(call.mac, scope, env), scope)
      val args = call.args.map {
        case arg: Tessla.NamedArgument =>
          val id = expToId(translateExpression(arg.expr, scope, env), scope)
          FlatTessla.NamedArgument(arg.id.name, id, arg.loc)
        case arg: Tessla.PositionalArgument =>
          val id = expToId(translateExpression(arg.expr, scope, env), scope)
          FlatTessla.PositionalArgument(id, arg.loc)
      }
      FlatTessla.MacroCall(mac, call.mac.loc, call.typeArgs.map(translateType(_, scope, env)), args, call.loc)

    case ite: Tessla.StaticIfThenElse =>
      FlatTessla.StaticIfThenElse(
        FlatTessla.IdLoc(expToId(translateExpression(ite.condition, scope, env), scope), ite.condition.loc),
        FlatTessla.IdLoc(expToId(translateExpression(ite.thenCase, scope, env), scope), ite.thenCase.loc),
        FlatTessla.IdLoc(expToId(translateExpression(ite.elseCase, scope, env), scope), ite.elseCase.loc),
        ite.loc)

    case block: Tessla.Block =>
      val innerEnv = env ++ Env(variables = createIdMap(block.definitions.map(_.id.name)), types = Map())
      block.definitions.foreach { definition =>
        addDefinition(definition, scope, innerEnv)
      }
      val id = expToId(translateExpression(block.expression, scope, innerEnv), scope)
      FlatTessla.Variable(id, block.expression.loc)

    case objectLit: Tessla.ObjectLiteral =>
      checkForDuplicates(objectLit.members.map(_.id))
      val innerEnv = env ++ Env(variables = createIdMap(objectLit.members.map(_.id.name)), types = Map())
      val members = objectLit.members.map {
        case Tessla.MemberDefinition.Full(definition) =>
          addDefinition(definition, scope, innerEnv)
          definition.id.name -> FlatTessla.IdLoc(innerEnv.variables(definition.id.name), definition.body.loc)
        case Tessla.MemberDefinition.Simple(id) =>
          // Use env instead of innerEnv here because for `${x, y}` we specifically want to look up the values
          // of x and y in the scope outside of the object, not inside the object, which would just lead to infinite
          // recursion.
          val body = translateExpression(Tessla.Variable(id), scope, env)
          scope.addVariable(FlatTessla.VariableEntry(innerEnv.variables(id.name), body, None, id.loc))
          id.name -> FlatTessla.IdLoc(innerEnv.variables(id.name), id.loc)
      }
      FlatTessla.ObjectLiteral(members.toMap, objectLit.loc)

    case tuple: Tessla.Tuple =>
      val members = tuple.elements.zipWithIndex.map {
        case (element, index) =>
          s"_${index+1}" -> FlatTessla.IdLoc(expToId(translateExpression(element, scope, env), scope), element.loc)
      }
      FlatTessla.ObjectLiteral(members.toMap, tuple.loc)

    case memberAccess: Tessla.MemberAccess =>
      val receiverId = expToId(translateExpression(memberAccess.receiver, scope, env), scope)
      val receiver = FlatTessla.IdLoc(receiverId, memberAccess.receiver.loc)
      FlatTessla.MemberAccess(receiver, memberAccess.member.name, memberAccess.member.loc, memberAccess.loc)
  }
}
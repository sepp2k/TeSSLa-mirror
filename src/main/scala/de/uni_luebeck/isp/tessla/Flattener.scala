package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Flattener(spec: Tessla.Specification)
  extends FlatTessla.IdentifierFactory with TranslationPhase.Translator[FlatTessla.Specification] {
  type IdMap = Map[String, FlatTessla.Identifier]

  case class Env(variables: IdMap, types: IdMap) {
    def ++(other: Env) = Env(variables ++ other.variables, types ++ other.types)
  }

  def createIdMap(names: Iterable[String]): IdMap = {
    names.map(name => name -> makeIdentifier(name)).toMap
  }

  def getId(statement: Tessla.Statement): Option[Tessla.Identifier] = statement match {
    case definition: Tessla.Definition => Some(definition.id)
    case in: Tessla.In => Some(in.id)
    case module: Tessla.Module => Some(module.id)
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

  val annotationDefs = spec.statements.collect {
    case annotationDef: Tessla.AnnotationDefinition =>
      annotationDef.parameters.foreach { param =>
        if (param.parameterType.isEmpty) {
          error(MissingTypeAnnotationParam(param.name, param.loc))
        }
      }
      annotationDef.id.name -> annotationDef
  }.toMap

  override def translateSpec() = {
    val globalDefs = new FlatTessla.Definitions(None)
    val globalVariableIdMap = createIdMap(spec.statements.flatMap(getName))
    val globalTypeIdMap = createIdMap(spec.statements.collect {
      case typeDef: Tessla.TypeDefinition => typeDef.id.name
    })
    val globalEnv = Env(globalVariableIdMap, globalTypeIdMap)
    val emptySpec = FlatTessla.Specification(globalDefs, Seq(), outAllLocation = None, globalVariableIdMap)
    checkForDuplicates(spec.statements.flatMap(getId))
    // Process type definitions before anything else, so that types can be used before they're defined
    spec.statements.foreach {
      case typeDef: Tessla.TypeDefinition =>
        addTypeDefinition(typeDef, globalDefs, globalEnv)
      case _ => // do nothing
    }
    spec.statements.foldLeft(emptySpec) {
      case (result, outAll : Tessla.OutAll) =>
        if(result.outAll) warn(ConflictingOut(outAll.loc, previous = result.outAllLocation.get))
        result.copy(outAllLocation = Some(outAll.loc))

      case (result, out: Tessla.Out) =>
        result.outStreams.find(_.nameOpt.contains(out.name)).foreach {
          previous =>
            warn(ConflictingOut(out.loc, previous = previous.loc))
        }
        val id = expToId(translateExpression(out.expr, globalDefs, globalEnv), globalDefs)
        val newOut = FlatTessla.OutStream(id, Some(out.name), out.loc)
        result.copy(outStreams = result.outStreams :+ newOut)

      case (result, print: Tessla.Print) =>
        val id = expToId(translateExpression(print.expr, globalDefs, globalEnv), globalDefs)
        val newPrint = FlatTessla.OutStream(id, None, print.loc)
        result.copy(outStreams = result.outStreams :+ newPrint)

      case (result, definition: Tessla.Definition) =>
        addDefinition(definition, globalDefs, globalEnv)
        result

      case (result, _: Tessla.TypeDefinition | _: Tessla.AnnotationDefinition) =>
        result

      case (result, in: Tessla.In) =>
        addInStream(in, globalDefs, globalEnv)
        result

      case (result, module: Tessla.Module) =>
        addModule(module, globalDefs, globalEnv)
        result
    }
  }

  def addInStream(in: Tessla.In, defs: FlatTessla.Definitions, env: Env): Unit = {
    val streamType = translateType(in.streamType, defs, env)
    val inputStream = FlatTessla.InputStream(in.id.name, streamType, in.streamType.loc, in.loc)
    val annotations = in.annotations.map(translateAnnotation)
    val entry = FlatTessla.VariableEntry(env.variables(in.id.name), inputStream, Some(streamType), annotations, in.loc)
    defs.addVariable(entry)
  }

  def addModule(module: Tessla.Module, defs: FlatTessla.Definitions, outerEnv: Env): Unit = {
    val variableIdMap = createIdMap(module.contents.flatMap(getName))
    val typeIdMap = createIdMap(module.contents.collect {
      case typeDef: Tessla.TypeDefinition => typeDef.id.name
    })
    val env = outerEnv ++ Env(variableIdMap, typeIdMap)
    // Process type definitions before anything else, so that types can be used before they're defined
    module.contents.foreach {
      case typeDef: Tessla.TypeDefinition =>
        addTypeDefinition(typeDef, defs, env)
      case _ => // do nothing
    }
    // TODO: Handle annotation definitions in modules
    module.contents.foreach {
      case definition: Tessla.Definition =>
        addDefinition(definition, defs, env)

      case module: Tessla.Module =>
        addModule(module, defs, env)

      case inout@(_: Tessla.Out | _: Tessla.OutAll | _: Tessla.Print | _: Tessla.In) =>
        error(InOutStatementInModule(inout.loc))

      case annoDef: Tessla.AnnotationDefinition =>
        error(AnnotationDefInModule(annoDef.loc))

      case _: Tessla.TypeDefinition => // Do nothing because types have already been handled
    }
    val valueMembers = module.contents.flatMap { stat =>
      getName(stat).map { name =>
        name -> FlatTessla.IdLoc(variableIdMap(name), stat.loc)
      }
    }.toMap
    val obj = FlatTessla.ObjectLiteral(valueMembers, module.loc)
    defs.addVariable(FlatTessla.VariableEntry(outerEnv.variables(module.name), obj, None, Seq(), module.loc))
  }

  def translateParameter(param: Tessla.Parameter, idMap: IdMap, defs: FlatTessla.Definitions, env: Env): FlatTessla.Parameter = {
    val typ = param.parameterType match {
      case Some(t) =>
        translateType(t, defs, env)
      case None =>
        error(MissingTypeAnnotationParam(param.id.name, param.id.loc))
        // Since we call error here, the compilation will abort after this phase and the result will never be used.
        // Therefore it's okay to insert a null here (once we extend the system to keep compiling after errors, the
        // null should be replaced by a proper error node)
        null
    }
    FlatTessla.Parameter(param, typ, idMap(param.id.name))
  }

  def translateAnnotation(annotation: Tessla.Annotation) = {
    annotationDefs.get(annotation.name) match {
      case Some(annotationDef) =>
        if (annotationDef.parameters.size != annotation.arguments.size) {
          error(ArityMismatch(
            name = annotation.name,
            expected = annotationDef.parameters.size,
            actual = annotation.arguments.size,
            loc = annotation.loc
          ))
          FlatTessla.Annotation(annotation.name, Map(), annotation.loc)
        } else {
          var posArgIdx = 0
          val args: Map[String, Tessla.LiteralValue] = annotation.arguments.map {
            case arg: Tessla.PositionalArgument[Tessla.Literal] =>
              val param = annotationDef.parameters(posArgIdx)
              posArgIdx += 1
              param.name -> arg.expr.value
            case arg: Tessla.NamedArgument[Tessla.Literal] =>
              annotationDef.parameters.find(_.name == arg.name) match {
                case Some(param) => param.name -> arg.expr.value
                case None => throw UndefinedNamedArg(arg.name, arg.loc)
              }
          }.toMap
          FlatTessla.Annotation(annotation.name, args, annotation.loc)
        }
      case None =>
        error(UndefinedAnnotation(annotation.id))
        FlatTessla.Annotation(annotation.name, Map(), annotation.loc)
    }
  }

  def addDefinition(definition: Tessla.Definition, defs: FlatTessla.Definitions, env: Env): Unit = {
    val (blockDefs, defBody) = definition.body match {
      case Tessla.ExpressionBody(block: Tessla.Block) => (block.definitions, Tessla.ExpressionBody(block.expression))
      case b => (Seq(), b)
    }
    checkForDuplicates(definition.parameters.map(_.id) ++ blockDefs.map(_.id))
    checkForDuplicates(definition.typeParameters)
    val annotations = definition.annotations.map(translateAnnotation)
    val liftableAnnotation = definition.annotations.find(_.name == "liftable")
    if (definition.parameters.isEmpty && definition.typeParameters.isEmpty) {
      liftableAnnotation.foreach { annotation =>
        throw MacroAnnotationOnNonMacro(annotation.id, definition.id.name)
      }
      // For parameterless definitions, inner definitions become part of the global defs in flat tessla.
      // Flat tessla only has distinct defs for macros with parameters (as those need to be instantiated
      // multiple times)
      val innerEnv = env ++ Env(variables = createIdMap(blockDefs.map(_.id.name)), types = Map())
      blockDefs.foreach { blockDef =>
        addDefinition(blockDef, defs, innerEnv)
      }
      val exp = defBody match {
        case b: Tessla.ExpressionBody => translateExpression(b.exp, defs, innerEnv)
        case b: Tessla.BuiltInBody => FlatTessla.BuiltInOperator(b.id.name, b.id.loc)
      }
      val typ = definition.returnType.map(translateType(_, defs, innerEnv))
      defs.addVariable(FlatTessla.VariableEntry(env.variables(definition.id.name), exp, typ, annotations, definition.headerLoc))
    } else {
      val innerDefs = new FlatTessla.Definitions(Some(defs))
      val paramIdMap = createIdMap(definition.parameters.map(_.id.name))
      val typeParamIdMap = createIdMap(definition.typeParameters.map(_.name))
      // Environment that contains the macro's parameters and type parameters, but not any of its inner definitions
      // This is used to process the type signature as we want to be able to use type arguments there,
      // but not any type definitions that are inside the macro (once we have those)
      val paramEnv = env ++ Env(variables = paramIdMap, types = typeParamIdMap)
      val innerEnv = paramEnv ++ Env(variables = createIdMap(blockDefs.map(_.id.name)), types = Map())
      definition.typeParameters.foreach { typeParameter =>
        val tp = FlatTessla.TypeParameter(typeParamIdMap(typeParameter.name), typeParameter.loc)
        innerDefs.addType(FlatTessla.TypeEntry(tp.id, 0, _ => tp, tp.loc))
      }
      val parameters = definition.parameters.map(translateParameter(_, paramIdMap, innerDefs, paramEnv))
      parameters.foreach { param =>
        val typ = param.parameterType
        innerDefs.addVariable(FlatTessla.VariableEntry(paramIdMap(param.name), param, Some(typ), Seq(), param.loc))
      }
      blockDefs.foreach { innerDef =>
        addDefinition(innerDef, innerDefs, innerEnv)
      }
      // Get the values of the type map in the order in which they appeared in the type parameter list
      val typeParameters = definition.typeParameters.map(tp => paramEnv.types(tp.name))
      val returnTypeOpt = definition.returnType.map(translateType(_, innerDefs, paramEnv))
      defBody match {
        case b: Tessla.ExpressionBody =>
          val exp = translateExpression(b.exp, innerDefs, innerEnv)
          val mac = FlatTessla.Macro(
            typeParameters, parameters, innerDefs, returnTypeOpt, definition.headerLoc, exp, definition.loc,
            isLiftable = liftableAnnotation.isDefined
          )
          val typ = returnTypeOpt.map { returnType =>
            FlatTessla.FunctionType(typeParameters, parameters.map(_.parameterType), returnType, mac.isLiftable)
          }
          defs.addVariable(FlatTessla.VariableEntry(env.variables(definition.id.name), mac, typ, annotations, definition.headerLoc))
        case b: Tessla.BuiltInBody =>
          val typ = returnTypeOpt.map { returnType =>
            FlatTessla.FunctionType(typeParameters, parameters.map(_.parameterType), returnType, liftableAnnotation.isDefined)
          }
          val builtIn = FlatTessla.BuiltInOperator(b.id.name, b.id.loc)
          defs.addVariable(FlatTessla.VariableEntry(env.variables(definition.id.name), builtIn, typ, annotations, definition.headerLoc))
      }
    }
  }

  def addTypeDefinition(definition: Tessla.TypeDefinition, defs: FlatTessla.Definitions, env: Env): Unit = {
    checkForDuplicates(definition.typeParameters)
    def constructType(typeArgs: Seq[FlatTessla.Type]) = {
      val innerDefs = new FlatTessla.Definitions(Some(defs))
      val typeParamIdMap = createIdMap(definition.typeParameters.map(_.name))
      val innerEnv = env ++ Env(variables = Map(), types = typeParamIdMap)
      definition.typeParameters.zip(typeArgs).foreach {
        case (typeParameter, typeArg) =>
          val id = typeParamIdMap(typeParameter.name)
          innerDefs.addType(FlatTessla.TypeEntry(id, 0, _ => typeArg, typeParameter.loc))
      }
      definition.body match {
        case Tessla.TypeAlias(typ) => translateType(typ, innerDefs, innerEnv)
        case Tessla.BuiltInType(id) => FlatTessla.BuiltInType(id.name, typeArgs)
      }
    }
    defs.addType(FlatTessla.TypeEntry(env.types(definition.id.name), definition.typeParameters.length, constructType, definition.loc))
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
  def expToId(exp: FlatTessla.Expression, defs: FlatTessla.Definitions) = exp match {
    case v: FlatTessla.Variable => v.id
    case _ =>
      val id = makeIdentifier()
      defs.addVariable(FlatTessla.VariableEntry(id, exp, None, Seq(), exp.loc))
      id
  }

  def translateType(typ: Tessla.Type, defs: FlatTessla.Definitions, env: Env): FlatTessla.Type = typ match {
    case Tessla.SimpleType(id) =>
      val typeEntry = env.types.get(id.name).flatMap(defs.resolveType).getOrElse(throw UndefinedType(id.name, id.loc))
      if (typeEntry.arity == 0) typeEntry.typeConstructor(Seq())
      else throw TypeArityMismatch(id.name, typeEntry.arity, 0, id.loc)

    case Tessla.TypeApplication(id, typeArgs, _) =>
      val translatedArgs = typeArgs.map(translateType(_, defs, env))
      val typeEntry = defs.resolveType(env.types.getOrElse(id.name, throw UndefinedType(id.name, id.loc))).getOrElse(throw UndefinedType(id.name, id.loc))
      if (typeEntry.arity == translatedArgs.length) typeEntry.typeConstructor(translatedArgs)
      else throw TypeArityMismatch(id.name, typeEntry.arity, translatedArgs.length, id.loc)

    case Tessla.FunctionType(parameterTypes, returnType, _) =>
      FlatTessla.FunctionType(
        // Explicitly written function types never have any type arguments because we don't support higher rank types
        Seq(),
        parameterTypes.map(translateType(_, defs, env)),
        translateType(returnType, defs, env),
        // Explicitly written function types are never liftable because we have no syntax for that
        isLiftable = false
      )

    case ot: Tessla.ObjectType =>
      checkForDuplicates(ot.memberTypes.map(_._1))
      val memberTypes = ot.memberTypes.map {
        case (id, t) =>
          id.name -> translateType(t, defs, env)
      }
      FlatTessla.ObjectType(memberTypes.toMap, ot.isOpen)

    case tt: Tessla.TupleType =>
      val memberTypes = tt.elementTypes.zipWithIndex.map {
        case (t, idx) =>
          s"_${idx+1}" -> translateType(t, defs, env)
      }
      FlatTessla.ObjectType(memberTypes.toMap, isOpen = false)
  }

  def translateExpression(expr: Tessla.Expression, defs: FlatTessla.Definitions, env: Env): FlatTessla.Expression = expr match {
    case variable: Tessla.Variable =>
      getExp(variable.id, env)

    case literal: Tessla.Literal =>
      FlatTessla.Literal(literal.value, literal.loc)

    case call: Tessla.MacroCall =>
      val mac = expToId(translateExpression(call.mac, defs, env), defs)
      val args = call.args.map {
        case arg: Tessla.NamedArgument[_] =>
          val id = expToId(translateExpression(arg.expr, defs, env), defs)
          FlatTessla.NamedArgument(arg.id.name, FlatTessla.IdLoc(id, arg.id.loc), arg.loc)
        case arg: Tessla.PositionalArgument[_] =>
          val id = expToId(translateExpression(arg.expr, defs, env), defs)
          FlatTessla.PositionalArgument(id, arg.loc)
      }
      FlatTessla.MacroCall(mac, call.mac.loc, call.typeArgs.map(translateType(_, defs, env)), args, call.loc)

    case ite: Tessla.StaticIfThenElse =>
      FlatTessla.StaticIfThenElse(
        FlatTessla.IdLoc(expToId(translateExpression(ite.condition, defs, env), defs), ite.condition.loc),
        FlatTessla.IdLoc(expToId(translateExpression(ite.thenCase, defs, env), defs), ite.thenCase.loc),
        FlatTessla.IdLoc(expToId(translateExpression(ite.elseCase, defs, env), defs), ite.elseCase.loc),
        ite.loc)

    case block: Tessla.Block =>
      val innerEnv = env ++ Env(variables = createIdMap(block.definitions.map(_.id.name)), types = Map())
      block.definitions.foreach { definition =>
        addDefinition(definition, defs, innerEnv)
      }
      val id = expToId(translateExpression(block.expression, defs, innerEnv), defs)
      FlatTessla.Variable(id, block.expression.loc)

    case objectLit: Tessla.ObjectLiteral =>
      checkForDuplicates(objectLit.members.map(_.id))
      val innerEnv = env ++ Env(variables = createIdMap(objectLit.members.map(_.id.name)), types = Map())
      val members = objectLit.members.map { member =>
        val id = member.id
        val exp = member match {
          case full: Tessla.MemberDefinition.Full => full.value
          case _: Tessla.MemberDefinition.Simple => Tessla.Variable(id)
        }
        // Use env instead of innerEnv here because for `{x=x, y=y}` or just `{x, y}` we specifically want to look
        // up the values of x and y in the defs outside of the object, not inside the object, which would just lead
        // to infinite recursion.
        val body = translateExpression(exp, defs, env)
        defs.addVariable(FlatTessla.VariableEntry(innerEnv.variables(id.name), body, None, Seq(), id.loc))
        id.name -> FlatTessla.IdLoc(innerEnv.variables(id.name), exp.loc)
      }
      FlatTessla.ObjectLiteral(members.toMap, objectLit.loc)

    case tuple: Tessla.Tuple =>
      val members = tuple.elements.zipWithIndex.map {
        case (element, index) =>
          s"_${index+1}" -> FlatTessla.IdLoc(expToId(translateExpression(element, defs, env), defs), element.loc)
      }
      FlatTessla.ObjectLiteral(members.toMap, tuple.loc)

    case memberAccess: Tessla.MemberAccess =>
      val receiverId = expToId(translateExpression(memberAccess.receiver, defs, env), defs)
      val receiver = FlatTessla.IdLoc(receiverId, memberAccess.receiver.loc)
      FlatTessla.MemberAccess(receiver, memberAccess.member.name, memberAccess.member.loc, memberAccess.loc)

    case lambda: Tessla.Lambda =>
      val (blockDefs, exp) = lambda.body match {
        case block: Tessla.Block => (block.definitions, block.expression)
        case e => (Seq(), e)
      }
      checkForDuplicates(lambda.parameters.map(_.id) ++ blockDefs.map(_.id))
      val innerDefs = new FlatTessla.Definitions(Some(defs))
      val paramIdMap = createIdMap(lambda.parameters.map(_.id.name))
      val innerEnv = env ++ Env(variables = paramIdMap ++ createIdMap(blockDefs.map(_.id.name)), types = Map())
      val parameters = lambda.parameters.map(translateParameter(_, paramIdMap, innerDefs, innerEnv))
      parameters.foreach { param =>
        val typ = param.parameterType
        innerDefs.addVariable(FlatTessla.VariableEntry(paramIdMap(param.name), param, Some(typ), Seq(), param.loc))
      }
      blockDefs.foreach { innerDef =>
        addDefinition(innerDef, innerDefs, innerEnv)
      }
      val body = translateExpression(exp, innerDefs, innerEnv)
      FlatTessla.Macro(Seq(), parameters, innerDefs, None, lambda.headerLoc, body, lambda.loc, isLiftable = false)
  }
}

object Flattener extends TranslationPhase[Tessla.Specification, FlatTessla.Specification] {
  override def translate(spec: Tessla.Specification) = {
    new Flattener(spec).translate()
  }
}
/*
 * Copyright 2020 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core
import de.uni_luebeck.isp.tessla.core.Errors._
import de.uni_luebeck.isp.tessla.core.FlatTessla.AnnotationEntry
import de.uni_luebeck.isp.tessla.core.Warnings.ConflictingOut

import scala.annotation.unused
import scala.collection.mutable.ArrayBuffer

object Flattener extends TranslationPhase[Tessla.Specification, FlatTessla.Specification] {
  override def translate(spec: Tessla.Specification): TranslationPhase.Result[FlatTessla.Specification] = {
    new Flattener(spec).translate()
  }
}

/**
 * Performs the following tasks:
 *  - Turns nested expressions into a flat, three-address-code-like structure
 *  - Append a unique numeric value to each identifier to make them unique across the entire specification
 *  - Adds scope objects to the global and macro scopes, which map identifiers to their associated expressions,
 *  and produces errors for conflicting definitions in the same scope
 *  - Removes block expressions and hoists them into the surrounding macro scope or the global scope)
 *  - Transforms modules into records
 *  - Resolves imports of modules by replacing usages of imported identifiers by a member-access on that module.
 *
  */
class Flattener(spec: Tessla.Specification)
    extends FlatTessla.IdentifierFactory
    with TranslationPhase.Translator[FlatTessla.Specification] {
  type IdMap = Map[String, FlatTessla.Identifier]
  type AnnotationIdMap = Map[(String, Boolean), FlatTessla.Identifier]
  type Imports = Map[String, FlatTessla.Identifier]

  private case class Env(variables: IdMap, types: IdMap, annotations: AnnotationIdMap) {
    def ++(other: Env): Env = Env(
      variables ++ other.variables,
      types ++ other.types,
      annotations ++ other.annotations
    )
  }

  private def createIdMap(names: Iterable[String]): IdMap = {
    names.map(name => name -> makeIdentifier(name)).toMap
  }

  private def createAnnotationIdMap(names: Iterable[(String, Boolean)]): AnnotationIdMap = {
    names.map {
      case (name, isGlobal) => (name, isGlobal) -> makeIdentifier(name)
    }.toMap
  }

  private def createEnv(
    varNames: Iterable[String],
    typeNames: Iterable[String],
    annotationEntries: Iterable[(String, Boolean)]
  ): Env = {
    Env(createIdMap(varNames), createIdMap(typeNames), createAnnotationIdMap(annotationEntries))
  }

  private def getName(statement: Tessla.Statement): Option[String] = Tessla.getId(statement).map(_.name)

  private val globalDefs = new FlatTessla.Definitions(None)
  private val globalVariableIdMap = createIdMap(spec.statements.flatMap(getName))
  private val globalTypeIdMap = createIdMap(spec.statements.collect {
    case typeDef: Tessla.TypeDefinition => typeDef.id.name
  })

  private val globalAnnotationIdMap = createAnnotationIdMap(spec.statements.collect {
    case annotationDef: Tessla.AnnotationDefinition => (annotationDef.id.name, annotationDef.global)
  })

  private var moduleEnvs = Map[FlatTessla.Identifier, Env]()
  private var globalImports = Map[String, FlatTessla.Identifier]()
  private val globalAnnotations = ArrayBuffer[FlatTessla.Annotation]()
  private val globalEnv = Env(globalVariableIdMap, globalTypeIdMap, globalAnnotationIdMap)

  private val errorExpression: FlatTessla.Variable = FlatTessla.Variable(makeIdentifier("<<error>>"), Location.unknown)

  override def translateSpec(): FlatTessla.Specification = {
    val emptySpec = FlatTessla.Specification(Seq(), globalDefs, Seq(), outAll = None, globalVariableIdMap)

    spec.statements.collect {
      case module: Tessla.Module => addModuleEnv(module, globalEnv)
    }

    val withDefs = spec.statements.sorted.foldLeft(emptySpec) {
      case (result, outAll: Tessla.OutAll) =>
        if (result.hasOutAll) warn(ConflictingOut(outAll.loc, previous = result.outAll.get.loc))
        val annotations = outAll.annotations.map(translateAnnotation(_, global = false))
        result.copy(outAll = Some(FlatTessla.OutAll(annotations, outAll.loc)))

      case (result, out: Tessla.Out) =>
        result.outStreams.find(_.name == out.name).foreach { previous =>
          warn(ConflictingOut(out.loc, previous = previous.loc))
        }
        val id = expToId(translateExpression(out.expr, globalDefs, globalEnv, globalImports), globalDefs)
        val annotations = out.annotations.map(
          translateAnnotation(_, global = false)
        )
        val newOut = FlatTessla.OutStream(id, out.name, annotations, out.loc)
        result.copy(outStreams = result.outStreams :+ newOut)

      case (result, definition: Tessla.Definition) =>
        addDefinition(definition, globalDefs, globalEnv, globalImports)
        result

      case (result, typeDef: Tessla.TypeDefinition) =>
        addTypeDefinition(typeDef, globalDefs, globalEnv, globalImports)
        result

      case (result, annotationDef: Tessla.AnnotationDefinition) =>
        addAnnotationDefinition(annotationDef, globalDefs, globalEnv)
        result

      case (result, in: Tessla.In) =>
        addInStream(in, globalDefs, globalEnv)
        result

      case (result, module: Tessla.Module) =>
        addModule(module, globalDefs, globalEnv, globalImports)
        result

      case (result, imprt: Tessla.Import) =>
        globalImports = addImport(globalImports, imprt, globalDefs, moduleEnvs, globalEnv)
        result.copy(globalNames = result.globalNames ++ globalImports)

      case (result, annotation: Tessla.GlobalAnnotation) =>
        globalAnnotations += translateAnnotation(annotation)
        result
    }

    withDefs.copy(annotations = globalAnnotations.toSeq)
  }

  /**
   * Add an input stream to the definitions.
   *
    * @param in the input stream to add
   * @param defs the definitions to add the stream to
   * @param env the current environment
   */
  def addInStream(in: Tessla.In, defs: FlatTessla.Definitions, env: Env): Unit = {
    val streamType = translateType(in.streamType, defs, env)
    val inputStream = FlatTessla.InputStream(in.id.name, streamType, in.streamType.loc, in.loc)
    val annotations = in.annotations.map(translateAnnotation(_, global = false))
    val entry = FlatTessla.VariableEntry(
      env.variables(in.id.name),
      inputStream,
      Some(streamType),
      annotations,
      false,
      in.loc
    )
    defs.addVariable(entry)
  }

  def addAnnotationDefinition(annoDef: Tessla.AnnotationDefinition, defs: FlatTessla.Definitions, env: Env): Unit = {
    val paramIdMap = createIdMap(annoDef.parameters.map(_.id.name))
    defs.addAnnotation(
      AnnotationEntry(
        env.annotations((annoDef.id.name, annoDef.global)),
        annoDef.parameters.map(translateParameter(_, paramIdMap, defs, env)),
        annoDef.global,
        annoDef.loc
      )
    )
  }

  /**
   * Create the environment for the given module
   * @param module the module
   * @param outerEnv the current environment
   */
  def addModuleEnv(module: Tessla.Module, outerEnv: Env): Unit = {
    val env = createEnv(
      module.contents.flatMap(getName),
      module.contents.collect {
        case typeDef: Tessla.TypeDefinition => typeDef.id.name
      },
      module.contents.collect {
        case annoDef: Tessla.AnnotationDefinition => annoDef.id.name -> annoDef.global
      }
    )
    moduleEnvs += outerEnv.variables(module.name) -> env

    module.contents.collect {
      case module: Tessla.Module => addModuleEnv(module, env)
    }
  }

  /**
   * Add a module to the definitions
   * @param module the module to add
   * @param defs the definitions to add the module to
   * @param outerEnv the current environment
   * @param outerImports the currently scoped imports
   */
  def addModule(module: Tessla.Module, defs: FlatTessla.Definitions, outerEnv: Env, outerImports: Imports): Unit = {
    val innerEnv = moduleEnvs(outerEnv.variables(module.name))
    val env = outerEnv ++ innerEnv
    val variableIdMap = env.variables

    var innerImports = Map[String, FlatTessla.Identifier]()
    var imports = outerImports

    module.contents.sorted.foreach {
      case definition: Tessla.Definition =>
        addDefinition(definition, defs, env, imports)

      case module: Tessla.Module =>
        addModule(module, defs, env, imports)

      case inout @ (_: Tessla.Out | _: Tessla.OutAll | _: Tessla.In) =>
        error(InOutStatementInModule(inout.loc))

      case annoDef: Tessla.AnnotationDefinition =>
        error(AnnotationDefInModule(annoDef.loc))

      case imprt: Tessla.Import =>
        innerImports = addImport(innerImports, imprt, defs, moduleEnvs, env)
        imports ++= innerImports

      case typeDef: Tessla.TypeDefinition =>
        addTypeDefinition(typeDef, defs, env, imports)

      case annotation: Tessla.GlobalAnnotation =>
        globalAnnotations += translateAnnotation(annotation)
    }
    val valueMembers = module.contents.flatMap { stat =>
      getName(stat).map { name =>
        name -> FlatTessla.IdLoc(variableIdMap(name), stat.loc)
      }
    }.toMap
    val obj = FlatTessla.ObjectLiteral(valueMembers, module.loc)
    defs.addVariable(
      FlatTessla.VariableEntry(outerEnv.variables(module.name), obj, None, Seq(), false, module.loc)
    )
  }

  /**
   * Resolve and add an import to the current imports
   * @param imports the current imports
   * @param imprt the import to add
   * @param defs the current definitions
   * @param envs the mapping of module environments
   * @param outerEnv the current environment
   * @return the resolved imports
   */
  def addImport(
    imports: Imports,
    imprt: Tessla.Import,
    defs: FlatTessla.Definitions,
    envs: Map[FlatTessla.Identifier, Env],
    outerEnv: Env
  ): Imports = {
    val env = imprt.path.foldLeft(outerEnv) {
      case (env, id) =>
        val resolved = env.variables.getOrElse(id.name, throw UndefinedVariable(id))
        envs(resolved)
    }
    val head = Tessla.Variable(imprt.path.head)
    val access = imprt.path.tail.foldLeft[Tessla.Expression](head)(Tessla.MemberAccess(_, _, imprt.loc))

    val newImports = env.variables.keySet
      .map(k =>
        k -> {
          val exp = Tessla.MemberAccess(access, Tessla.Identifier(k, imprt.loc), imprt.loc)
          val flatExp = translateExpression(exp, defs, outerEnv, Map())
          expToId(flatExp, defs)
        }
      )
      .toMap

    imports.keySet
      .intersect(newImports.keySet)
      .foreach(s => error(ImportAmbiguousDefinitionError(imprt.path.mkString("."), s, imprt.loc)))
    imports ++ newImports
  }

  /**
   * Translate and add a definition to the current definitions.
   *
    * In general, the inner definitions of the definition get added to a newly created local set of definitions,
   * and then the return expression gets translated.
   *
    * For parameterless macros, their inner definitions get extracted into the global scope.
   *
    * The body of the definition is flattened using [[expToId]].
   *
    * @param definition the definition to add
   * @param defs the current definitions
   * @param env the current environment
   * @param imports the current imports
   */
  def addDefinition(definition: Tessla.Definition, defs: FlatTessla.Definitions, env: Env, imports: Imports): Unit = {
    val (blockDefs, defBody) = definition.body match {
      case Tessla.ExpressionBody(block: Tessla.Block) =>
        (block.definitions, Tessla.ExpressionBody(block.expression))
      case b => (Seq(), b)
    }

    if (definition.parameters.isEmpty && definition.typeParameters.isEmpty) {
      if (definition.isLiftable) throw LiftableOnNonMacro(definition.loc, definition.id.name)

      // For parameterless definitions, inner definitions become part of the global defs in flat tessla.
      // Flat tessla only has distinct defs for macros with parameters (as those need to be instantiated
      // multiple times)
      val innerEnv = env ++ Env(variables = createIdMap(blockDefs.map(_.id.name)), types = Map(), annotations = Map())
      blockDefs.foreach { blockDef =>
        addDefinition(blockDef, defs, innerEnv, imports)
      }
      val typ = definition.returnType.map(translateType(_, defs, innerEnv))
      val exp = defBody match {
        case b: Tessla.ExpressionBody => translateExpression(b.exp, defs, innerEnv, imports)
        case b: Tessla.Extern =>
          val referenceImplementation = b.referenceImplementation.map { exp =>
            expToId(translateExpression(exp, defs, env, imports), defs, typ)
          }
          FlatTessla.Extern(b.id.name, Seq(), Seq(), referenceImplementation, b.id.loc)
      }
      defs.addVariable(
        FlatTessla
          .VariableEntry(env.variables(definition.id.name), exp, typ, Seq(), definition.parens, definition.headerLoc)
      )
    } else {
      val innerDefs = new FlatTessla.Definitions(Some(defs))
      val paramIdMap = createIdMap(definition.parameters.map(_._2.id.name))
      val typeParamIdMap = createIdMap(definition.typeParameters.map(_.name))
      // Environment that contains the macro's parameters and type parameters, but not any of its inner definitions
      // This is used to process the type signature as we want to be able to use type arguments there,
      // but not any type definitions that are inside the macro (once we have those)
      val paramEnv = env ++ Env(variables = paramIdMap, types = typeParamIdMap, annotations = Map())
      val innerEnv =
        paramEnv ++ Env(variables = createIdMap(blockDefs.map(_.id.name)), types = Map(), annotations = Map())
      definition.typeParameters.foreach { typeParameter =>
        val tp = FlatTessla.TypeParameter(typeParamIdMap(typeParameter.name), typeParameter.loc)
        innerDefs.addType(FlatTessla.TypeEntry(tp.id, 0, _ => tp, tp.loc))
      }
      val parameters = definition.parameters.map(p => p._1 -> translateParameter(p._2, paramIdMap, innerDefs, paramEnv))
      parameters.map(_._2).foreach { param =>
        val typ = param.parameterType
        innerDefs.addVariable(
          FlatTessla.VariableEntry(paramIdMap(param.name), param, Some(typ), Seq(), false, param.loc)
        )
      }
      blockDefs.foreach { innerDef =>
        addDefinition(innerDef, innerDefs, innerEnv, imports)
      }
      // Get the values of the type map in the order in which they appeared in the type parameter list
      val typeParameters = definition.typeParameters.map(tp => paramEnv.types(tp.name))
      val returnTypeOpt = definition.returnType.map(translateType(_, innerDefs, paramEnv))
      def expBody(tesslaExp: Tessla.Expression, id: FlatTessla.Identifier): Unit = {
        val exp = translateExpression(tesslaExp, innerDefs, innerEnv, imports)
        val expId = expToId(exp, innerDefs, returnTypeOpt)
        val mac = FlatTessla.Macro(
          typeParameters,
          parameters,
          innerDefs,
          returnTypeOpt,
          definition.headerLoc,
          FlatTessla.IdLoc(expId, exp.loc),
          definition.loc,
          isLiftable = definition.isLiftable
        )
        val typ = returnTypeOpt.map { returnType =>
          FlatTessla.FunctionType(
            typeParameters,
            parameters.map(p => p._1 -> p._2.parameterType),
            returnType,
            mac.isLiftable
          )
        }
        defs.addVariable(FlatTessla.VariableEntry(id, mac, typ, Seq(), definition.parens, definition.headerLoc))
      }
      defBody match {
        case b: Tessla.ExpressionBody =>
          expBody(b.exp, env.variables(definition.id.name))
        case b: Tessla.Extern =>
          val typ = returnTypeOpt.map { returnType =>
            FlatTessla.FunctionType(
              typeParameters,
              parameters.map(p => p._1 -> p._2.parameterType),
              returnType,
              definition.isLiftable
            )
          }
          val referenceImplementation = b.referenceImplementation.map { exp =>
            val id = makeIdentifier()
            expBody(exp, id)
            id
          }
          val builtIn = FlatTessla.Extern(
            b.id.name,
            typeParameters,
            parameters,
            referenceImplementation,
            b.loc
          )
          defs.addVariable(
            FlatTessla.VariableEntry(
              env.variables(definition.id.name),
              builtIn,
              typ,
              Seq(),
              definition.parens,
              definition.headerLoc
            )
          )
      }
    }
  }

  /** Add a type definition to the current definitions
   * @param definition the type definition to add
   * @param defs the current definitions
   * @param env the current environment
   * @param imports the current imports (unused, as modules do not contain type definitions at this point, which
   *                entails that cannot contain type definitions either)
   */
  def addTypeDefinition(
    definition: Tessla.TypeDefinition,
    defs: FlatTessla.Definitions,
    env: Env,
    @unused imports: Imports // TODO: Currently unused as modules do not contain type definitions.
  ): Unit = {
    def constructType(typeArgs: Seq[FlatTessla.Type]) = {
      val innerDefs = new FlatTessla.Definitions(Some(defs))
      val typeParamIdMap = createIdMap(definition.typeParameters.map(_.name))
      val innerEnv = env ++ Env(variables = Map(), types = typeParamIdMap, annotations = Map())
      definition.typeParameters.zip(typeArgs).foreach {
        case (typeParameter, typeArg) =>
          val id = typeParamIdMap(typeParameter.name)
          innerDefs.addType(FlatTessla.TypeEntry(id, 0, _ => typeArg, typeParameter.loc))
      }
      definition.body match {
        case Tessla.TypeAlias(typ) => translateType(typ, innerDefs, innerEnv)
        case Tessla.ExternType(id) => FlatTessla.BuiltInType(id.name, typeArgs)
      }
    }
    defs.addType(
      FlatTessla.TypeEntry(
        env.types(definition.id.name),
        definition.typeParameters.length,
        constructType,
        definition.loc
      )
    )
  }

  def getExp(tesslaId: Tessla.Identifier, env: Env, imports: Imports): FlatTessla.Expression = {
    env.variables
      .get(tesslaId.name)
      .orElse(imports.get(tesslaId.name))
      .map(FlatTessla.Variable(_, tesslaId.loc))
      .getOrElse {
        error(UndefinedVariable(tesslaId))
        errorExpression
      }
  }

  /**
   * Turn an arbitrary expression into an identifier by either creating a new identifier and making it point to the
   * expression or, if the expression is a variable reference, returning the referenced identifier.
   */
  def expToId(
    exp: FlatTessla.Expression,
    defs: FlatTessla.Definitions,
    typeAnnotation: FlatTessla.TypeAnnotation = None
  ): FlatTessla.Identifier = exp match {
    case v: FlatTessla.Variable => v.id
    case _ =>
      val id = makeIdentifier()
      defs.addVariable(FlatTessla.VariableEntry(id, exp, typeAnnotation, Seq(), false, exp.loc))
      id
  }

  def translateParameter(
    param: Tessla.Parameter,
    idMap: IdMap,
    defs: FlatTessla.Definitions,
    env: Env
  ): FlatTessla.Parameter = {
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

  def translateAnnotation(annotation: Tessla.GlobalAnnotation): FlatTessla.Annotation = {
    translateAnnotation(annotation.annotation, global = true)
  }

  def translateAnnotation(annotation: Tessla.Annotation, global: Boolean = false): FlatTessla.Annotation = {
    val id = globalEnv.annotations.getOrElse(
      (annotation.name, global), {
        error(UndefinedAnnotation(annotation.id))
        makeIdentifier(annotation.name)
      }
    )

    val args: Seq[FlatTessla.Argument] = annotation.arguments.map {
      case arg: Tessla.NamedArgument[Tessla.Expression] =>
        val id = expToId(translateExpression(arg.expr, globalDefs, globalEnv, globalImports), globalDefs)
        FlatTessla.NamedArgument(arg.id.name, FlatTessla.IdLoc(id, arg.id.loc), arg.loc)
      case arg: Tessla.PositionalArgument[Tessla.Expression] =>
        val id = expToId(translateExpression(arg.expr, globalDefs, globalEnv, globalImports), globalDefs)
        FlatTessla.PositionalArgument(id, arg.loc)
    }
    FlatTessla.Annotation(id, args, annotation.loc)
  }

  def translateType(typ: Tessla.Type, defs: FlatTessla.Definitions, env: Env): FlatTessla.Type =
    typ match {
      case Tessla.SimpleType(id) =>
        val typeEntry = env.types
          .get(id.name)
          .flatMap(defs.resolveType)
          .getOrElse(throw UndefinedType(id.name, id.loc))
        if (typeEntry.arity == 0) typeEntry.typeConstructor(Seq())
        else throw TypeArityMismatch(id.name, typeEntry.arity, 0, id.loc)

      case Tessla.TypeApplication(id, typeArgs, _) =>
        val translatedArgs = typeArgs.map(translateType(_, defs, env))
        val typeEntry = defs
          .resolveType(env.types.getOrElse(id.name, throw UndefinedType(id.name, id.loc)))
          .getOrElse(throw UndefinedType(id.name, id.loc))
        if (typeEntry.arity == translatedArgs.length) typeEntry.typeConstructor(translatedArgs)
        else throw TypeArityMismatch(id.name, typeEntry.arity, translatedArgs.length, id.loc)

      case Tessla.FunctionType(parameterTypes, returnType, _) =>
        FlatTessla.FunctionType(
          // Explicitly written function types never have any type arguments because we don't support higher rank types
          Seq(),
          parameterTypes.map(t => (t._1, translateType(t._2, defs, env))),
          translateType(returnType, defs, env),
          // Explicitly written function types are never liftable because we have no syntax for that
          isLiftable = false
        )

      case ot: Tessla.ObjectType =>
        val memberTypes = ot.memberTypes.map {
          case (id, t) =>
            id.name -> translateType(t, defs, env)
        }
        FlatTessla.ObjectType(memberTypes)
    }

  def translateExpression(
    expr: Tessla.Expression,
    defs: FlatTessla.Definitions,
    env: Env,
    imports: Imports
  ): FlatTessla.Expression = expr match {
    case variable: Tessla.Variable =>
      getExp(variable.id, env, imports)

    case literal: Tessla.Literal =>
      FlatTessla.Literal(literal.value, literal.loc)

    case call: Tessla.MacroCall =>
      val mac = expToId(translateExpression(call.mac, defs, env, imports), defs)
      val args = call.args.map {
        case arg: Tessla.NamedArgument[Tessla.Expression] =>
          val id = expToId(translateExpression(arg.expr, defs, env, imports), defs)
          FlatTessla.NamedArgument(arg.id.name, FlatTessla.IdLoc(id, arg.id.loc), arg.loc)
        case arg: Tessla.PositionalArgument[Tessla.Expression] =>
          val id = expToId(translateExpression(arg.expr, defs, env, imports), defs)
          FlatTessla.PositionalArgument(id, arg.loc)
      }
      FlatTessla.MacroCall(
        mac,
        call.mac.loc,
        call.typeArgs.map(translateType(_, defs, env)),
        args,
        call.loc
      )

    case block: Tessla.Block =>
      val innerEnv =
        env ++ Env(variables = createIdMap(block.definitions.map(_.id.name)), types = Map(), annotations = Map())
      block.definitions.foreach { definition =>
        addDefinition(definition, defs, innerEnv, imports)
      }
      val id = expToId(translateExpression(block.expression, defs, innerEnv, imports), defs)
      FlatTessla.Variable(id, block.expression.loc)

    case objectLit: Tessla.ObjectLiteral =>
      val innerEnv =
        env ++ Env(variables = createIdMap(objectLit.members.keys.map(_.name)), types = Map(), annotations = Map())
      val members = objectLit.members.map {
        case (id, exp) =>
          // Use env instead of innerEnv here because for `{x=x, y=y}` or just `{x, y}` we specifically want to look
          // up the values of x and y in the defs outside of the object, not inside the object, which would just lead
          // to infinite recursion.
          val body = translateExpression(exp, defs, env, imports)
          defs.addVariable(
            FlatTessla.VariableEntry(innerEnv.variables(id.name), body, None, Seq(), false, id.loc)
          )
          id.name -> FlatTessla.IdLoc(innerEnv.variables(id.name), exp.loc)
      }
      FlatTessla.ObjectLiteral(members, objectLit.loc)

    case rootMemberAccess: Tessla.RootMemberAccess =>
      getExp(rootMemberAccess.member, globalEnv, imports)

    case memberAccess: Tessla.MemberAccess =>
      val receiverId = expToId(translateExpression(memberAccess.receiver, defs, env, imports), defs)
      val receiver = FlatTessla.IdLoc(receiverId, memberAccess.receiver.loc)
      FlatTessla.MemberAccess(
        receiver,
        memberAccess.member.name,
        memberAccess.member.loc,
        memberAccess.loc
      )

    case lambda: Tessla.Lambda =>
      val (blockDefs, exp) = lambda.body match {
        case block: Tessla.Block => (block.definitions, block.expression)
        case e                   => (Seq(), e)
      }
      val innerDefs = new FlatTessla.Definitions(Some(defs))
      val paramIdMap = createIdMap(lambda.parameters.map(_._2.id.name))
      val innerEnv =
        env ++ Env(variables = paramIdMap ++ createIdMap(blockDefs.map(_.id.name)), types = Map(), annotations = Map())
      val parameters = lambda.parameters.map(p => p._1 -> translateParameter(p._2, paramIdMap, innerDefs, innerEnv))
      parameters.map(_._2).foreach { param =>
        val typ = param.parameterType
        innerDefs.addVariable(
          FlatTessla.VariableEntry(paramIdMap(param.name), param, Some(typ), Seq(), true, param.loc)
        )
      }
      blockDefs.foreach { innerDef =>
        addDefinition(innerDef, innerDefs, innerEnv, imports)
      }
      val resultExp = translateExpression(exp, innerDefs, innerEnv, imports)
      val resultId = expToId(resultExp, innerDefs)
      FlatTessla.Macro(
        Seq(),
        parameters,
        innerDefs,
        None,
        lambda.headerLoc,
        FlatTessla.IdLoc(resultId, resultExp.loc),
        lambda.loc,
        isLiftable = false
      )
  }

  implicit val ordering: Ordering[Tessla.Statement] = Ordering.by {
    case _: Tessla.Import         => 0
    case _: Tessla.TypeDefinition => 1
    case _: Tessla.Module         => 2
    case _                        => 3
  }
}

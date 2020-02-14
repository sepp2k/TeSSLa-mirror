package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import util.mapValues
import scala.collection.mutable

class TypeChecker(spec: FlatTessla.Specification)
  extends TypedTessla.IdentifierFactory with TranslationPhase.Translator[TypedTessla.TypedSpecification] {
  private val typeMap = mutable.Map[TypedTessla.Identifier, TypedTessla.Type]()
  type Env = Map[FlatTessla.Identifier, TypedTessla.Identifier]

  override def translateSpec(): TypedTessla.TypedSpecification = {
    val (defs, env) = translateDefsWithParents(spec.globalDefs)
    var outputStreams = spec.outStreams.map(translateOutStream(_, defs, env))
    if (spec.outAll) {
      val streams = defs.variables.values.filter(entry => entry.typeInfo.isStreamType)
      outputStreams ++= streams.flatMap { entry =>
        entry.id.nameOpt.map(name => TypedTessla.OutStream(entry.id, Some(name), entry.loc))
      }
    }
    TypedTessla.TypedSpecification(defs, outputStreams, spec.outAllLocation)
  }

  def translateOutStream(stream: FlatTessla.OutStream, defs: TypedTessla.Definitions, env: Env): TypedTessla.OutStream = {
    val id = env(stream.id)
    typeMap(id) match {
      case b: TypedTessla.BuiltInType if b.isStreamType =>
        TypedTessla.OutStream(id, stream.nameOpt, stream.loc)
      case t if t.isValueType =>
        TypedTessla.OutStream(liftConstant(id, defs, env, stream.loc), stream.nameOpt, stream.loc)
      case other =>
        error(TypeMismatch(expected = "stream or value type", other, stream.loc))
        TypedTessla.OutStream(id, Some("<error>"), stream.loc)
    }
  }

  def processTypeAnnotation(entry: FlatTessla.VariableEntry, env: Env) = entry.typeInfo.foreach { typ =>
    typeMap(env(entry.id)) = translateType(typ, env)
  }

  def translateType(typ: FlatTessla.Type, env: Env): TypedTessla.Type = typ match {
    case b: FlatTessla.BuiltInType => TypedTessla.BuiltInType(b.name, b.typeArgs.map(translateType(_, env)))
    case f: FlatTessla.FunctionType =>
      val typeParams = f.typeParameters.map(tvar => makeIdentifier(tvar.nameOpt))
      val innerEnv = env ++ f.typeParameters.zip(typeParams).toMap
      val paramTypes = f.parameterTypes.map(translateType(_, innerEnv))
      TypedTessla.FunctionType(typeParams, paramTypes, translateType(f.returnType, innerEnv), f.isLiftable)
    case o: FlatTessla.ObjectType =>
      TypedTessla.ObjectType(mapValues(o.memberTypes)(translateType(_, env)), o.isOpen)
    case tvar: FlatTessla.TypeParameter =>
      val newTvar = env.getOrElse(tvar.id, throw InternalError(s"Failed to look up type variable $tvar", tvar.loc))
      TypedTessla.TypeParameter(newTvar, tvar.loc)
  }

  def insertInferredType(id: TypedTessla.Identifier, inferredType: TypedTessla.Type, loc: Location) = {
    typeMap.get(id) match {
      case None =>
        typeMap(id) = inferredType
      case Some(declaredType) =>
        if (!isSubtypeOrEqual(parent = declaredType, child = inferredType)) {
          error(TypeMismatch(declaredType, inferredType, loc))
        }
    }
  }

  def translateDefsWithParents(defs: FlatTessla.Definitions): (TypedTessla.Definitions, Env) = {
    defs.parent.map(translateDefsWithParents) match {
      case Some((parentDefs, parentEnv)) =>
        translateDefs(defs, Some(parentDefs), parentEnv)
      case None =>
        translateDefs(defs, None, Map())
    }
  }

  def parameterTypes(mac: FlatTessla.Macro): Seq[FlatTessla.Type] = {
    mac.parameters.map(_.parameterType)
  }

  def isLiftableMacro(exp: FlatTessla.Expression): Boolean = exp match {
    case m: FlatTessla.Macro => m.isLiftable
    case _ => false
  }

  def isBuiltIn(exp: FlatTessla.Expression): Boolean = exp.isInstanceOf[FlatTessla.BuiltInOperator]

  /**
    * Return all the entries that need to be type inferred before the current entry, i.e.
    * all the entries that are used by this entry and do not have an explicit type annotation.
    */
  def requiredEntries(defs: FlatTessla.Definitions, entry: FlatTessla.VariableEntry): Seq[FlatTessla.VariableEntry] = {
    requiredEntries(defs, entry.expression)
  }

  def requiredEntries(defs: FlatTessla.Definitions, expression: FlatTessla.Expression): Seq[FlatTessla.VariableEntry] = {
    def resolve(id: FlatTessla.Identifier) = {
      // An entry needs to be processed before this one iff this one uses it and it either has no type annotation or
      // it is a liftable macro (in which case the lifting needs to happen before it is used)
      defs.resolveVariable(id).toList.filter { arg =>
        arg.typeInfo.isEmpty || isLiftableMacro(arg.expression) || isBuiltIn(arg.expression)
      }
    }
    expression match {
      case v: FlatTessla.Variable =>
        resolve(v.id)
      case _: FlatTessla.Literal | _: FlatTessla.InputStream | _ : FlatTessla.Parameter =>
        Seq()

      case builtIn: FlatTessla.BuiltInOperator =>
        builtIn.referenceImplementation.toSeq.flatMap(resolve)

      case ite: FlatTessla.StaticIfThenElse =>
        resolve(ite.condition.id) ++ resolve(ite.thenCase.id) ++ resolve(ite.elseCase.id)

      case call: FlatTessla.MacroCall =>
        // Since we invoke requiredEntries* with an outer defs in the macro case (see below), we might encounter
        // identifiers that aren't defined in the defs we see, so we use flatMap to discard the Nones.
        val args = call.args.flatMap(arg => resolve(arg.id))
        resolve(call.macroID) ++ args

      case mac: FlatTessla.Macro =>
        // Since identifiers used in the macro may either be defined inside or outside the
        // macro (and we only want the outside ones), we need to filter them out afterwards.
        // We still need to use the inner scope to find the required entries, so that the
        // dependencies of inner definitions are still considered.
        // Note that identifiers are unique at this stage, so we won't run into a situation
        // where the macro contains a local identifier that shadows an outer one.
        resolve(mac.result.id) ++ mac.body.variables.values.flatMap(requiredEntries(mac.body, _)).filter {
          definition => defs.resolveVariable(definition.id).isDefined
        }

      case obj: FlatTessla.ObjectLiteral =>
        obj.members.values.flatMap(member => resolve(member.id)).toSeq

      case acc: FlatTessla.MemberAccess =>
        resolve(acc.receiver.id)
    }
  }

  def translateDefs(defs: FlatTessla.Definitions, parent: Option[TypedTessla.Definitions], parentEnv: Env): (TypedTessla.Definitions, Env) = {
    val env = parentEnv ++ mapValues(defs.variables)(entry => makeIdentifier(entry.id.nameOpt))
    val resultingDefs = new TypedTessla.Definitions(parent)
    defs.variables.values.foreach(processTypeAnnotation(_, env))

    ReverseTopologicalSort.sort(defs.variables.values)(requiredEntries(defs, _)) match {
      case ReverseTopologicalSort.Cycles(nodesInCycles) =>
        nodesInCycles.foreach { entry =>
          entry.id.nameOpt.foreach(name => error(MissingTypeAnnotationRec(name, entry.loc)))
        }
        abort()
      case ReverseTopologicalSort.Sorted(sorted) =>
        sorted.filter(defs.variables.values.toSeq.contains(_)).foreach { entry =>
          resultingDefs.addVariable(translateEntry(entry, resultingDefs, env))
        }
    }
    (resultingDefs, env)
  }

  def translateEntry(entry: FlatTessla.VariableEntry, defs: TypedTessla.Definitions, env: Env): TypedTessla.VariableEntry = {
    val id = env(entry.id)
    val (exp, typ) = translateExpression(entry.expression, typeMap.get(id), Some(id), defs, env)
    insertInferredType(id, typ, exp.loc)
    val annotations = entry.annotations.map { annotation =>
      TypedTessla.Annotation(annotation.name, annotation.arguments, annotation.loc)
    }
    TypedTessla.VariableEntry(id, exp, typ, annotations, entry.loc)
  }

  def typeSubst(expected: TypedTessla.Type, actual: TypedTessla.Type, typeParams: Set[TypedTessla.Identifier],
                substitutions: mutable.Map[TypedTessla.Identifier, TypedTessla.Type]): TypedTessla.Type = {
    (expected, actual) match {
      case (tparam: TypedTessla.TypeParameter, _) =>
        if (!actual.isValueType) {
          // Ugly hack: By returning an unexpanded type variable named "value type", the error message will say
          // "Expected value type"
          TypedTessla.TypeParameter(makeIdentifier("value type"), tparam.loc)
        } else if (typeParams.contains(tparam.id)) {
          substitutions.getOrElseUpdate(tparam.id, actual)
        } else {
          tparam
        }
      case (expectedFunctionType: TypedTessla.FunctionType, actualFunctionType: TypedTessla.FunctionType) =>
        // Since we don't support higher-order types, i.e. function types that appear as a subtype can't be generic
        // themselves, we know that none of the function types, except the initial one that was used to generated the
        // type environment, will have type parameters, so we don't need to update the type envrionment with new type
        // variables.
        val parameterTypes = expectedFunctionType.parameterTypes.zip(actualFunctionType.parameterTypes).map {
          case (expectedParamType, actualParamType) =>
            typeSubst(expectedParamType, actualParamType, typeParams, substitutions)
        }
        val returnType = typeSubst(expectedFunctionType.returnType, actualFunctionType.returnType, typeParams, substitutions)
        TypedTessla.FunctionType(Seq(), parameterTypes, returnType, expectedFunctionType.isLiftable)
      case (expectedType: TypedTessla.BuiltInType, actualType: TypedTessla.BuiltInType) if expectedType.name == actualType.name =>
        val typeArgs = expectedType.typeArgs.zip(actualType.typeArgs).map {
          case (expectedElementType, actualElementType) =>
            typeSubst(expectedElementType, actualElementType, typeParams, substitutions)
        }
        TypedTessla.BuiltInType(expectedType.name, typeArgs)
      // Allow for auto-lifting of values
      case (b: TypedTessla.BuiltInType, actualElementType) if b.isStreamType =>
        val expectedElementType = b.typeArgs.head
        TypedTessla.BuiltInType(b.name, typeSubst(expectedElementType, actualElementType, typeParams, substitutions) +: b.typeArgs.tail)
      case (expected: TypedTessla.ObjectType, actual: TypedTessla.ObjectType) =>
        val members = expected.memberTypes.map {
          case (name, expectedMemberType) =>
            name -> actual.memberTypes.get(name).map { actualMemberType =>
              typeSubst(expectedMemberType, actualMemberType, typeParams, substitutions)
            }.getOrElse(expectedMemberType)
        }
        TypedTessla.ObjectType(members, expected.isOpen)
      case (expectedType: TypedTessla.BuiltInType, actualType: TypedTessla.BuiltInType) =>
        assert(expectedType.name != actualType.name)
        expected
      case (expectedType, actualType) =>
        assert(expectedType.getClass != actualType.getClass)
        expected
    }
  }

  def findPredef(name: String, env: Env): TypedTessla.Identifier = {
    env(spec.globalNames.getOrElse(name, throw InternalError(s"Standard library must define $name")))
  }

  def findPredef(name: String, env: Env, loc: Location): TypedTessla.Identifier = {
    env(spec.globalNames.getOrElse(name, throw UndefinedVariable(Tessla.Identifier(s"__root__.$name", loc))))
  }

  def liftConstant(constant: TypedTessla.Identifier, defs: TypedTessla.Definitions, env: Env, loc: Location) = {
    val typeOfConstant = typeMap(constant)
    val liftedType = streamType(typeOfConstant)
    val liftedId = makeIdentifier()
    val nilCall = TypedTessla.MacroCall(findPredef("nil", env), loc, Seq(typeOfConstant), Seq(), loc)
    val nilId = makeIdentifier()
    val nilEntry = TypedTessla.VariableEntry(nilId, nilCall, liftedType, Seq(), loc)
    defs.addVariable(nilEntry)
    val defaultArgs = Seq(
      TypedTessla.PositionalArgument(nilId, loc),
      TypedTessla.PositionalArgument(constant, loc)
    )
    val defaultCall = TypedTessla.MacroCall(findPredef("default", env), loc, Seq(typeOfConstant), defaultArgs, loc)
    val entry = TypedTessla.VariableEntry(liftedId, defaultCall, liftedType, Seq(), loc)
    defs.addVariable(entry)
    liftedId
  }

  def checkLiftability(functionType: TypedTessla.FunctionType) = {
    functionType.parameterTypes.forall(_.isValueType) && functionType.returnType.isValueType
  }

  def liftFunctionType(functionType: TypedTessla.FunctionType) = {
    TypedTessla.FunctionType(
      functionType.typeParameters,
      functionType.parameterTypes.map(streamType),
      streamType(functionType.returnType),
      isLiftable = true
    )
  }

  def isSubtypeOrEqual(parent: TypedTessla.Type, child: TypedTessla.Type): Boolean = (parent, child) match {
    case (parent: TypedTessla.FunctionType, genericChild: TypedTessla.FunctionType) =>
      // TODO: This ignores type parameters of the expected type because functions with type parameters can't
      //       currently be passed around anyway. Once that is possible, this code needs to be adjusted.
      val typeSubstitutions = mutable.Map[TypedTessla.Identifier, TypedTessla.Type]()
      val child = typeSubst(genericChild, parent, genericChild.typeParameters.toSet, typeSubstitutions).
        asInstanceOf[TypedTessla.FunctionType]
      val compatibleLiftedness = !parent.isLiftable || child.isLiftable
      val compatibleReturnTypes = isSubtypeOrEqual(parent.returnType, child.returnType)
      val compatibleParameterTypes = parent.parameterTypes.length == child.parameterTypes.length &&
        parent.parameterTypes.zip(child.parameterTypes).forall {
          // function parameters are contravariant, so the order of arguments to isSubtypeOrEqual is switched
          case (parentParamType, childParamType) => isSubtypeOrEqual(childParamType, parentParamType)
        }
      compatibleLiftedness && compatibleReturnTypes && compatibleParameterTypes
    case (parent: TypedTessla.ObjectType, child: TypedTessla.ObjectType) =>
      parent.memberTypes.forall {
        case (name, typ) =>
          child.memberTypes.get(name).exists(childTyp => isSubtypeOrEqual(parent = typ, child = childTyp))
      } && (parent.isOpen || !child.isOpen && parent.memberTypes.keySet == child.memberTypes.keySet)
    case _ =>
      parent == child
  }

  val intType = TypedTessla.BuiltInType("Int", Seq())
  val floatType = TypedTessla.BuiltInType("Float", Seq())
  val stringType = TypedTessla.BuiltInType("String", Seq())
  val boolType = TypedTessla.BuiltInType("Bool", Seq())

  def streamType(t: TypedTessla.Type) = TypedTessla.BuiltInType("Events", Seq(t))

  def translateExpression(expression: FlatTessla.Expression, declaredType: Option[TypedTessla.Type],
                          id: Option[TypedTessla.Identifier], defs: TypedTessla.Definitions, env: Env)
  : (TypedTessla.Expression, TypedTessla.Type) = {
    expression match {
      case v: FlatTessla.Variable =>
        val id = env(v.id)
        TypedTessla.Variable(id, v.loc) -> typeMap(id)
      case lit: FlatTessla.Literal =>
        val t = lit.value match {
          case _: Tessla.IntLiteral => intType
          case _: Tessla.FloatLiteral => floatType
          case _: Tessla.TimeLiteral =>
            // TODO: Implement units of measure, this should contain the appropriate unit
            intType
          case _: Tessla.StringLiteral => stringType
        }
        TypedTessla.Literal(lit.value, lit.loc) -> t
      case inStream: FlatTessla.InputStream =>
        val typ = translateType(inStream.streamType, env)
        if (!typ.isStreamType) {
          error(InputStreamMustHaveStreamType(inStream.typeLoc))
        }
        TypedTessla.InputStream(inStream.name, typ, inStream.typeLoc, inStream.loc) -> typ
      case param: FlatTessla.Parameter =>
        val id = env(param.id)
        val t = typeMap(id)
        TypedTessla.Parameter(param.param, t, id) -> t
      case ite: FlatTessla.StaticIfThenElse =>
        val cond = TypedTessla.IdLoc(env(ite.condition.id), ite.condition.loc)
        val thenCase = TypedTessla.IdLoc(env(ite.thenCase.id), ite.thenCase.loc)
        val elseCase = TypedTessla.IdLoc(env(ite.elseCase.id), ite.elseCase.loc)
        val condType = typeMap(cond.id)
        if (condType != boolType) {
          error(TypeMismatch(boolType, condType, cond.loc))
        }
        (typeMap(thenCase.id), typeMap(elseCase.id)) match {
          case (s1, s2) if s1.isStreamType && s2.isStreamType =>
            if (s1 != s2) {
              error(TypeMismatch(s1, s2, elseCase.loc))
            }
            TypedTessla.StaticIfThenElse(cond, thenCase, elseCase, ite.loc) -> s1
          case (s: TypedTessla.BuiltInType, v) if s.isStreamType =>
            if (s.typeArgs.head != v) {
              error(TypeMismatch(s.typeArgs.head, v, elseCase.loc))
            }
            val liftedElseCase = elseCase.copy(id = liftConstant(elseCase.id, defs, env, elseCase.loc))
            TypedTessla.StaticIfThenElse(cond, thenCase, liftedElseCase, ite.loc) -> s
          case (v, s: TypedTessla.BuiltInType) if s.isStreamType =>
            if (s.typeArgs.head != v) {
              error(TypeMismatch(v, s.typeArgs.head, elseCase.loc))
            }
            val liftedThenCase = thenCase.copy(id = liftConstant(thenCase.id, defs, env, thenCase.loc))
            TypedTessla.StaticIfThenElse(cond, liftedThenCase, elseCase, ite.loc) -> s
          case (v1, v2) =>
            if (v1 != v2) {
              error(TypeMismatch(v1, v2, elseCase.loc))
            }
            TypedTessla.StaticIfThenElse(cond, thenCase, elseCase, ite.loc) -> v1
        }
      case call: FlatTessla.MacroCall =>
        typeMap(env(call.macroID)) match {
          case t: TypedTessla.FunctionType =>
            val name = call.macroID.nameOpt.getOrElse("<macro>")
            if (call.args.length != t.parameterTypes.length) {
              throw ArityMismatch(name, t.parameterTypes.length, call.args.length, call.loc)
            }
            val typeArgs = call.typeArgs.map(translateType(_, env))
            if (typeArgs.nonEmpty && typeArgs.length != t.typeParameters.length) {
              throw TypeArityMismatch(name, t.typeParameters.length, call.typeArgs.length, call.loc)
            }
            var macroID = env(call.macroID)
            var possiblyLiftedType = t
            var lastArgs: Seq[TypedTessla.Argument] = Seq()
            if (t.isLiftable && call.args.exists(arg => typeMap(env(arg.id)).isStreamType)) {
              possiblyLiftedType = liftFunctionType(t)
              lastArgs = Seq(TypedTessla.PositionalArgument(macroID, call.macroLoc))
              macroID = findPredef(s"slift${call.args.length}", env, call.macroLoc)
            }
            val typeSubstitutions = mutable.Map(t.typeParameters.zip(typeArgs): _*)
            val typeParams = t.typeParameters.toSet
            val args = call.args.zip(possiblyLiftedType.parameterTypes).map {
              case (arg, genericExpected) =>
                val id = env(arg.id)
                val actual = typeMap(id)
                val expected = typeSubst(genericExpected, actual, typeParams, typeSubstitutions)
                val possiblyLifted =
                  if (isSubtypeOrEqual(parent = expected, child = actual)) {
                    id
                  } else if(streamType(actual) == expected) {
                    liftConstant(id, defs, env, arg.loc)
                  } else {
                    (actual, expected) match {
                      case (a: TypedTessla.FunctionType, e: TypedTessla.FunctionType)
                        if a.isLiftable && isSubtypeOrEqual(parent = e, child = liftFunctionType(a)) =>
                        val sliftId = findPredef(s"slift${a.parameterTypes.length}_curried", env, arg.loc)
                        val typeArgs = a.parameterTypes :+ a.returnType
                        val sliftArgs = Seq(TypedTessla.PositionalArgument(id, arg.loc))
                        val sliftCall = TypedTessla.MacroCall(sliftId, arg.loc, typeArgs, sliftArgs, arg.loc)
                        val liftedId = makeIdentifier()
                        defs.addVariable(TypedTessla.VariableEntry(liftedId, sliftCall, e, Seq(), arg.loc))
                        liftedId
                      case _ =>
                        error(TypeMismatch(expected, actual, arg.loc))
                        id
                    }
                  }
                arg match {
                  case _: FlatTessla.PositionalArgument =>
                    TypedTessla.PositionalArgument(possiblyLifted, arg.loc)
                  case named: FlatTessla.NamedArgument =>
                    TypedTessla.NamedArgument(named.name, TypedTessla.IdLoc(possiblyLifted, named.idLoc.loc), named.loc)
                }
            } ++ lastArgs
            val leftOverTypeParameters = typeParams.diff(typeSubstitutions.keySet)
            if (leftOverTypeParameters.nonEmpty) {
              throw TypeArgumentsNotInferred(name, call.macroLoc)
            }
            val calculatedTypeArgs = t.typeParameters.map(typeSubstitutions)
            val returnType = typeSubst(possiblyLiftedType.returnType, possiblyLiftedType.returnType,
              typeParams, typeSubstitutions)
            val newTypeArgs = if (t.isLiftable && call.args.exists(arg => typeMap(env(arg.id)).isStreamType)) {
              t.parameterTypes.map(x => typeSubst(x, x, typeParams, typeSubstitutions)) :+
                typeSubst(t.returnType, t.returnType, typeParams, typeSubstitutions)
            } else {
              calculatedTypeArgs
            }
            TypedTessla.MacroCall(macroID, call.macroLoc, newTypeArgs, args, call.loc) -> returnType
          case other =>
            throw TypeMismatch("function", other, call.macroLoc)
        }

      case o: FlatTessla.ObjectLiteral =>
        val members = mapValues(o.members)(member => TypedTessla.IdLoc(env(member.id), member.loc))
        val memberTypes = mapValues(members)(member => typeMap(member.id))
        TypedTessla.ObjectLiteral(members, o.loc) -> TypedTessla.ObjectType(memberTypes, isOpen = false)

      case acc: FlatTessla.MemberAccess =>
        val receiver = env(acc.receiver.id)
        typeMap(receiver) match {
          case ot: TypedTessla.ObjectType =>
            val t = ot.memberTypes.getOrElse(acc.member, throw MemberNotDefined(ot, acc.member, acc.memberLoc))
            val ma = TypedTessla.MemberAccess(TypedTessla.IdLoc(receiver, acc.receiver.loc), acc.member, acc.memberLoc, acc.loc)
            ma -> t
          case b @ TypedTessla.BuiltInType(_, Seq(ot: TypedTessla.ObjectType)) if b.isStreamType =>
            val memberType = ot.memberTypes.getOrElse(acc.member, throw MemberNotDefined(ot, acc.member, acc.memberLoc))
            val slift1Id = findPredef(s"slift1", env, acc.loc)
            val macroId = makeIdentifier()
            val param = TypedTessla.Parameter(Tessla.Parameter(Tessla.Identifier("obj", acc.receiver.loc), None), ot, makeIdentifier())
            val macroDefs = new TypedTessla.Definitions(Some(defs))
            macroDefs.addVariable(TypedTessla.VariableEntry(param.id, param, ot, Seq(), acc.receiver.loc))
            val resultId = makeIdentifier()
            val resultExp = TypedTessla.MemberAccess(param.idLoc, acc.member, acc.memberLoc, acc.loc)
            macroDefs.addVariable(TypedTessla.VariableEntry(resultId, resultExp, memberType, Seq(), acc.loc))
            val mac = TypedTessla.Macro(Seq(), Seq(param), macroDefs, memberType, acc.loc,
              TypedTessla.IdLoc(resultId, acc.loc), acc.loc, isLiftable = false)
            defs.addVariable(TypedTessla.VariableEntry(macroId, mac, TypedTessla.FunctionType(Seq(), Seq(ot), memberType, true), Seq(), acc.loc))
            val arg = TypedTessla.PositionalArgument(receiver, acc.receiver.loc)
            val macroArg = TypedTessla.PositionalArgument(macroId, acc.loc)
            val liftedMacroCall = TypedTessla.MacroCall(slift1Id, acc.loc, Seq(ot, memberType), Seq(arg, macroArg), acc.loc)
            liftedMacroCall -> streamType(memberType)
          case other =>
            throw TypeMismatch("object", other, acc.receiver.loc)
        }

      case b: FlatTessla.BuiltInOperator =>
        val t = declaredType.getOrElse(throw MissingTypeAnnotationBuiltIn(b.name, b.loc))
        val typeParameters = t match {
          case ft: TypedTessla.FunctionType => ft.typeParameters
          case _ => Seq()
        }
        val innerEnv = env ++ b.typeParameters.zip(typeParameters) ++ b.parameters.map(_.id).map { id =>
          id -> makeIdentifier(id.nameOpt)
        }
        val parameters = b.parameters.map { p =>
          val t = translateType(p.parameterType, innerEnv)
          TypedTessla.Parameter(p.param, t, innerEnv(p.id))
        }
        val refImpl = b.referenceImplementation.map(env)
        TypedTessla.BuiltInOperator(b.name, typeParameters, parameters, refImpl, b.loc) -> t

      case mac: FlatTessla.Macro =>
        val tvarIDs = declaredType match {
          case Some(f: TypedTessla.FunctionType) =>
            f.typeParameters
          case _ =>
            mac.typeParameters.map(tvar => makeIdentifier(tvar.nameOpt))
        }
        val tvarEnv = mac.typeParameters.zip(tvarIDs).toMap
        val (innerDefs, innerEnv) = translateDefs(mac.body, Some(defs), env ++ tvarEnv)
        val result = TypedTessla.IdLoc(innerEnv(mac.result.id), mac.result.loc)
        val returnType = typeMap(result.id)
        val paramTypes = parameterTypes(mac).map(translateType(_, env ++ tvarEnv))
        val macroType = TypedTessla.FunctionType(tvarIDs, paramTypes, returnType, isLiftable = mac.isLiftable)
        val parameters = mac.parameters.map { p =>
          val t = translateType(p.parameterType, innerEnv)
          TypedTessla.Parameter(p.param, t, innerEnv(p.id))
        }
        if (mac.isLiftable) {
          if (!checkLiftability(macroType)) {
            error(UnliftableMacroType(mac.headerLoc))
          }
        }
        TypedTessla.Macro(tvarIDs, parameters, innerDefs, returnType, mac.headerLoc, result, mac.loc, mac.isLiftable) -> macroType
    }
  }
}

object TypeChecker extends TranslationPhase[FlatTessla.Specification, TypedTessla.TypedSpecification] {
  override def translate(spec: FlatTessla.Specification) = {
    new TypeChecker(spec).translate()
  }
}
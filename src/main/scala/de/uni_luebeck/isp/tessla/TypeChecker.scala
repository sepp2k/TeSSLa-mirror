package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.FlatTessla.VariableEntry

import scala.collection.mutable

class TypeChecker extends TypedTessla.IdentifierFactory with TranslationPhase[FlatTessla.Specification, TypedTessla.Specification] {
  private val typeMap = mutable.Map[TypedTessla.Identifier, TypedTessla.Type]()
  type Env = Map[FlatTessla.Identifier, TypedTessla.Identifier]

  // For each macro of a liftable type, we'll create a lifted version of that macro. This map will map the ID of
  // the unlifted version to that of the lifted version
  private val liftedMacros = mutable.Map[TypedTessla.Identifier, TypedTessla.Identifier]()

  private var stdlibNames: Map[String, FlatTessla.Identifier] = _

  override def translateSpec(spec: FlatTessla.Specification): TypedTessla.Specification = {
    stdlibNames = spec.stdlibNames
    val (scope, env) = translateScopeWithParents(spec.globalScope)
    TypedTessla.Specification(scope, spec.outStreams.map(translateOutStream(_, scope, env)), spec.outAllLocation, stdlibNames.mapValues(env))
  }

  def translateOutStream(stream: FlatTessla.OutStream, scope: TypedTessla.Scope, env: Env): TypedTessla.OutStream = {
    val id = env(stream.id)
    typeMap(id) match {
      case _: TypedTessla.StreamType =>
        TypedTessla.OutStream(id, stream.name, stream.loc)
      case t if t.isValueType =>
        TypedTessla.OutStream(liftConstant(id, scope, env, stream.loc), stream.name, stream.loc)
      case other =>
        throw TypeMismatch("stream or value type", other, stream.loc)
    }
  }

  def processTypeAnnotation(entry: FlatTessla.VariableEntry, env: Env) = declaredType(entry) match {
    case Some(typ) =>
      typeMap(env(entry.id)) = translateType(typ, env)
    case None =>
  }

  def translateType(typ: FlatTessla.Type, env: Env): TypedTessla.Type = typ match {
    case FlatTessla.IntType => TypedTessla.IntType
    case FlatTessla.BoolType => TypedTessla.BoolType
    case FlatTessla.StringType => TypedTessla.StringType
    case FlatTessla.UnitType => TypedTessla.UnitType
    case s: FlatTessla.StreamType =>
      TypedTessla.StreamType(translateType(s.elementType, env))
    case f: FlatTessla.FunctionType =>
      val typeParams = f.typeParameters.map(tvar => makeIdentifier(tvar.nameOpt))
      val innerEnv = env ++ f.typeParameters.zip(typeParams).toMap
      val paramTypes = f.parameterTypes.map(translateType(_, innerEnv))
      TypedTessla.FunctionType(typeParams, paramTypes, translateType(f.returnType, innerEnv), f.isLiftable)
    case s: FlatTessla.SetType =>
      TypedTessla.SetType(translateType(s.elementType, env))
    case m: FlatTessla.MapType =>
      TypedTessla.MapType(keyType = translateType(m.keyType, env), valueType = translateType(m.valueType, env))
    case tvar: FlatTessla.TypeParameter =>
      TypedTessla.TypeParameter(env(tvar.id), tvar.loc)
  }

  def insertInferredType(id: TypedTessla.Identifier, inferredType: TypedTessla.Type, loc: Location) = {
    typeMap.get(id) match {
      case None =>
        typeMap(id) = inferredType
      case Some(declaredType) =>
        if (inferredType != declaredType) {
          throw TypeMismatch(declaredType, inferredType, loc)
        }
    }
  }

  def translateScopeWithParents(scope: FlatTessla.Scope): (TypedTessla.Scope, Env) = {
    scope.parent.map(translateScopeWithParents) match {
      case Some((parentScope, parentEnv)) =>
        translateScope(scope, Some(parentScope), parentEnv)
      case None =>
        translateScope(scope, None, Map())
    }
  }

  def getParameterType(param: FlatTessla.Parameter): FlatTessla.Type = {
    param.parameterType.getOrElse(throw MissingTypeAnnotationParam(param.name, param.loc))
  }

  def parameterTypes(mac: FlatTessla.Macro): Seq[FlatTessla.Type] = {
    mac.parameters.map(getParameterType)
  }

  def declaredType(entry: VariableEntry): Option[FlatTessla.Type] = {
    entry.typeInfo.orElse {
      entry.expression match {
        case m: FlatTessla.Macro =>
          m.returnType.map { returnType =>
            val typeParameters = m.typeParameters
            val paramTypes = parameterTypes(m)
            FlatTessla.FunctionType(typeParameters, paramTypes, returnType, m.isLiftable)
          }
        case _ =>
          entry.typeInfo
      }
    }
  }

  /**
    * Return all the entries that need to be type inferred before the current entry, i.e.
    * all the entries that are used by this entry and do not have an explicit type annotation.
    */
  def requiredEntries(scope: FlatTessla.Scope, entry: FlatTessla.VariableEntry): Seq[FlatTessla.VariableEntry] = {
    requiredEntries(scope, entry.expression)
  }

  def isLiftableMacro(entry: FlatTessla.Expression) = entry match {
    case m: FlatTessla.Macro => m.isLiftable
    case _ => false
  }

  def requiredEntries(scope: FlatTessla.Scope, expression: FlatTessla.Expression): Seq[FlatTessla.VariableEntry] = {
    def resolve(id: FlatTessla.Identifier) = {
      // An entry needs to be processed before this one iff this one uses it and it either has no type annotation or
      // it is a liftable macro (in which case the lifting needs to happen before it is used)
      scope.resolveVariable(id).toList.filter(arg => declaredType(arg).isEmpty || isLiftableMacro(arg.expression))
    }
    expression match {
      case v: FlatTessla.Variable =>
        resolve(v.id)
      case _: FlatTessla.Literal | _: FlatTessla.InputStream | _ : FlatTessla.Parameter
           | _ : FlatTessla.BuiltInOperator =>
        Seq()

      case call: FlatTessla.MacroCall =>
        // Since we invoke requiredEntries* with an outer scope in the macro case (see below), we might encounter
        // identifiers that aren't defined in the scope we see, so we use flatMap to discard the Nones.
        val args = call.args.flatMap(arg => scope.resolveVariable(arg.id)).filterNot(arg => declaredType(arg).isDefined)
        resolve(call.macroID) ++ args

      case mac: FlatTessla.Macro =>
        // Since identifiers used in the macro may either be defined inside or outside the
        // macro (and we only want the outside ones), we use the outer scope for lookup
        // Note that identifiers are unique at this stage, so we won't run into a situation
        // where the macro contains a local identifier that shadows an outer one.
        requiredEntries(scope, mac.body) ++ mac.scope.variables.values.flatMap(requiredEntries(scope, _))
    }
  }

  def translateScope(scope: FlatTessla.Scope, parent: Option[TypedTessla.Scope], parentEnv: Env): (TypedTessla.Scope, Env) = {
    val env = parentEnv ++ scope.variables.mapValues(entry => makeIdentifier(entry.id.nameOpt))
    val resultingScope = new TypedTessla.Scope(parent)
    scope.variables.values.foreach(processTypeAnnotation(_, env))
    var errors: IndexedSeq[TesslaError] = scope.variables.values.flatMap { entry =>
      entry.expression match {
        case m: FlatTessla.Macro =>
          m.parameters.flatMap { param =>
            param.parameterType match {
              case Some(_) => None
              case None => Some(MissingTypeAnnotationParam(param.name, param.loc))
            }
          }
        case _ => None
      }
    }.toIndexedSeq

    if (errors.isEmpty) {
      ReverseTopologicalSort.sort(scope.variables.values)(requiredEntries(scope, _)) match {
        case ReverseTopologicalSort.Cycles(nodesInCycles) =>
          errors ++= nodesInCycles.flatMap { entry =>
            entry.id.nameOpt.map(MissingTypeAnnotationRec(_, entry.loc))
          }.toIndexedSeq
        case ReverseTopologicalSort.Sorted(sorted) =>
          sorted.foreach { entry =>
            resultingScope.addVariable(translateEntry(entry, resultingScope, env))
          }
      }
    }
    if (errors.nonEmpty) {
      // Add all but the last error to the error list and then throw the last.
      // We need to throw one of them, so the execution does not continue (possibly leading to missing key exceptions
      // later), but we also want to record the others. We exclude the last from iteration, so it isn't reported twice
      // (once in the loop and then again when throwing it afterwards)
      errors.init.foreach { err =>
        error(err)
      }
      throw errors.last
    }
    (resultingScope, env)
  }

  def translateEntry(entry: FlatTessla.VariableEntry, scope: TypedTessla.Scope, env: Env): TypedTessla.VariableEntry = {
    val id = env(entry.id)
    val (exp, typ) = translateExpression(entry.expression, typeMap.get(id), Some(id), scope, env)
    insertInferredType(id, typ, exp.loc)
    TypedTessla.VariableEntry(id, exp, typ, entry.loc)
  }

  def typeSubst(expected: TypedTessla.Type, actual: TypedTessla.Type, typeParams: Set[TypedTessla.Identifier],
                substitutions: mutable.Map[TypedTessla.Identifier, TypedTessla.Type]): TypedTessla.Type = {
    (expected, actual) match {
      case (tparam: TypedTessla.TypeParameter, _) =>
        if (typeParams.contains(tparam.id)) {
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
      case (TypedTessla.StreamType(expectedElementType), TypedTessla.StreamType(actualElementType)) =>
        TypedTessla.StreamType(typeSubst(expectedElementType, actualElementType, typeParams, substitutions))
      // Allow for auto-lifting of values
      case (TypedTessla.StreamType(expectedElementType), actualElementType) =>
        TypedTessla.StreamType(typeSubst(expectedElementType, actualElementType, typeParams, substitutions))
      case (TypedTessla.SetType(expectedElementType), TypedTessla.SetType(actualElementType)) =>
        TypedTessla.SetType(typeSubst(expectedElementType, actualElementType, typeParams, substitutions))
      case (TypedTessla.MapType(k, v), TypedTessla.MapType(k2, v2)) =>
        TypedTessla.MapType(typeSubst(k, k2, typeParams, substitutions), typeSubst(v, v2, typeParams, substitutions))
      case _ =>
        expected
    }
  }

  def mkTVar(name: String) = TypedTessla.TypeParameter(makeIdentifier(name), Location.builtIn)

  val typesOfBuiltIns: Map[BuiltIn, TypedTessla.Type] = BuiltIn.builtIns.map {
    case (_, builtIn) =>
      import TypedTessla._
      val typ = builtIn match {
        case BuiltIn.Add | BuiltIn.Sub | BuiltIn.Mul | BuiltIn.Div | BuiltIn.BitAnd | BuiltIn.BitOr | BuiltIn.BitXor
           | BuiltIn.LeftShift | BuiltIn.RightShift =>
          FunctionType(Seq(), Seq(IntType, IntType), IntType, isLiftable = true)

        case BuiltIn.Negate | BuiltIn.BitFlip =>
          FunctionType(Seq(), Seq(IntType), IntType, isLiftable = true)

        case BuiltIn.Lt | BuiltIn.Gt | BuiltIn.Gte | BuiltIn.Lte =>
          FunctionType(Seq(), Seq(IntType, IntType), BoolType, isLiftable = true)

        case BuiltIn.Eq | BuiltIn.Neq =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(t, t), BoolType, isLiftable = true)

        case BuiltIn.And | BuiltIn.Or =>
          FunctionType(Seq(), Seq(BoolType, BoolType), BoolType, isLiftable = true)

        case BuiltIn.Not =>
          FunctionType(Seq(), Seq(BoolType), BoolType, isLiftable = true)

        case BuiltIn.First =>
          val t1 = mkTVar("First")
          val t2 = mkTVar("Second")
          FunctionType(Seq(t1.id, t2.id), Seq(t1, t2), t1, isLiftable = true)

        case BuiltIn.IfThenElse =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(BoolType, t, t), t, isLiftable = true)

        case BuiltIn.IfThen =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(BoolType), StreamType(t)), StreamType(t), isLiftable = false)

        case BuiltIn.Nil =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(), StreamType(t), isLiftable = false)

        case BuiltIn.Default =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t), t), StreamType(t), isLiftable = false)

        case BuiltIn.DefaultFrom | BuiltIn.Merge =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t), StreamType(t)), StreamType(t), isLiftable = false)

        case BuiltIn.Last =>
          val t1 = mkTVar("Value")
          val t2 = mkTVar("Clock")
          FunctionType(Seq(t1.id, t2.id), Seq(StreamType(t1), StreamType(t2)), StreamType(t1), isLiftable = false)

        case BuiltIn.DelayedLast =>
          val t = mkTVar("Value")
          FunctionType(Seq(t.id), Seq(StreamType(t), StreamType(IntType)), StreamType(t), isLiftable = false)

        case BuiltIn.Const =>
          val t1 = mkTVar("Old")
          val t2 = mkTVar("New")
          FunctionType(Seq(t1.id, t2.id), Seq(t1, StreamType(t2)), StreamType(t1), isLiftable = false)

        case BuiltIn.Time =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t)), StreamType(IntType), isLiftable = false)

        case BuiltIn.MapEmpty =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(), MapType(k, v), isLiftable = false)

        case BuiltIn.MapAdd =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v), k, v), MapType(k, v), isLiftable = true)

        case BuiltIn.MapGet =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v), k), v, isLiftable = true)

        case BuiltIn.MapContains =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v), k), BoolType, isLiftable = true)

        case BuiltIn.MapRemove =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v), k), MapType(k, v), isLiftable = true)

        case BuiltIn.SetEmpty =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(), SetType(t), isLiftable = false)

        case BuiltIn.SetAdd =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(SetType(t), t), SetType(t), isLiftable = true)

        case BuiltIn.SetContains =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(SetType(t), t), BoolType, isLiftable = true)

        case BuiltIn.SetRemove =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(SetType(t), t), SetType(t), isLiftable = true)
      }
      builtIn -> typ
  }

  def liftConstant(constant: TypedTessla.Identifier, scope: TypedTessla.Scope, env: Env, loc: Location) = {
    val typeOfConstant = typeMap(constant)
    val streamType = TypedTessla.StreamType(typeOfConstant)
    val liftedId = makeIdentifier()
    val nilCall = TypedTessla.MacroCall(env(stdlibNames("nil")), loc, Seq(typeOfConstant), Seq(), loc)
    val nilId = makeIdentifier("nil")
    val nilEntry = TypedTessla.VariableEntry(nilId, nilCall, streamType, loc)
    scope.addVariable(nilEntry)
    val defaultArgs = Seq(
      TypedTessla.PositionalArgument(nilId, loc),
      TypedTessla.PositionalArgument(constant, loc)
    )
    val defaultCall = TypedTessla.MacroCall(env(stdlibNames("default")), loc, Seq(typeOfConstant), defaultArgs, loc)
    val entry = TypedTessla.VariableEntry(liftedId, defaultCall, streamType, loc)
    scope.addVariable(entry)
    liftedId
  }

  def checkLiftability(functionType: TypedTessla.FunctionType) = {
    functionType.parameterTypes.forall(_.isValueType) && functionType.returnType.isValueType
  }

  def liftFunctionType(functionType: TypedTessla.FunctionType) = {
    TypedTessla.FunctionType(
      functionType.typeParameters,
      functionType.parameterTypes.map(TypedTessla.StreamType),
      TypedTessla.StreamType(functionType.returnType),
      isLiftable = false
    )
  }

  def translateExpression(expression: FlatTessla.Expression, declaredType: Option[TypedTessla.Type],
                          id: Option[TypedTessla.Identifier], scope: TypedTessla.Scope, env: Env)
  : (TypedTessla.Expression, TypedTessla.Type) = {
    expression match {
      case v: FlatTessla.Variable =>
        val id = env(v.id)
        TypedTessla.Variable(id, v.loc) -> typeMap(id)
      case lit: FlatTessla.Literal =>
        val t = lit.value match {
          case _: Tessla.IntLiteral => TypedTessla.IntType
          case _: Tessla.TimeLiteral =>
            // TODO: Implement units of measure, this should contain the appropriate unit
            TypedTessla.IntType
          case Tessla.Unit => TypedTessla.UnitType
          case _: Tessla.BoolLiteral => TypedTessla.BoolType
          case _: Tessla.StringLiteral => TypedTessla.StringType
        }
        TypedTessla.Literal(lit.value, lit.loc) -> t
      case inStream: FlatTessla.InputStream =>
        val typ = translateType(inStream.streamType, env)
        TypedTessla.InputStream(inStream.name, typ, inStream.loc) -> typ
      case param: FlatTessla.Parameter =>
        val id = env(param.id)
        val t = typeMap(id)
        TypedTessla.Parameter(param.param, t, id) -> t
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
            if (t.isLiftable && call.args.exists(arg => !typeMap(env(arg.id)).isValueType)) {
              possiblyLiftedType = liftFunctionType(t)
              macroID = liftedMacros(macroID)
            }
            val typeSubstitutions = mutable.Map(t.typeParameters.zip(typeArgs): _*)
            val typeParams = t.typeParameters.toSet
            val args = call.args.zip(possiblyLiftedType.parameterTypes).map {
              case (arg, genericExpected) =>
                val id = env(arg.id)
                val actual = typeMap(id)
                val expected = typeSubst(genericExpected, actual, typeParams, typeSubstitutions)
                val possiblyLifted =
                  if (actual == expected) {
                    id
                  } else if(TypedTessla.StreamType(actual) == expected) {
                    liftConstant(id, scope, env, arg.loc)
                  } else {
                    (actual, expected) match {
                      case (a: TypedTessla.FunctionType, e: TypedTessla.FunctionType)
                        if a.isLiftable && liftFunctionType(a) == e && liftedMacros.contains(id) =>
                        liftedMacros(id)
                      case _ =>
                        throw TypeMismatch(expected, actual, arg.loc)
                    }
                  }
                arg match {
                  case _: FlatTessla.PositionalArgument =>
                    TypedTessla.PositionalArgument(possiblyLifted, arg.loc)
                  case named: FlatTessla.NamedArgument =>
                    TypedTessla.NamedArgument(named.name, possiblyLifted, named.loc)
                }
            }
            val leftOverTypeParameters = typeParams.diff(typeSubstitutions.keySet)
            if (leftOverTypeParameters.nonEmpty) {
              throw TypeArgumentsNotInferred(name, call.macroLoc)
            }
            val returnType = typeSubst(possiblyLiftedType.returnType, possiblyLiftedType.returnType,
              typeParams, typeSubstitutions)
            TypedTessla.MacroCall(macroID, call.macroLoc, typeArgs, args, call.loc) -> returnType
          case other =>
            throw TypeMismatch("function", other, call.macroLoc)
        }

      case FlatTessla.BuiltInOperator(b) =>
        val t = typesOfBuiltIns(b)
        // Register each builtin as its own lifted version, so things just work when looking up the lifted version
        // of a built-in.
        id.foreach { id =>
          liftedMacros(id) = id
        }
        TypedTessla.BuiltInOperator(b) -> t
      case mac: FlatTessla.Macro =>
        val (tvarIDs, expectedReturnType) = declaredType match {
          case Some(f: TypedTessla.FunctionType) =>
            (f.typeParameters, Some(f.returnType))
          case _ =>
            val ids = mac.typeParameters.map(tvar => makeIdentifier(tvar.nameOpt))
            (ids, None)
        }
        val tvarEnv = mac.typeParameters.zip(tvarIDs).toMap
        val (innerScope, innerEnv) = translateScope(mac.scope, Some(scope), env ++ tvarEnv)
        val (body, returnType) = translateExpression(mac.body, expectedReturnType, None, innerScope, innerEnv)
        val paramTypes = parameterTypes(mac).map(translateType(_, env ++ tvarEnv))
        val macroType = TypedTessla.FunctionType(tvarIDs, paramTypes, returnType, isLiftable = mac.isLiftable)
        val parameters = mac.parameters.map { p =>
          val t = translateType(getParameterType(p), innerEnv)
          TypedTessla.Parameter(p.param, t, innerEnv(p.id))
        }
        val parameterIDs = mac.parameters.map(_.id).toSet
        if (mac.isLiftable) {
          if (!checkLiftability(macroType)) {
            throw UnliftableMacroType(mac.loc)
          }
          val liftedType = liftFunctionType(macroType)
          val liftedScope = new FlatTessla.Scope(mac.scope.parent)
          mac.scope.types.values.foreach { entry =>
            liftedScope.addType(entry)
          }
          mac.scope.variables.values.foreach { entry =>
            if (parameterIDs.contains(entry.id)) {
              liftedScope.addVariable(entry.copy(typeInfo = entry.typeInfo.map(FlatTessla.StreamType)))
            } else {
              liftedScope.addVariable(entry)
            }
          }
          val (innerScope, innerEnv) = translateScope(liftedScope, Some(scope), env ++ tvarEnv)
          val expected = expectedReturnType.map(TypedTessla.StreamType)
          val (body, returnType) = translateExpression(mac.body, expected, None, innerScope, innerEnv)
          val parameters = mac.parameters.map { p =>
            val t = translateType(getParameterType(p), innerEnv)
            TypedTessla.Parameter(p.param, t, innerEnv(p.id))
          }
          // Add a lifted projection operator so that there is a new event whenever any of the parameters get a new
          // event (as per the lift semantics)
          val resultId = makeIdentifier()
          val resultEntry = TypedTessla.VariableEntry(resultId, body, liftedType.returnType, mac.loc)
          innerScope.addVariable(resultEntry)
          val firstParams = (resultId +: parameters.map(_.id)).map(TypedTessla.PositionalArgument(_, body.loc))
          val firstCall = TypedTessla.MacroCall(env(stdlibNames("first")), Location.builtIn, Seq(), firstParams, body.loc)
          val lifted = TypedTessla.Macro(tvarIDs, parameters, innerScope, returnType, firstCall, mac.loc, mac.isLiftable)
          val liftedId = makeIdentifier(id.get.nameOpt)
          val liftedEntry = TypedTessla.VariableEntry(liftedId, lifted, liftedType, mac.loc)
          scope.addVariable(liftedEntry)
          liftedMacros(id.get) = liftedId
        }
        TypedTessla.Macro(tvarIDs, parameters, innerScope, returnType, body, mac.loc, mac.isLiftable) -> macroType
    }
  }
}

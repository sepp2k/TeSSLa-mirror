package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.FlatTessla.VariableEntry
import util.mapValues
import scala.collection.mutable

class TypeChecker(spec: FlatTessla.Specification)
  extends TranslationPhase.Translator[TypedTessla.Specification] with TypedTessla.IdentifierFactory {
  private val typeMap = mutable.Map[TypedTessla.Identifier, TypedTessla.Type]()
  type Env = Map[FlatTessla.Identifier, TypedTessla.Identifier]

  // For each macro of a liftable type, we'll create a lifted version of that macro. This map will map the ID of
  // the unlifted version to that of the lifted version
  private val liftedMacros = mutable.Map[TypedTessla.Identifier, TypedTessla.Identifier]()

  private val stdlibNames: Map[String, FlatTessla.Identifier] = spec.stdlibNames

  override def translateSpec(): TypedTessla.Specification = {
    val (defs, env) = translateDefsWithParents(spec.globalDefs)
    var outputStreams = spec.outStreams.map(translateOutStream(_, defs, env))
    if (spec.outAll) {
      val streams = defs.variables.values.filter(entry => isStreamType(entry.typeInfo))
      outputStreams ++= streams.flatMap { entry =>
        entry.id.nameOpt.map(name => TypedTessla.OutStream(entry.id, Some(name), entry.loc))
      }
    }
    TypedTessla.Specification(defs, outputStreams, spec.outAllLocation, mapValues(stdlibNames)(env))
  }

  def translateOutStream(stream: FlatTessla.OutStream, defs: TypedTessla.Definitions, env: Env): TypedTessla.OutStream = {
    val id = env(stream.id)
    typeMap(id) match {
      case _: TypedTessla.StreamType =>
        TypedTessla.OutStream(id, stream.nameOpt, stream.loc)
      case t if t.isValueType =>
        TypedTessla.OutStream(liftConstant(id, defs, env, stream.loc), stream.nameOpt, stream.loc)
      case other =>
        error(TypeMismatch("stream or value type", other, stream.loc))
        TypedTessla.OutStream(id, Some("<error>"), stream.loc)
    }
  }

  def processTypeAnnotation(entry: FlatTessla.VariableEntry, env: Env) = declaredType(entry) match {
    case Some(typ) =>
      typeMap(env(entry.id)) = translateType(typ, env)
    case None =>
  }

  def translateType(typ: FlatTessla.Type, env: Env): TypedTessla.Type = typ match {
    case FlatTessla.IntType => TypedTessla.IntType
    case FlatTessla.FloatType => TypedTessla.FloatType
    case FlatTessla.BoolType => TypedTessla.BoolType
    case FlatTessla.StringType => TypedTessla.StringType
    case FlatTessla.OptionType(t) => TypedTessla.OptionType(translateType(t, env))
    case FlatTessla.CtfType => TypedTessla.CtfType
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
    case s: FlatTessla.ListType =>
      TypedTessla.ListType(translateType(s.elementType, env))
    case o: FlatTessla.ObjectType =>
      TypedTessla.ObjectType(mapValues(o.memberTypes)(translateType(_, env)), o.isOpen)
    case tvar: FlatTessla.TypeParameter =>
      TypedTessla.TypeParameter(env(tvar.id), tvar.loc)
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

  def isLiftableMacro(entry: FlatTessla.Expression) = entry match {
    case m: FlatTessla.Macro => m.isLiftable
    case _ => false
  }

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
      defs.resolveVariable(id).toList.filter(arg => declaredType(arg).isEmpty || isLiftableMacro(arg.expression))
    }
    expression match {
      case v: FlatTessla.Variable =>
        resolve(v.id)
      case _: FlatTessla.Literal | _: FlatTessla.InputStream | _ : FlatTessla.Parameter
           | _ : FlatTessla.BuiltInOperator =>
        Seq()

      case ite: FlatTessla.StaticIfThenElse =>
        resolve(ite.condition.id) ++ resolve(ite.thenCase.id) ++ resolve(ite.elseCase.id)

      case call: FlatTessla.MacroCall =>
        // Since we invoke requiredEntries* with an outer defs in the macro case (see below), we might encounter
        // identifiers that aren't defined in the defs we see, so we use flatMap to discard the Nones.
        val args = call.args.flatMap(arg => resolve(arg.id))
        resolve(call.macroID) ++ args

      case mac: FlatTessla.Macro =>
        // Since identifiers used in the macro may either be defined inside or outside the
        // macro (and we only want the outside ones), we use the outer defs for lookup
        // Note that identifiers are unique at this stage, so we won't run into a situation
        // where the macro contains a local identifier that shadows an outer one.
        requiredEntries(defs, mac.result) ++ mac.body.variables.values.flatMap(requiredEntries(defs, _))

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
        sorted.foreach { entry =>
          resultingDefs.addVariable(translateEntry(entry, resultingDefs, env))
        }
    }
    (resultingDefs, env)
  }

  def translateEntry(entry: FlatTessla.VariableEntry, defs: TypedTessla.Definitions, env: Env): TypedTessla.VariableEntry = {
    val id = env(entry.id)
    val (exp, typ) = translateExpression(entry.expression, typeMap.get(id), Some(id), defs, env)
    insertInferredType(id, typ, exp.loc)
    TypedTessla.VariableEntry(id, exp, typ, entry.loc)
  }

  def typeSubst(expected: TypedTessla.Type, actual: TypedTessla.Type, typeParams: Set[TypedTessla.Identifier],
                substitutions: mutable.Map[TypedTessla.Identifier, TypedTessla.Type], loc: Location): TypedTessla.Type = {
    (expected, actual) match {
      case (tparam: TypedTessla.TypeParameter, _) =>
        if (!actual.isValueType) {
          error(TypeMismatch("value type", actual, loc))
        }
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
            typeSubst(expectedParamType, actualParamType, typeParams, substitutions, loc)
        }
        val returnType = typeSubst(expectedFunctionType.returnType, actualFunctionType.returnType, typeParams, substitutions, loc)
        TypedTessla.FunctionType(Seq(), parameterTypes, returnType, expectedFunctionType.isLiftable)
      case (TypedTessla.StreamType(expectedElementType), TypedTessla.StreamType(actualElementType)) =>
        TypedTessla.StreamType(typeSubst(expectedElementType, actualElementType, typeParams, substitutions, loc))
      // Allow for auto-lifting of values
      case (TypedTessla.StreamType(expectedElementType), actualElementType) =>
        TypedTessla.StreamType(typeSubst(expectedElementType, actualElementType, typeParams, substitutions, loc))
      case (TypedTessla.SetType(expectedElementType), TypedTessla.SetType(actualElementType)) =>
        TypedTessla.SetType(typeSubst(expectedElementType, actualElementType, typeParams, substitutions, loc))
      case (TypedTessla.ListType(expectedElementType), TypedTessla.ListType(actualElementType)) =>
        TypedTessla.ListType(typeSubst(expectedElementType, actualElementType, typeParams, substitutions, loc))
      case (TypedTessla.OptionType(expectedElementType), TypedTessla.OptionType(actualElementType)) =>
        TypedTessla.OptionType(typeSubst(expectedElementType, actualElementType, typeParams, substitutions, loc))
      case (TypedTessla.MapType(k, v), TypedTessla.MapType(k2, v2)) =>
        TypedTessla.MapType(typeSubst(k, k2, typeParams, substitutions, loc), typeSubst(v, v2, typeParams, substitutions, loc))
      case (expected: TypedTessla.ObjectType, actual: TypedTessla.ObjectType) =>
        val members = expected.memberTypes.map {
          case (name, expectedMemberType) =>
            name -> actual.memberTypes.get(name).map { actualMemberType =>
              typeSubst(expectedMemberType, actualMemberType, typeParams, substitutions, loc)
            }.getOrElse(expectedMemberType)
        }
        TypedTessla.ObjectType(members, expected.isOpen)
      case (TypedTessla.IntType, TypedTessla.IntType)
         | (TypedTessla.FloatType, TypedTessla.FloatType)
         | (TypedTessla.BoolType, TypedTessla.BoolType)
         | (TypedTessla.StringType, TypedTessla.StringType) =>
        expected
      case (left, right) =>
        assert(left.getClass != right.getClass)
        expected
    }
  }

  def mkTVar(name: String) = TypedTessla.TypeParameter(makeIdentifier(name), Location.builtIn)

  val typesOfBuiltIns: Map[BuiltIn, TypedTessla.Type] = BuiltIn.builtIns.map {
    case (_, builtIn) =>
      import TypedTessla._
      val typ = builtIn match {
        case BuiltIn.Add | BuiltIn.Sub | BuiltIn.Mul | BuiltIn.Div | BuiltIn.Mod | BuiltIn.BitAnd | BuiltIn.BitOr
           | BuiltIn.BitXor | BuiltIn.LeftShift | BuiltIn.RightShift =>
          FunctionType(Seq(), Seq(IntType, IntType), IntType, isLiftable = true)

        case BuiltIn.FAdd | BuiltIn.FSub | BuiltIn.FMul | BuiltIn.FDiv | BuiltIn.Pow | BuiltIn.Log =>
          FunctionType(Seq(), Seq(FloatType, FloatType), FloatType, isLiftable = true)

        case BuiltIn.Negate | BuiltIn.BitFlip =>
          FunctionType(Seq(), Seq(IntType), IntType, isLiftable = true)

        case BuiltIn.FNegate =>
          FunctionType(Seq(), Seq(FloatType), FloatType, isLiftable = true)

        case BuiltIn.FloatToInt =>
          FunctionType(Seq(), Seq(FloatType), IntType, isLiftable = true)

        case BuiltIn.IntToFloat =>
          FunctionType(Seq(), Seq(IntType), FloatType, isLiftable = true)

        case BuiltIn.Lt | BuiltIn.Gt | BuiltIn.Gte | BuiltIn.Lte =>
          FunctionType(Seq(), Seq(IntType, IntType), BoolType, isLiftable = true)

        case BuiltIn.FLt | BuiltIn.FGt | BuiltIn.FGte | BuiltIn.FLte =>
          FunctionType(Seq(), Seq(FloatType, FloatType), BoolType, isLiftable = true)

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

        case BuiltIn.Nil =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(), StreamType(t), isLiftable = false)

        case BuiltIn.Default =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t), t), StreamType(t), isLiftable = false)

        case BuiltIn.DefaultFrom | BuiltIn.Merge =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t), StreamType(t)), StreamType(t), isLiftable = false)

        case BuiltIn.Filter =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t), StreamType(BoolType)), StreamType(t), isLiftable = false)

        case BuiltIn.Last =>
          val t1 = mkTVar("Value")
          val t2 = mkTVar("Clock")
          FunctionType(Seq(t1.id, t2.id), Seq(StreamType(t1), StreamType(t2)), StreamType(t1), isLiftable = false)

        case BuiltIn.Delay =>
          val t = mkTVar("Resets")
          val unitType = TypedTessla.ObjectType(Map(), isOpen = false)
          FunctionType(Seq(t.id), Seq(StreamType(IntType), StreamType(t)), StreamType(unitType), isLiftable = false)

        case BuiltIn.Const =>
          val t1 = mkTVar("Old")
          val t2 = mkTVar("New")
          FunctionType(Seq(t1.id, t2.id), Seq(t1, StreamType(t2)), StreamType(t1), isLiftable = false)

        case BuiltIn.Time =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t)), StreamType(IntType), isLiftable = false)

        case BuiltIn.Lift1 =>
          val t = mkTVar("T")
          val u = mkTVar("U")
          val fType = FunctionType(Seq(), Seq(OptionType(t)), OptionType(u), isLiftable = false)
          FunctionType(Seq(t.id, u.id), Seq(StreamType(t), fType), StreamType(u), isLiftable = false)

        case BuiltIn.Lift =>
          val t = mkTVar("T")
          val u = mkTVar("U")
          val v = mkTVar("V")
          val fType = FunctionType(Seq(), Seq(OptionType(t), OptionType(u)), OptionType(v), isLiftable = false)
          FunctionType(Seq(t.id, u.id, v.id), Seq(StreamType(t), StreamType(u), fType), StreamType(v), isLiftable = false)

        case BuiltIn.Lift3 =>
          val t = mkTVar("T")
          val u = mkTVar("U")
          val v = mkTVar("V")
          val w = mkTVar("W")
          val fType = FunctionType(Seq(), Seq(OptionType(t), OptionType(u), OptionType(v)), OptionType(w), isLiftable = false)
          FunctionType(Seq(t.id, u.id, v.id, w.id), Seq(StreamType(t), StreamType(u), StreamType(v), fType), StreamType(w), isLiftable = false)

        case BuiltIn.None =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(), OptionType(t), isLiftable = false)

        case BuiltIn.Some =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(t), OptionType(t), isLiftable = true)

        case BuiltIn.IsNone =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(OptionType(t)), BoolType, isLiftable = true)

        case BuiltIn.GetSome =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(OptionType(t)), t, isLiftable = true)

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

        case BuiltIn.MapSize =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v)), IntType, isLiftable = true)

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

        case BuiltIn.SetSize =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(SetType(t)), IntType, isLiftable = true)

        case BuiltIn.SetUnion | BuiltIn.SetIntersection =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(SetType(t), SetType(t)), SetType(t), isLiftable = true)

        case BuiltIn.SetFold =>
          val a = mkTVar("A")
          val b = mkTVar("B")
          val fType = FunctionType(Seq(), Seq(b, a), b, isLiftable = false)
          FunctionType(Seq(a.id, b.id), Seq(SetType(a), b, fType), b, isLiftable = false)

        case BuiltIn.ListEmpty =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(), ListType(t), isLiftable = true)

        case BuiltIn.ListSize =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(ListType(t)), IntType, isLiftable = true)

        case BuiltIn.ListHead | BuiltIn.ListLast =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(ListType(t)), t, isLiftable = true)

        case BuiltIn.ListPrepend | BuiltIn.ListAppend =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(ListType(t), t), ListType(t), isLiftable = true)

        case BuiltIn.ListTail | BuiltIn.ListInit =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(ListType(t)), ListType(t), isLiftable = true)

        case BuiltIn.ListFold =>
          val a = mkTVar("A")
          val b = mkTVar("B")
          val fType = FunctionType(Seq(), Seq(b, a), b, isLiftable = false)
          FunctionType(Seq(a.id, b.id), Seq(ListType(a), b, fType), b, isLiftable = false)

        case BuiltIn.String_concat =>
          FunctionType(Seq(), Seq(StringType, StringType), StringType, isLiftable = true)

        case BuiltIn.ToString =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(t), StringType, isLiftable = true)

        case BuiltIn.CtfGetInt =>
          FunctionType(Seq(), Seq(CtfType, StringType), IntType, isLiftable = true)

        case BuiltIn.CtfGetString =>
          FunctionType(Seq(), Seq(CtfType, StringType), StringType, isLiftable = true)

        case BuiltIn.TesslaInfo =>
          ObjectType(Map("version" -> StringType), isOpen = false)

        case BuiltIn.StdLibCount =>
          val x = mkTVar("X")
          FunctionType(Seq(x.id), Seq(StreamType(x)), StreamType(IntType), isLiftable = false)
      }
      builtIn -> typ
  }

  def liftConstant(constant: TypedTessla.Identifier, defs: TypedTessla.Definitions, env: Env, loc: Location) = {
    val typeOfConstant = typeMap(constant)
    val streamType = TypedTessla.StreamType(typeOfConstant)
    val liftedId = makeIdentifier()
    val nilCall = TypedTessla.MacroCall(env(stdlibNames("nil")), loc, Seq(typeOfConstant), Seq(), loc)
    val nilId = makeIdentifier()
    val nilEntry = TypedTessla.VariableEntry(nilId, nilCall, streamType, loc)
    defs.addVariable(nilEntry)
    val defaultArgs = Seq(
      TypedTessla.PositionalArgument(nilId, loc),
      TypedTessla.PositionalArgument(constant, loc)
    )
    val defaultCall = TypedTessla.MacroCall(env(stdlibNames("default")), loc, Seq(typeOfConstant), defaultArgs, loc)
    val entry = TypedTessla.VariableEntry(liftedId, defaultCall, streamType, loc)
    defs.addVariable(entry)
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

  def isStreamType(typ: TypedTessla.Type): Boolean = typ match {
    case _: TypedTessla.StreamType => true
    case _ => false
  }

  def isSubtypeOrEqual(parent: TypedTessla.Type, child: TypedTessla.Type): Boolean = (parent, child) match {
    case (parent: TypedTessla.ObjectType, child: TypedTessla.ObjectType) =>
      parent.memberTypes.forall {
        case (name, typ) =>
          child.memberTypes.get(name).exists(childTyp => isSubtypeOrEqual(parent = typ, child = childTyp))
      } && (parent.isOpen || parent.memberTypes.keySet == child.memberTypes.keySet)
    case _ =>
      parent == child
  }

  def translateExpression(expression: FlatTessla.Expression, declaredType: Option[TypedTessla.Type],
                          id: Option[TypedTessla.Identifier], defs: TypedTessla.Definitions, env: Env)
  : (TypedTessla.Expression, TypedTessla.Type) = {
    expression match {
      case v: FlatTessla.Variable =>
        val id = env(v.id)
        TypedTessla.Variable(id, v.loc) -> typeMap(id)
      case lit: FlatTessla.Literal =>
        val t = lit.value match {
          case _: Tessla.IntLiteral => TypedTessla.IntType
          case _: Tessla.FloatLiteral => TypedTessla.FloatType
          case _: Tessla.TimeLiteral =>
            // TODO: Implement units of measure, this should contain the appropriate unit
            TypedTessla.IntType
          case _: Tessla.BoolLiteral => TypedTessla.BoolType
          case _: Tessla.StringLiteral => TypedTessla.StringType
        }
        TypedTessla.Literal(lit.value, lit.loc) -> t
      case inStream: FlatTessla.InputStream =>
        val typ = translateType(inStream.streamType, env)
        if (!isStreamType(typ)) {
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
        if (condType != TypedTessla.BoolType) {
          error(TypeMismatch(TypedTessla.BoolType, condType, cond.loc))
        }
        (typeMap(thenCase.id), typeMap(elseCase.id)) match {
          case (s1: TypedTessla.StreamType, s2: TypedTessla.StreamType) =>
            if (s1 != s2) {
              error(TypeMismatch(s1, s2, elseCase.loc))
            }
            TypedTessla.StaticIfThenElse(cond, thenCase, elseCase, ite.loc) -> s1
          case (s: TypedTessla.StreamType, v) =>
            if (s.elementType != v) {
              error(TypeMismatch(s.elementType, v, elseCase.loc))
            }
            val liftedElseCase = elseCase.copy(id = liftConstant(elseCase.id, defs, env, elseCase.loc))
            TypedTessla.StaticIfThenElse(cond, thenCase, liftedElseCase, ite.loc) -> s
          case (v, s: TypedTessla.StreamType) =>
            if (s.elementType != v) {
              error(TypeMismatch(v, s.elementType, elseCase.loc))
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
            if (t.isLiftable && call.args.exists(arg => isStreamType(typeMap(env(arg.id))))) {
              possiblyLiftedType = liftFunctionType(t)
              macroID = liftedMacros(macroID)
            }
            val typeSubstitutions = mutable.Map(t.typeParameters.zip(typeArgs): _*)
            val typeParams = t.typeParameters.toSet
            val args = call.args.zip(possiblyLiftedType.parameterTypes).map {
              case (arg, genericExpected) =>
                val id = env(arg.id)
                val actual = typeMap(id)
                val expected = typeSubst(genericExpected, actual, typeParams, typeSubstitutions, arg.loc)
                val possiblyLifted =
                  if (isSubtypeOrEqual(parent = expected, child = actual)) {
                    id
                  } else if(TypedTessla.StreamType(actual) == expected) {
                    liftConstant(id, defs, env, arg.loc)
                  } else {
                    (actual, expected) match {
                      case (a: TypedTessla.FunctionType, e: TypedTessla.FunctionType)
                        if a.isLiftable && liftFunctionType(a) == e =>
                        liftedMacros(id)
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
            }
            val leftOverTypeParameters = typeParams.diff(typeSubstitutions.keySet)
            if (leftOverTypeParameters.nonEmpty) {
              throw TypeArgumentsNotInferred(name, call.macroLoc)
            }
            val returnType = typeSubst(possiblyLiftedType.returnType, possiblyLiftedType.returnType,
              typeParams, typeSubstitutions, call.loc)
            TypedTessla.MacroCall(macroID, call.macroLoc, typeArgs, args, call.loc) -> returnType
          case other =>
            throw TypeMismatch("function", other, call.macroLoc)
        }

      case o: FlatTessla.ObjectLiteral =>
        val members = mapValues(o.members)(member => TypedTessla.IdLoc(env(member.id), member.loc))
        val memberTypes = mapValues(members)(member => typeMap(member.id))
        TypedTessla.ObjectLiteral(members, o.loc) -> TypedTessla.ObjectType(memberTypes, isOpen = false)

      case acc: FlatTessla.MemberAccess =>
        val receiver = env(acc.receiver.id)
        val t = typeMap(receiver) match {
          case ot: TypedTessla.ObjectType =>
            ot.memberTypes.getOrElse(acc.member, throw MemberNotDefined(ot, acc.member, acc.memberLoc))
          case TypedTessla.StreamType(ot: TypedTessla.ObjectType) =>
            val memberType = ot.memberTypes.getOrElse(acc.member, throw MemberNotDefined(ot, acc.member, acc.memberLoc))
            TypedTessla.StreamType(memberType)
          case other =>
            throw TypeMismatch("object", other, acc.receiver.loc)
        }
        TypedTessla.MemberAccess(TypedTessla.IdLoc(receiver, acc.receiver.loc), acc.member, acc.memberLoc, acc.loc) -> t

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
        val (innerDefs, innerEnv) = translateDefs(mac.body, Some(defs), env ++ tvarEnv)
        val (body, returnType) = translateExpression(mac.result, expectedReturnType, None, innerDefs, innerEnv)
        val paramTypes = parameterTypes(mac).map(translateType(_, env ++ tvarEnv))
        val macroType = TypedTessla.FunctionType(tvarIDs, paramTypes, returnType, isLiftable = mac.isLiftable)
        val parameters = mac.parameters.map { p =>
          val t = translateType(p.parameterType, innerEnv)
          TypedTessla.Parameter(p.param, t, innerEnv(p.id))
        }
        val parameterIDs = mac.parameters.map(_.id).toSet
        if (mac.isLiftable) {
          if (!checkLiftability(macroType)) {
            error(UnliftableMacroType(mac.headerLoc))
          }
          val liftedType = liftFunctionType(macroType)
          val liftedDefs = new FlatTessla.Definitions(mac.body.parent)
          mac.body.types.values.foreach { entry =>
            liftedDefs.addType(entry)
          }
          mac.body.variables.values.foreach { entry =>
            if (parameterIDs.contains(entry.id)) {
              liftedDefs.addVariable(entry.copy(typeInfo = entry.typeInfo.map(FlatTessla.StreamType)))
            } else {
              liftedDefs.addVariable(entry)
            }
          }
          val (innerDefs, innerEnv) = translateDefs(liftedDefs, Some(defs), env ++ tvarEnv)
          val expected = expectedReturnType.map(TypedTessla.StreamType)
          val (body, returnType) = translateExpression(mac.result, expected, None, innerDefs, innerEnv)
          val parameters = mac.parameters.map { p =>
            val t = translateType(p.parameterType, innerEnv)
            TypedTessla.Parameter(p.param, t, innerEnv(p.id))
          }
          // Add a lifted projection operator so that there is a new event whenever any of the parameters get a new
          // event (as per the lift semantics)
          val resultId = makeIdentifier()
          val resultEntry = TypedTessla.VariableEntry(resultId, body, liftedType.returnType, mac.loc)
          innerDefs.addVariable(resultEntry)
          val firstParams = (resultId +: parameters.map(_.id)).map(TypedTessla.PositionalArgument(_, body.loc))
          val firstCall = TypedTessla.MacroCall(env(stdlibNames("first")), Location.builtIn, Seq(), firstParams, body.loc)
          val lifted = TypedTessla.Macro(tvarIDs, parameters, innerDefs, returnType, mac.headerLoc, firstCall, mac.loc, mac.isLiftable)
          val liftedId = makeIdentifier(id.get.nameOpt)
          val liftedEntry = TypedTessla.VariableEntry(liftedId, lifted, liftedType, mac.loc)
          defs.addVariable(liftedEntry)
          liftedMacros(id.get) = liftedId
        }
        TypedTessla.Macro(tvarIDs, parameters, innerDefs, returnType, mac.headerLoc, body, mac.loc, mac.isLiftable) -> macroType
    }
  }
}

object TypeChecker extends TranslationPhase[FlatTessla.Specification, TypedTessla.Specification] {
  override def translate(spec: FlatTessla.Specification) = {
    new TypeChecker(spec).translate()
  }
}
package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.FlatTessla.VariableEntry

import scala.collection.mutable

class TypeChecker extends FlatTessla.IdentifierFactory with TranslationPhase[FlatTessla.Specification, TypedTessla.Specification] {
  private val typeMap = mutable.Map[TypedTessla.Identifier, TypedTessla.Type]()
  private var stdlibNames: Map[String, FlatTessla.Identifier] = _

  override def translateSpec(spec: FlatTessla.Specification): TypedTessla.Specification = {
    stdlibNames = spec.stdlibNames
    // Initialize ID-generation counter, so that newly created Identifiers won't conflict with old ones
    identifierCounter = spec.idCount
    val scope = translateScopeWithParents(spec.globalScope)
    TypedTessla.Specification(scope, spec.outStreams, spec.outAllLocation, spec.stdlibNames, identifierCounter)
  }

  def processTypeAnnotation(entry: FlatTessla.VariableEntry) = entry.typeInfo match {
    case Some(typ) =>
      typeMap(entry.id) = typ
    case None =>
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

  def translateScopeWithParents(scope: FlatTessla.Scope): TypedTessla.Scope = {
    val parent = scope.parent.map(translateScopeWithParents)
    translateScope(scope, parent)
  }

  def getParameterType(param: FlatTessla.Parameter): TypedTessla.Type = {
    param.parameterType.getOrElse(throw MissingTypeAnnotationParam(param.name, param.loc))
  }

  def parameterTypes(mac: FlatTessla.Macro): Seq[TypedTessla.Type] = {
    mac.parameters.map(getParameterType)
  }

  def declaredType(entry: VariableEntry): Option[TypedTessla.Type] = {
    entry.typeInfo.orElse {
      entry.expression match {
        case m: FlatTessla.Macro =>
          m.returnType.map { returnType =>
            FlatTessla.FunctionType(m.typeParameters, parameterTypes(m), returnType)
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

  def requiredEntries(scope: FlatTessla.Scope, expression: FlatTessla.Expression): Seq[FlatTessla.VariableEntry] = {
    expression match {
      case _: FlatTessla.Variable | _: FlatTessla.Literal | _: FlatTessla.InputStream
           | _ : FlatTessla.Parameter | _ : FlatTessla.BuiltInOperator =>
        Seq()

      case call: FlatTessla.MacroCall =>
        // Since we invoke requiredEntries* with an outer scope in the macro case (see below), we might encounter
        // identifiers that aren't defined in the scope we see, so we use flatMap to discard the Nones.
        call.args.flatMap(arg => scope.resolveVariable(arg.id)).filterNot(arg => declaredType(arg).isDefined)

      case mac: FlatTessla.Macro =>
        // Since identifiers used in the macro may either be defined inside or outside the
        // macro (and we only want the outside ones), we use the outer scope for lookup
        // Note that identifiers are unique at this stage, so we won't run into a situation
        // where the macro contains a local identifier that shadows an outer one.
        requiredEntries(scope, mac.body) ++ mac.scope.variables.values.flatMap(requiredEntries(scope, _))
    }
  }

  def translateScope(scope: FlatTessla.Scope, parent: Option[TypedTessla.Scope]): TypedTessla.Scope = {
    val resultingScope = new TypedTessla.Scope(parent)
    scope.variables.values.foreach(processTypeAnnotation)
    ReverseTopologicalSort.sort(scope.variables.values)(requiredEntries(scope, _)) match {
      case ReverseTopologicalSort.Cycles(nodesInCycles) =>
        nodesInCycles.foreach {
          case (entry) =>
            entry.id.nameOpt match {
              case Some(name) =>
                error(MissingTypeAnnotationRec(name, entry.loc))
              case None =>
            }
        }
      case ReverseTopologicalSort.Sorted(sorted) =>
        sorted.foreach { entry =>
          resultingScope.addVariable(translateEntry(entry, resultingScope))
        }
    }
    resultingScope
  }

  def translateEntry(entry: FlatTessla.VariableEntry, scope: TypedTessla.Scope): TypedTessla.VariableEntry = {
    val (exp, typ) = translateExpression(entry.expression, scope)
    insertInferredType(entry.id, typ, exp.loc)
    TypedTessla.VariableEntry(entry.id, exp, typ, entry.loc)
  }

  def typeSubst(typ: TypedTessla.Type, typeEnv: Map[TypedTessla.Identifier, TypedTessla.Type]): TypedTessla.Type = typ match {
    case tvar: FlatTessla.TypeParameter =>
      typeEnv.getOrElse(tvar.id, throw UndefinedType(tvar.id.nameOpt.getOrElse(tvar.id.toString), tvar.loc))
    case functionType: FlatTessla.FunctionType =>
      typeSubst(functionType, typeEnv)
    case FlatTessla.StreamType(elementType) =>
      FlatTessla.StreamType(typeSubst(elementType, typeEnv))
    case FlatTessla.SetType(elementType) =>
      FlatTessla.SetType(typeSubst(elementType, typeEnv))
    case FlatTessla.MapType(k, v) =>
      FlatTessla.MapType(typeSubst(k, typeEnv), typeSubst(v, typeEnv))
    case FlatTessla.IntType | FlatTessla.BoolType | FlatTessla.StringType | FlatTessla.UnitType =>
      typ
  }

  // Overload for function types to encode the fact that typeSubst on a function type always produces another function
  // type
  def typeSubst(typ: FlatTessla.FunctionType, typeEnv: Map[TypedTessla.Identifier, TypedTessla.Type]): FlatTessla.FunctionType = {
    // Since we don't support higher-order types, i.e. function types that appear as a subtype can't be generic
    // themselves, we know that none of the function types, except the initial one that was used to generated the
    // type environment, will have type parameters, so we don't need to update the type envrionment with new type
    // variables.
    val parameterTypes = typ.parameterTypes.map(typeSubst(_, typeEnv))
    val returnType = typeSubst(typ.returnType, typeEnv)
    FlatTessla.FunctionType(Seq(), parameterTypes, returnType)
  }

  def mkTVar(name: String) = FlatTessla.TypeParameter(makeIdentifier(name), Location.builtIn)

  val typesOfBuiltIns: Map[BuiltIn, TypedTessla.Type] = BuiltIn.builtIns.map {
    case (_, builtIn) =>
      import FlatTessla._
      val typ = builtIn match {
        case BuiltIn.Add | BuiltIn.Sub | BuiltIn.Mul | BuiltIn.Div | BuiltIn.BitAnd | BuiltIn.BitOr | BuiltIn.BitXor
           | BuiltIn.BitFlip | BuiltIn.LeftShift | BuiltIn.RightShift =>
          FunctionType(Seq(), Seq(IntType, IntType), IntType)

        case BuiltIn.Negate =>
          FunctionType(Seq(), Seq(IntType), IntType)

        case BuiltIn.Lt | BuiltIn.Gt | BuiltIn.Gte | BuiltIn.Lte =>
          FunctionType(Seq(), Seq(IntType, IntType), BoolType)

        case BuiltIn.Eq | BuiltIn.Neq =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(t, t), BoolType)

        case BuiltIn.And | BuiltIn.Or =>
          FunctionType(Seq(), Seq(BoolType, BoolType), BoolType)

        case BuiltIn.Not =>
          FunctionType(Seq(), Seq(BoolType), BoolType)

        case BuiltIn.First =>
          val t1 = mkTVar("First")
          val t2 = mkTVar("Second")
          FunctionType(Seq(t1.id, t2.id), Seq(t1, t2), t1)

        case BuiltIn.IfThenElse =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(BoolType, t, t), t)

        case BuiltIn.IfThen =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(BoolType), StreamType(t)), StreamType(t))

        case BuiltIn.Nil =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(), StreamType(t))

        case BuiltIn.Default =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t), t), StreamType(t))

        case BuiltIn.DefaultFrom | BuiltIn.Merge =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t), StreamType(t)), StreamType(t))

        case BuiltIn.Last =>
          val t1 = mkTVar("Value")
          val t2 = mkTVar("Clock")
          FunctionType(Seq(t1.id, t2.id), Seq(StreamType(t1), StreamType(t2)), StreamType(t1))

        case BuiltIn.DelayedLast =>
          val t = mkTVar("Value")
          FunctionType(Seq(t.id), Seq(StreamType(t), StreamType(IntType)), StreamType(t))

        case BuiltIn.Const =>
          val t1 = mkTVar("Old")
          val t2 = mkTVar("New")
          FunctionType(Seq(t1.id, t2.id), Seq(StreamType(t1), t2), StreamType(t2))

        case BuiltIn.Time =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(StreamType(t)), StreamType(IntType))

        case BuiltIn.MapEmpty =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(), MapType(k, v))

        case BuiltIn.MapAdd =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v), k, v), MapType(k, v))

        case BuiltIn.MapGet =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v), k), v)

        case BuiltIn.MapContains =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v), k), BoolType)

        case BuiltIn.MapRemove =>
          val k = mkTVar("Key")
          val v = mkTVar("Value")
          FunctionType(Seq(k.id, v.id), Seq(MapType(k, v), k), MapType(k, v))

        case BuiltIn.SetEmpty =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(), SetType(t))

        case BuiltIn.SetAdd =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(SetType(t), t), SetType(t))

        case BuiltIn.SetContains =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(SetType(t), t), BoolType)

        case BuiltIn.SetRemove =>
          val t = mkTVar("T")
          FunctionType(Seq(t.id), Seq(SetType(t), t), SetType(t))
      }
      builtIn -> typ
  }

  def liftConstant(constant: FlatTessla.Identifier, scope: TypedTessla.Scope, loc: Location) = {
    val typeOfConstant = typeMap(constant)
    val streamType = FlatTessla.StreamType(typeOfConstant)
    val liftedId = makeIdentifier()
    val nilCall = TypedTessla.MacroCall(stdlibNames("nil"), loc, Seq(typeOfConstant), Seq(), loc)
    val nilId = makeIdentifier("nil")
    val nilEntry = TypedTessla.VariableEntry(nilId, nilCall, streamType, loc)
    scope.addVariable(nilEntry)
    val defaultArgs = Seq(
      FlatTessla.PositionalArgument(nilId, loc),
      FlatTessla.PositionalArgument(constant, loc)
    )
    val defaultCall = TypedTessla.MacroCall(stdlibNames("default"), loc, Seq(typeOfConstant), defaultArgs, loc)
    val entry = TypedTessla.VariableEntry(liftedId, defaultCall, streamType, loc)
    scope.addVariable(entry)
    liftedId
  }

  def translateExpression(expression: FlatTessla.Expression, scope: TypedTessla.Scope): (TypedTessla.Expression, TypedTessla.Type) = {
    expression match {
      case v: FlatTessla.Variable =>
        TypedTessla.Variable(v.id, v.loc) -> typeMap(v.id)
      case lit: FlatTessla.Literal =>
        val t = lit.value match {
          case _: Tessla.IntLiteral => FlatTessla.IntType
          case _: Tessla.TimeLiteral =>
            // TODO: Implement units of measure, this should contain the appropriate unit
            FlatTessla.IntType
          case Tessla.Unit => FlatTessla.UnitType
          case _: Tessla.BoolLiteral => FlatTessla.BoolType
          case _: Tessla.StringLiteral => FlatTessla.StringType
        }
        TypedTessla.Literal(lit.value, lit.loc) -> t
      case inStream: FlatTessla.InputStream =>
        TypedTessla.InputStream(inStream.name, inStream.streamType, inStream.loc) -> inStream.streamType
      case param: FlatTessla.Parameter =>
        val t = getParameterType(param)
        TypedTessla.Parameter(param.param, t, param.id) -> t
      case call: FlatTessla.MacroCall =>
        typeMap(call.macroID) match {
          case t: FlatTessla.FunctionType =>
            val name = call.macroID.nameOpt.getOrElse("<macro>")
            if (call.args.length != t.parameterTypes.length) {
              throw ArityMismatch(name, t.parameterTypes.length, call.args.length, call.loc)
            }
            if (call.typeArgs.length != t.typeParameters.length) {
              throw TypeArityMismatch(name, t.typeParameters.length, call.typeArgs.length, call.loc)
            }
            val typeArgs = call.typeArgs
            val typeEnv = t.typeParameters.zip(typeArgs).toMap
            val concreteType = typeSubst(t, typeEnv)
            val args = call.args.zip(concreteType.parameterTypes).map {
              case (arg, expected) =>
                val actual = typeMap(arg.id)
                if (actual == expected) {
                  arg
                } else if(FlatTessla.StreamType(actual) == expected) {
                  val lifted = liftConstant(arg.id, scope, arg.loc)
                  arg match {
                    case _: FlatTessla.PositionalArgument => FlatTessla.PositionalArgument(lifted, arg.loc)
                    case named: FlatTessla.NamedArgument => FlatTessla.NamedArgument(named.name, lifted, named.loc)
                  }
                } else {
                  throw TypeMismatch(expected, actual, arg.loc)
                }
            }
            TypedTessla.MacroCall(call.macroID, call.macroLoc, typeArgs, args, call.loc) -> concreteType.returnType
          case other =>
            throw TypeMismatch("function", other, call.macroLoc)
        }

      case FlatTessla.BuiltInOperator(b) =>
        TypedTessla.BuiltInOperator(b) -> typesOfBuiltIns(b)
      case mac: FlatTessla.Macro =>
        val parameters = mac.parameters.map(p => TypedTessla.Parameter(p.param, getParameterType(p), p.id))
        val innerScope = translateScope(mac.scope, Some(scope))
        val (body, returnType) = translateExpression(mac.body, innerScope)
        val macroType = FlatTessla.FunctionType(mac.typeParameters, parameterTypes(mac), returnType)
        TypedTessla.Macro(mac.typeParameters, parameters, innerScope, returnType, body, mac.loc) -> macroType
    }
  }
}

package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.FlatTessla.VariableEntry

import scala.collection.mutable

class TypeChecker extends TranslationPhase[FlatTessla.Specification, TypedTessla.Specification] {
  val typeMap = mutable.Map[TypedTessla.Identifier, TypedTessla.Type]()

  override def translateSpec(spec: FlatTessla.Specification): TypedTessla.Specification = {
    val scope = translateScopeWithParents(spec.globalScope)
    TypedTessla.Specification(scope, spec.outStreams, spec.outAllLocation)
  }

  @Deprecated
  def placeholderType = FlatTessla.StreamType(FlatTessla.IntType) // TODO: Properly determine type

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
      case FlatTessla.Nil| _: FlatTessla.Variable | _: FlatTessla.Literal | _: FlatTessla.InputStream
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
      case ReverseTopologicalSort.Cycles(cycleStarts) =>
        cycleStarts.foreach {
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
      FlatTessla.MapType(typeSubst(k, typeEnv), typeSubst(k, typeEnv))
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

  def translateExpression(expression: FlatTessla.Expression, scope: TypedTessla.Scope): (TypedTessla.Expression, TypedTessla.Type) = {
    expression match {
      case FlatTessla.Nil =>
        TypedTessla.Nil -> placeholderType
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
            call.args.zip(concreteType.parameterTypes).foreach {
              case (arg, expected) =>
                val actual = typeMap(arg.id)
                if (actual != expected) {
                  throw TypeMismatch(expected, actual, arg.loc)
                }
            }
            TypedTessla.MacroCall(call.macroID, call.macroLoc, typeArgs, call.args, call.loc) -> concreteType.returnType
          case other =>
            throw TypeMismatch("function", other, call.macroLoc)
        }

      case FlatTessla.BuiltInOperator(b) =>
        TypedTessla.BuiltInOperator(b) -> placeholderType
      case mac: FlatTessla.Macro =>
        val parameters = mac.parameters.map(p => TypedTessla.Parameter(p.param, getParameterType(p), p.id))
        val innerScope = translateScope(mac.scope, Some(scope))
        val (body, returnType) = translateExpression(mac.body, innerScope)
        // TODO: Add type parameters to type environment
        val macroType = FlatTessla.FunctionType(mac.typeParameters, parameterTypes(mac), returnType)
        TypedTessla.Macro(mac.typeParameters, parameters, innerScope, returnType, body, mac.loc) -> macroType
    }
  }
}

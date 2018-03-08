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
  def placeholderType = TypedTessla.StreamType(TypedTessla.IntType) // TODO: Properly determine type

  def translateType(typ: FlatTessla.Type): TypedTessla.Type = typ match {
    case Tessla.SimpleType(id) =>
      id.name match {
        case "Int" => TypedTessla.IntType
        case "Bool" => TypedTessla.BoolType
        case "Unit" => TypedTessla.UnitType
        case "String" => TypedTessla.StringType
        case "TimeSpan" => TypedTessla.TimeSpanType
        case name => throw UndefinedType(name, id.loc)
      }
    case Tessla.TypeApplication(id, typeArgs, loc) =>
      val translatedArgs = typeArgs.map(translateType)
      (id.name, translatedArgs) match {
        case ("Events", Seq(typeArg)) =>
          if (typeArg.isValueType) {
            TypedTessla.StreamType(typeArg)
          } else {
            throw StreamOfStreams(loc)
          }
        case ("Events", args) =>
          throw TypeArityMismatch("Events", 1, args.length, loc)
        case (name, _) =>
          throw UndefinedTypeConstructor(name, id.loc)
      }
  }

  def processTypeAnnotation(entry: FlatTessla.VariableEntry) = entry.typeInfo match {
    case Some(typ) =>
      typeMap(entry.id) = translateType(typ)
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

  def parameterTypes(mac: FlatTessla.Macro): Seq[TypedTessla.Type] = {
    mac.parameters.map { param =>
      param.parameterType.map(translateType).getOrElse(throw MissingTypeAnnotationParam(param.name, param.loc))
    }
  }

  def declaredType(entry: VariableEntry): Option[TypedTessla.Type] = {
    entry.typeInfo.map(translateType).orElse {
      entry.expression match {
        case m: FlatTessla.Macro =>
          m.returnType.map { returnType =>
            TypedTessla.FunctionType(m.typeParameters, parameterTypes(m), translateType(returnType))
          }
        case _ =>
          entry.typeInfo.map(translateType)
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

  def translateExpression(expression: FlatTessla.Expression, scope: TypedTessla.Scope): (TypedTessla.Expression, TypedTessla.Type) = {
    expression match {
      case FlatTessla.Nil =>
        TypedTessla.Nil -> placeholderType
      case v: FlatTessla.Variable =>
        TypedTessla.Variable(v.id, v.loc) -> typeMap(v.id)
      case lit: FlatTessla.Literal =>
        val t = lit.value match {
          case _: Tessla.IntLiteral => TypedTessla.IntType
          case _: Tessla.TimeSpanLiteral => TypedTessla.TimeSpanType
          case Tessla.Unit => TypedTessla.UnitType
          case _: Tessla.BoolLiteral => TypedTessla.BoolType
          case _: Tessla.StringLiteral => TypedTessla.StringType
        }
        TypedTessla.Literal(lit.value, lit.loc) -> t
      case inStream: FlatTessla.InputStream =>
        val t = translateType(inStream.streamType)
        TypedTessla.InputStream(inStream.name, t, inStream.loc) -> t
      case param: FlatTessla.Parameter =>
        param.parameterType match {
          case Some(t) =>
            TypedTessla.Parameter(param.param, param.id) -> translateType(t)
          case None =>
            throw MissingTypeAnnotationParam(param.name, param.loc)
        }
      case call: FlatTessla.MacroCall =>
        typeMap(call.macroID) match {
          case t: TypedTessla.FunctionType =>
            if (call.args.length != t.parameterTypes.length) {
              val name = call.macroID.nameOpt.getOrElse("<macro>")
              throw ArityMismatch(name, t.parameterTypes.length, call.args.length, call.loc)
            }
            call.args.zip(t.parameterTypes).foreach {
              case (arg, expected) =>
                val actual = typeMap(arg.id)
                if (actual != expected) {
                  throw TypeMismatch(expected, actual, arg.loc)
                }
            }
            TypedTessla.MacroCall(call.macroID, call.macroLoc, call.args, call.loc) -> t.returnType
          case other =>
            throw TypeMismatch("function", other, call.macroLoc)
        }

      case FlatTessla.BuiltInOperator(b) =>
        TypedTessla.BuiltInOperator(b) -> placeholderType
      case mac: FlatTessla.Macro =>
        val parameters = mac.parameters.map(p => TypedTessla.Parameter(p.param, p.id))
        val innerScope = translateScope(mac.scope, Some(scope))
        val (body, returnType) = translateExpression(mac.body, innerScope)
        // TODO: Add type parameters to type environment
        val macroType = TypedTessla.FunctionType(mac.typeParameters, parameterTypes(mac), returnType)
        TypedTessla.Macro(mac.typeParameters, parameters, innerScope, returnType, body, mac.loc) -> macroType
    }
  }
}

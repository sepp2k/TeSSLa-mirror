package de.uni_luebeck.isp.tessla

class TypeChecker extends TranslationPhase[FlatTessla.Specification, TypedTessla.Specification] {
  override def translateSpec(spec: FlatTessla.Specification): TypedTessla.Specification = {
    val scope = translateScopeWithParents(spec.globalScope)
    TypedTessla.Specification(scope, spec.outStreams, spec.outAllLocation)
  }

  @Deprecated
  def placeholderType = TypedTessla.StreamType(TypedTessla.IntType) // TODO: Properly determine type

  def translateScope(scope: FlatTessla.Scope, parent: Option[TypedTessla.Scope]): TypedTessla.Scope = {
    val result = new TypedTessla.Scope(parent)
    scope.variables.foreach {
      case (name, entry) =>
        val (exp, typ) = translateExpression(entry.expression, result)
        result.addVariable(name, TypedTessla.VariableEntry(exp, typ))
    }
    result
  }

  def translateScopeWithParents(scope: FlatTessla.Scope): TypedTessla.Scope = {
    val parent = scope.parent.map(translateScopeWithParents)
    translateScope(scope, parent)
  }

  def translateExpression(expression: FlatTessla.Expression,
                          scope: TypedTessla.Scope): (TypedTessla.Expression, TypedTessla.Type) = {
    expression match {
      case FlatTessla.Nil =>
        TypedTessla.Nil -> placeholderType
      case v: FlatTessla.Variable =>
        TypedTessla.Variable(v.id, v.loc) -> placeholderType
      case lit: FlatTessla.Literal =>
        TypedTessla.Literal(lit.value, lit.loc) -> placeholderType
      case inStream: FlatTessla.InputStream =>
        TypedTessla.InputStream(inStream.name, placeholderType, inStream.loc) -> placeholderType
      case param: FlatTessla.Parameter =>
        TypedTessla.Parameter(param.param, param.id) -> placeholderType
      case call: FlatTessla.MacroCall =>
        TypedTessla.MacroCall(call.macroID, call.macroLoc, call.args, call.loc) -> placeholderType
      case FlatTessla.BuiltInOperator(b) =>
        TypedTessla.BuiltInOperator(b) -> placeholderType
      case mac: FlatTessla.Macro =>
        val parameters = mac.parameters.map(p => TypedTessla.Parameter(p.param, p.id))
        val innerScope = translateScope(mac.scope, Some(scope))
        val (body, returnType) = translateExpression(mac.body, innerScope)
        TypedTessla.Macro(mac.typeParameters, parameters, innerScope, returnType, body, mac.loc) -> placeholderType
    }
  }
}

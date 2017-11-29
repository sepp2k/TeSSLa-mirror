package de.uni_luebeck.isp.tessla

class TypeChecker extends TranslationPhase[FlatTessla.Specification, TypedTessla.Specification] {
  override def translateSpec(spec: FlatTessla.Specification): TypedTessla.Specification = {
    val scope = translateScope(spec.globalScope, None)
    TypedTessla.Specification(scope, spec.outStreams.map(translateOutStream(_, scope)), spec.outAllLocation)
  }

  def translateScope(scope: FlatTessla.Scope, parent: Option[TypedTessla.Scope]): TypedTessla.Scope = {
    val result = new TypedTessla.Scope(parent)
    scope.variables.foreach {
      case (name, entry) =>
        val exp = translateExpression(entry.expression, result)
        val typ = TypedTessla.UnknownType
        result.addVariable(name, TypedTessla.VariableEntry(exp, typ))
    }
    result
  }

  def translateOutStream(outStream: FlatTessla.OutStream, scope: TypedTessla.Scope): TypedTessla.OutStream = {
    TypedTessla.OutStream(translateExpression(outStream.expr, scope), outStream.name, outStream.loc)
  }

  def translateExpression(expression: FlatTessla.Expression, scope: TypedTessla.Scope): TypedTessla.Expression = expression match {
    case v: FlatTessla.Variable => TypedTessla.Variable(v.id, v.loc)
    case lit: FlatTessla.Literal => TypedTessla.Literal(lit.value, lit.loc)
    case inStream: FlatTessla.InStream => TypedTessla.InStream(inStream.name, TypedTessla.UnknownType, inStream.loc)
    case param: FlatTessla.Parameter => TypedTessla.Parameter(param.param)
    case call: FlatTessla.MacroCall =>
      TypedTessla.MacroCall(call.macroID, call.args, call.loc)
    case FlatTessla.BuiltInOperator(b) => TypedTessla.BuiltInOperator(b)
    case mac: FlatTessla.Macro =>
      val parameters = mac.parameters.map(p => TypedTessla.Parameter(p.param))
      val innerScope = translateScope(mac.scope, Some(scope))
      val returnType = TypedTessla.UnknownType
      val body = translateExpression(mac.body, innerScope)
      TypedTessla.Macro(mac.typeParameters, parameters, innerScope, returnType, body, mac.loc)
  }
}

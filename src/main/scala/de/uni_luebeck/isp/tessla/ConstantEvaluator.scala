package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{UndefinedNamedArg, UndefinedTimeUnit}
import de.uni_luebeck.isp.tessla.util.Lazy

import scala.collection.mutable

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TesslaCore.IdentifierFactory with TranslationPhase[TypedTessla.Specification, TesslaCore.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntryWrapper]

  class EnvEntryWrapper(var entry: EnvEntry)

  sealed abstract class EnvEntry
  case class StreamEntry(stream: TesslaCore.StreamRef) extends EnvEntry
  case class ValueEntry(value: Lazy[TesslaCore.Value]) extends EnvEntry
  case class MacroEntry(mac: TypedTessla.Macro) extends EnvEntry
  case class BuiltInEntry(builtIn: FlatTessla.BuiltIn) extends EnvEntry
  case object Translating extends EnvEntry
  case class NotYetTranslated(entry: TypedTessla.VariableEntry) extends EnvEntry

  val translatedStreams = mutable.Map[TesslaCore.Identifier, TesslaCore.StreamDefinition]()

  override def translateSpec(spec: TypedTessla.Specification): TesslaCore.Specification = {
    val env = createEnvForScopeWithParents(spec.globalScope)
    translateEnv(env)
    TesslaCore.Specification(translatedStreams.toMap, ???, ???)
  }

  def createEnvForScope(scope: TypedTessla.Scope): Env = {
    scope.variables.mapValues(entry => new EnvEntryWrapper(NotYetTranslated(entry))).toMap
  }

  def createEnvForScopeWithParents(scope: TypedTessla.Scope): Env = {
    val outerEnv = scope.parent.map(createEnvForScopeWithParents).getOrElse(Map())
    val innerEnv = createEnvForScope(scope)
    outerEnv ++ innerEnv
  }

  def translateEnv(env: Env): Unit = {
    env.foreach {
      case (_, entryWrapper) =>
        translateEntry(env, entryWrapper)
    }
  }

  def translateEntry(env: Env, wrapper: EnvEntryWrapper): Unit = wrapper.entry match {
    case NotYetTranslated(entry) =>
      wrapper.entry = Translating
      wrapper.entry = translateExpression(env, entry.expression)
    case Translating =>
      // TODO Proper loc with stack trace
      throw Errors.InfiniteRecursion(Location.unknown)
    case _ =>
      /* do nothing */
  }

  def translateType(typ: TypedTessla.Type) = typ match {
    case TypedTessla.UnknownType =>
      // TODO: Placeholder. Will be replaced once TypedTessla has proper types
      TesslaCore.IntType
  }

  def translateLiteral(literal: Tessla.LiteralValue, loc: Location): TesslaCore.Value = literal match {
    case Tessla.IntLiteral(x) =>
      TesslaCore.IntLiteral(x, loc)
    case Tessla.TimeLiteral(x, tu) =>
      baseTimeUnit match {
        case Some(base) =>
          TesslaCore.IntLiteral(tu.convertTo(base) * x, loc)
        case None =>
          throw UndefinedTimeUnit(tu.loc)
      }
    case Tessla.StringLiteral(str) =>
      TesslaCore.StringLiteral(str, loc)
    case Tessla.BoolLiteral(bool) =>
      TesslaCore.BoolLiteral(bool, loc)
    case Tessla.Unit =>
      TesslaCore.Unit(loc)
  }

  def translateExpression(env: Env, expression: TypedTessla.Expression): EnvEntry = expression match {
    case TypedTessla.BuiltInOperator(builtIn) => BuiltInEntry(builtIn)
    case mac: TypedTessla.Macro => MacroEntry(mac)
    case TypedTessla.Literal(lit, loc) =>
      // Evaluate the literal outside of the Lazy because we want TimeUnit-errors to appear even if
      // the expression is not used
      // TimeUnit-errors inside of macros *are* swallowed if the macro is never called, which is what
      // we want because a library might define macros using timeunits and, as long as you don't call
      // those macros, you should still be able to use the library when you don't have a time unit set.
      val translatedLit = translateLiteral(lit, loc)
      ValueEntry(Lazy(translatedLit))
    case p: TypedTessla.Parameter =>
      translateVar(env, p.id)
    case v: TypedTessla.Variable =>
      translateVar(env, v.id)
    case i: TypedTessla.InputStream =>
      StreamEntry(TesslaCore.InputStream(i.name, i.loc))
    case call: TypedTessla.MacroCall =>
      translateVar(env, call.macroID) match {
        case BuiltInEntry(builtIn) =>
          val args = call.args.map {
            case FlatTessla.PositionalArgument(id, loc) =>
              translateVar(env, id)
            case FlatTessla.NamedArgument(name, _, loc) =>
              throw UndefinedNamedArg(name, loc)
          }
          ???
        case MacroEntry(mac) => ???
      }
  }

  def translateVar(env: Env, id: TypedTessla.Identifier): EnvEntry = {
    translateEntry(env, env(id))
    env(id).entry
  }
}

package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{DivideByZero, InternalError, UndefinedTimeUnit}
import de.uni_luebeck.isp.tessla.util.Lazy

import scala.collection.mutable
import ConstantEvaluator._

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TesslaCore.IdentifierFactory with TranslationPhase[TypedTessla.Specification, TesslaCore.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntryWrapper]

  class EnvEntryWrapper(var entry: EnvEntry)

  sealed abstract class EnvEntry
  case class StreamEntry(stream: TesslaCore.StreamRef) extends EnvEntry
  case class ValueEntry(value: Lazy[TesslaCore.Value]) extends EnvEntry
  case class MacroEntry(mac: TypedTessla.Macro) extends EnvEntry
  case class BuiltInEntry(builtIn: BuiltIn) extends EnvEntry
  case object Translating extends EnvEntry
  case class NotYetTranslated(entry: TypedTessla.VariableEntry) extends EnvEntry

  val translatedStreams = mutable.Map[TesslaCore.Identifier, TesslaCore.StreamDefinition]()

  override def translateSpec(spec: TypedTessla.Specification): TesslaCore.Specification = {
    val env = createEnvForScopeWithParents(spec.globalScope)
    translateEnv(env, None)
    def inputStreams = spec.globalScope.variables.collect {
      case (_, TypedTessla.VariableEntry(is: TypedTessla.InputStream, typ)) =>
        (is.name, translateStreamType(typ), is.loc)
    }.toSeq
    def outputStreams = spec.outStreams.map { os =>
      (os.name, getStream(env(os.id).entry))
    }
    TesslaCore.Specification(translatedStreams.toMap, inputStreams, outputStreams)
  }

  def createEnvForScope(scope: TypedTessla.Scope): Env = {
    scope.variables.mapValues(entry => new EnvEntryWrapper(NotYetTranslated(entry))).toMap
  }

  def createEnvForScopeWithParents(scope: TypedTessla.Scope): Env = {
    val outerEnv = scope.parent.map(createEnvForScopeWithParents).getOrElse(Map())
    val innerEnv = createEnvForScope(scope)
    outerEnv ++ innerEnv
  }

  def translateEnv(env: Env, overrideLoc: Option[Location]): Unit = {
    env.foreach {
      case (_, entryWrapper) =>
        translateEntry(env, entryWrapper, overrideLoc)
    }
  }

  def translateEntry(env: Env, wrapper: EnvEntryWrapper, overrideLoc: Option[Location]): Unit = wrapper.entry match {
    case NotYetTranslated(entry) =>
      wrapper.entry = Translating
      wrapper.entry = translateExpression(env, entry.expression, entry.typeInfo, overrideLoc)
    case Translating =>
      // TODO Proper loc with stack trace
      throw Errors.InfiniteRecursion(Location.unknown)
    case _ =>
      /* do nothing */
  }

  def translateStreamType(typ: TypedTessla.Type) = typ match {
    case TypedTessla.StreamType(elementType) =>
      TesslaCore.StreamType(translateValueType(elementType))
    case _ =>
      throw InternalError(s"Expected stream type, got $typ - should have been caught by type checker")
  }

  def translateValueType(typ: TypedTessla.Type) = typ match {
    case TypedTessla.IntType => TesslaCore.IntType
    // Times are just seen as ints in tessla core
    case TypedTessla.TimeType => TesslaCore.IntType
    case TypedTessla.StringType => TesslaCore.StringType
    case TypedTessla.BoolType => TesslaCore.BoolType
    case TypedTessla.UnitType => TesslaCore.UnitType
    case _ : TypedTessla.FunctionType =>
      throw InternalError(s"Function type where value type expected - should have been caught by type checker")
    case TypedTessla.StreamType(_) =>
      throw InternalError(s"Stream type where value type expected - should have been caught by type checker")
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

  /**
    * @param overrideLoc If a definition is inlined, this is the location of the original variable use, so the locs
    *                    don't point to the expression that the variable refers to, but to where the variable used to
    *                    be.
    */
  def translateExpression(env: Env, expression: TypedTessla.Expression, typ: TypedTessla.Type, overrideLoc: Option[Location]): EnvEntry = expression match {
    case TypedTessla.Nil =>
      // Nil should only appear directly in the definition of the built-in "nil" and always be referred to through
      // the "nil" variable otherwise. So the location we set here, will only be used for the built-in definition
      // and be overridden by the variable's loc otherwise
      StreamEntry(TesslaCore.Nil(overrideLoc.getOrElse(Location.builtIn)))
    case TypedTessla.BuiltInOperator(builtIn) => BuiltInEntry(builtIn)
    case mac: TypedTessla.Macro => MacroEntry(mac)
    case TypedTessla.Literal(lit, loc) =>
      // Evaluate the literal outside of the Lazy because we want TimeUnit-errors to appear even if
      // the expression is not used
      // TimeUnit-errors inside of macros *are* swallowed if the macro is never called, which is what
      // we want because a library might define macros using time units and, as long as you don't call
      // those macros, you should still be able to use the library when you don't have a time unit set.
      val translatedLit = translateLiteral(lit, overrideLoc.getOrElse(loc))
      ValueEntry(Lazy(translatedLit))
    case p: TypedTessla.Parameter =>
      translateVar(env, p.id, p.loc)
    case v: TypedTessla.Variable =>
      translateVar(env, v.id, v.loc)
    case i: TypedTessla.InputStream =>
      StreamEntry(TesslaCore.InputStream(i.name, overrideLoc.getOrElse(i.loc)))
    case call: TypedTessla.MacroCall =>
      // No checks regarding arity or non-existing or duplicate named arguments because those mistakes
      // would be caught by the type checker
      translateVar(env, call.macroID, call.macroLoc) match {
        case BuiltInEntry(builtIn) =>
          val args = call.args.map {
            case FlatTessla.PositionalArgument(id, loc) =>
              translateVar(env, id, loc)
            case FlatTessla.NamedArgument(name, _, loc) =>
              throw InternalError(s"Undefined keyword arg $name for built-in should've been caught by type checker", loc)
          }
          translateBuiltIn(builtIn, args, typ, call.loc)
        case MacroEntry(mac) =>
          var posArgIdx = 0
          val args = call.args.map {
            case FlatTessla.PositionalArgument(id, _) =>
              val param = mac.parameters(posArgIdx)
              posArgIdx += 1
              param -> translateVar(env, id, param.loc)
            case FlatTessla.NamedArgument(name, id, loc) =>
              mac.parameters.find(_.name == name).get -> translateVar(env, id, loc)
          }.toMap
          translateMacro(mac, args, call.loc)
        case _ =>
          throw InternalError("Applying non-macro/builtin - should have been caught by the type checker.")
      }
  }

  def getStream(envEntry: EnvEntry) = envEntry match {
    case StreamEntry(s) => s
    case other => throw InternalError(s"Wrong type of environment entry: Expected StreamEntry, found: $other")
  }

  def getValue(envEntry: EnvEntry): TesslaCore.ValueOrError = envEntry match {
    case ValueEntry(s) => TesslaCore.ValueOrError.fromLazy(s)
    case other => throw InternalError(s"Wrong type of environment entry: Expected ValueEntry, found: $other")
  }

  def translateBuiltIn(builtIn: BuiltIn, arguments: Seq[EnvEntry], typ: TypedTessla.Type, loc: Location): EnvEntry = {
    def stream(coreExp: TesslaCore.Expression) = {
      val id = makeIdentifier()
      translatedStreams(id) = TesslaCore.StreamDefinition(coreExp, translateStreamType(typ))
      StreamEntry(TesslaCore.Stream(id, loc))
    }
    builtIn match {
      case BuiltIn.Default => stream(TesslaCore.Default(getStream(arguments(0)), getValue(arguments(1)), loc))
      case BuiltIn.DefaultFrom => stream(TesslaCore.DefaultFrom(getStream(arguments(0)), getStream(arguments(1)), loc))
      case BuiltIn.Last => stream(TesslaCore.Last(getStream(arguments(0)), getStream(arguments(1)), loc))
      case BuiltIn.DelayedLast => stream(TesslaCore.DelayedLast(getStream(arguments(0)), getStream(arguments(1)), loc))
      case BuiltIn.Const => stream(TesslaCore.Const(getStream(arguments(0)), getValue(arguments(1)), loc))
      case BuiltIn.Time => stream(TesslaCore.Time(getStream(arguments(0)), loc))
      case BuiltIn.Merge => stream(TesslaCore.Merge(getStream(arguments(0)), getStream(arguments(1)), loc))
      case op: BuiltIn.PrimitiveOperator =>
        if (arguments.forall(_.isInstanceOf[ValueEntry])) {
          ValueEntry(evalPrimitiveOperator(op, arguments.map(_.asInstanceOf[ValueEntry].value), loc).map(_.get))
        } else {
          val typeArgs = Seq() // TODO: type args
          stream(TesslaCore.Lift(op, typeArgs, arguments.map(_.asInstanceOf[StreamEntry].stream), loc))
        }
    }
  }

  def translateMacro(mac: TypedTessla.Macro, arguments: Map[TypedTessla.Parameter, EnvEntry], loc: Location): EnvEntry = {
    ???
  }

  def translateVar(env: Env, id: TypedTessla.Identifier, loc: Location): EnvEntry = {
    translateEntry(env, env(id), Some(loc))
    env(id).entry
  }
}

object ConstantEvaluator {
  private def getInt(v: TesslaCore.Value): BigInt = v match {
    case intLit: TesslaCore.IntLiteral => intLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Int, got: $v", v.loc)
  }

  private def getBool(v: TesslaCore.Value): Boolean = v match {
    case boolLit: TesslaCore.BoolLiteral => boolLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Bool, got: $v", v.loc)
  }

  def evalPrimitiveOperator(op: BuiltIn.PrimitiveOperator,
                            arguments: Seq[Lazy[TesslaCore.Value]],
                            loc: Location): Lazy[Option[TesslaCore.Value]] = Lazy {
    def binIntOp(op: (BigInt, BigInt) => BigInt) = {
      Some(TesslaCore.IntLiteral(op(getInt(arguments(0).get), getInt(arguments(1).get)), loc))
    }

    def div(x: BigInt, y: BigInt): BigInt = {
      // This is a bit dirty because we hard-code the fact that y corresponds to arguments(1),
      // but since this is only a local function, it should be fine.
      if (y == 0) throw DivideByZero(arguments(1).get.loc)
      else x/y
    }

    op match {
      case BuiltIn.Add => binIntOp(_ + _)
      case BuiltIn.Sub => binIntOp(_ - _)
      case BuiltIn.Mul => binIntOp(_ * _)
      case BuiltIn.Div => binIntOp(div)
      case BuiltIn.IfThenElse =>
        if (getBool(arguments(0).get)) Some(arguments(1).get)
        else Some(arguments(2).get)
      case _ => ???
    }
  }
}
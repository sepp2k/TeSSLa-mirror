package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Errors.{DivideByZero, InfiniteRecursion, InternalError, UndefinedTimeUnit}
import de.uni_luebeck.isp.tessla.util.Lazy

import ConstantEvaluator._

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TesslaCore.IdentifierFactory with TranslationPhase[TypedTessla.Specification, TesslaCore.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntryWrapper]

  case class EnvEntryWrapper(var entry: EnvEntry)

  sealed abstract class EnvEntry
  case class StreamEntry(stream: TesslaCore.Stream, expressionWrapper: ExpressionWrapper, typ: TesslaCore.StreamType) extends EnvEntry
  case class InputStreamEntry(stream: TesslaCore.InputStream) extends EnvEntry
  case class NilEntry(nil: TesslaCore.Nil) extends EnvEntry
  case class ValueEntry(value: Lazy[TesslaCore.Value]) extends EnvEntry
  case class MacroEntry(mac: TypedTessla.Macro) extends EnvEntry
  case class BuiltInEntry(builtIn: BuiltIn) extends EnvEntry
  case object Translating extends EnvEntry
  case class NotYetTranslated(entry: TypedTessla.VariableEntry) extends EnvEntry

  case class ExpressionWrapper(var expressionOpt: Option[TesslaCore.Expression])

  override def translateSpec(spec: TypedTessla.Specification): TesslaCore.Specification = {
    val env = createEnvForScopeWithParents(spec.globalScope)
    translateEnv(env, None)
    def inputStreams = spec.globalScope.variables.collect {
      case (_, TypedTessla.VariableEntry(_, is: TypedTessla.InputStream, typ, _)) =>
        (is.name, translateStreamType(typ), is.loc)
    }.toSeq
    def outputStreams = spec.outStreams.map { os =>
      (os.name, getStream(env(os.id).entry))
    }
    val translatedStreams = env.values.collect {
      case EnvEntryWrapper(entry : StreamEntry) =>
        entry.expressionWrapper.expressionOpt match {
          case Some(expression) =>
            entry.stream.id -> TesslaCore.StreamDefinition(expression, entry.typ)
          case None => throw InternalError("Unevaluated stream expression")
        }
    }
    TesslaCore.Specification(translatedStreams.toMap, inputStreams, outputStreams)
  }

  def createEnvForScope(scope: TypedTessla.Scope): Env = {
    scope.variables.mapValues(entry => EnvEntryWrapper(NotYetTranslated(entry))).toMap
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

  def translateStreamType(typ: TypedTessla.Type) = typ match {
    case FlatTessla.StreamType(elementType) =>
      TesslaCore.StreamType(translateValueType(elementType))
    case _ =>
      throw InternalError(s"Expected stream type, got $typ - should have been caught by type checker")
  }

  def translateValueType(typ: TypedTessla.Type): TesslaCore.ValueType = typ match {
    case FlatTessla.IntType => TesslaCore.IntType
    case FlatTessla.StringType => TesslaCore.StringType
    case FlatTessla.BoolType => TesslaCore.BoolType
    case FlatTessla.UnitType => TesslaCore.UnitType
    case FlatTessla.MapType(k, v) => TesslaCore.MapType(translateValueType(k), translateValueType(v))
    case FlatTessla.SetType(t) => TesslaCore.SetType(translateValueType(t))
    case _ : FlatTessla.FunctionType =>
      throw InternalError(s"Function type where value type expected - should have been caught by type checker")
    case FlatTessla.StreamType(_) =>
      throw InternalError(s"Stream type where value type expected - should have been caught by type checker")
    case _: FlatTessla.TypeParameter =>
      throw InternalError(s"Left-over type variable in type")
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
  def translateEntry(env: Env, wrapper: EnvEntryWrapper, overrideLoc: Option[Location]): Unit = wrapper.entry match {
    case NotYetTranslated(entry) =>
      def stream(coreExp: => TesslaCore.Expression) = {
        val id = makeIdentifier()
        val exp = ExpressionWrapper(None)
        val typ = translateStreamType(entry.typeInfo)
        wrapper.entry = StreamEntry(TesslaCore.Stream(id, entry.loc), exp, typ)
        exp.expressionOpt = Some(coreExp)
        StreamEntry(TesslaCore.Stream(id, entry.loc), exp, typ)
      }

      wrapper.entry = Translating
      entry.expression match {
        case TypedTessla.BuiltInOperator(builtIn) =>
          wrapper.entry = BuiltInEntry(builtIn)
        case mac: TypedTessla.Macro =>
          wrapper.entry = MacroEntry(mac)
        case TypedTessla.Literal(lit, loc) =>
          // Evaluate the literal outside of the Lazy because we want TimeUnit-errors to appear even if
          // the expression is not used
          // TimeUnit-errors inside of macros *are* swallowed if the macro is never called, which is what
          // we want because a library might define macros using time units and, as long as you don't call
          // those macros, you should still be able to use the library when you don't have a time unit set.
          val translatedLit = translateLiteral(lit, overrideLoc.getOrElse(loc))
          wrapper.entry = ValueEntry(Lazy(translatedLit))
        case p: TypedTessla.Parameter =>
          wrapper.entry = translateVar(env, p.id, p.loc)
        case v: TypedTessla.Variable =>
          wrapper.entry = translateVar(env, v.id, v.loc)
        case i: TypedTessla.InputStream =>
          wrapper.entry = InputStreamEntry(TesslaCore.InputStream(i.name, overrideLoc.getOrElse(i.loc)))
        case call: TypedTessla.MacroCall =>
          // No checks regarding arity or non-existing or duplicate named arguments because those mistakes
          // would be caught by the type checker
          translateVar(env, call.macroID, call.macroLoc) match {
            case BuiltInEntry(builtIn) =>
              // This is lazy, so the arguments don't get evaluated until they're used below, allowing us to
              // initialize entries where appropriate before the evaluation takes place
              lazy val args = call.args.map {
                case FlatTessla.PositionalArgument(id, loc) =>
                  translateVar(env, id, loc)
                case FlatTessla.NamedArgument(name, _, loc) =>
                  throw InternalError(s"Undefined keyword arg $name for built-in should've been caught by type checker", loc)
              }
              builtIn match {
                case BuiltIn.Nil =>
                  wrapper.entry = NilEntry(TesslaCore.Nil(entry.loc))
                case BuiltIn.Default =>
                  stream {
                    TesslaCore.Default(getStream(args(0)), getValue(args(1)), entry.loc)
                  }
                case BuiltIn.DefaultFrom =>
                  stream {
                    TesslaCore.DefaultFrom(getStream(args(0)), getStream(args(1)), entry.loc)
                  }
                case BuiltIn.Last =>
                  stream {
                    TesslaCore.Last(getStream(args(0)), getStream(args(1)), entry.loc)
                  }
                case BuiltIn.DelayedLast =>
                  stream {
                    TesslaCore.DelayedLast(getStream(args(0)), getStream(args(1)), entry.loc)
                  }
                case BuiltIn.Const =>
                  stream {
                    TesslaCore.Const(getStream(args(0)), getValue(args(1)), entry.loc)
                  }
                case BuiltIn.Time =>
                  stream {
                    TesslaCore.Time(getStream(args(0)), entry.loc)
                  }
                case BuiltIn.Merge =>
                  stream {
                    TesslaCore.Merge(getStream(args(0)), getStream(args(1)), entry.loc)
                  }
                case op: BuiltIn.PrimitiveOperator =>
                  entry.typeInfo match {
                    case _ : FlatTessla.StreamType =>
                      // TODO: Currently type parameter can only be instantiated to value types. Once that changes,
                      //       this code needs to be adjusted
                      val typeArgs = call.typeArgs.map(translateValueType)
                      stream {
                        TesslaCore.Lift(op, typeArgs, args.map(getStream), entry.loc)
                      }
                    case _ =>
                      val value = evalPrimitiveOperator(op, args.map(_.asInstanceOf[ValueEntry].value), entry.loc)
                      wrapper.entry = ValueEntry(value.map(_.get))
                  }
              }

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
            case other =>
              throw InternalError(s"Applying non-macro/builtin ($other) - should have been caught by the type checker.")
          }
      }
    case Translating =>
      // TODO Proper loc with stack trace
      throw InfiniteRecursion(Location.unknown)
    case _ =>
      /* Do nothing */
  }

  def getStream(envEntry: EnvEntry): TesslaCore.StreamRef = envEntry match {
    case s : StreamEntry => s.stream
    case NilEntry(nil) => nil
    case i : InputStreamEntry => i.stream
    case other => throw InternalError(s"Wrong type of environment entry: Expected StreamEntry, found: $other")
  }

  def getValue(envEntry: EnvEntry): TesslaCore.ValueOrError = envEntry match {
    case ValueEntry(s) => TesslaCore.ValueOrError.fromLazy(s)
    case other => throw InternalError(s"Wrong type of environment entry: Expected ValueEntry, found: $other")
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
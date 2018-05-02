package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors.{DivideByZero, InfiniteRecursion, InternalError, UndefinedTimeUnit}
import de.uni_luebeck.isp.tessla.util.Lazy
import ConstantEvaluator._

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TesslaCore.IdentifierFactory with TranslationPhase[TypedTessla.Specification, TesslaCore.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntryWrapper]
  val translatedStreams = mutable.Map[TesslaCore.Identifier, TesslaCore.StreamDefinition]()

  case class EnvEntryWrapper(var entry: EnvEntry)

  sealed abstract class EnvEntry
  case class StreamEntry(stream: TesslaCore.Stream, typ: TesslaCore.StreamType) extends EnvEntry
  case class InputStreamEntry(stream: TesslaCore.InputStream) extends EnvEntry
  case class NilEntry(nil: TesslaCore.Nil) extends EnvEntry
  case class ValueEntry(value: Lazy[TesslaCore.Value]) extends EnvEntry
  case class MacroEntry(mac: TypedTessla.Macro, closure: Env) extends EnvEntry {
    override def toString = s"MacroEntry($mac, ...)"
  }
  case class BuiltInEntry(builtIn: BuiltIn) extends EnvEntry
  case object Translating extends EnvEntry
  case class NotYetTranslated(entry: TypedTessla.VariableEntry, var closure: Env) extends EnvEntry {
    override def toString = s"NotYetTranslated($entry, ...)"
  }
  case class TypeEntry(typ: TesslaCore.ValueType) extends EnvEntry

  override def translateSpec(spec: TypedTessla.Specification): TesslaCore.Specification = {
    val env = createEnvForScopeWithParents(spec.globalScope)
    translateEnv(env, None)
    val inputStreams = spec.globalScope.variables.collect {
      case (_, TypedTessla.VariableEntry(_, is: TypedTessla.InputStream, typ, _)) =>
        (is.name, translateStreamType(typ, env), is.loc)
    }.toSeq
    var outputStreams = spec.outStreams.map { os =>
      (os.name, getStream(env(os.id).entry))
    }
    if (spec.outAll) {
      outputStreams ++= translatedStreams.keys.flatMap { id =>
        id.nameOpt.map( name => (name, TesslaCore.Stream(id, Location.unknown)) )
      }
    }
    TesslaCore.Specification(translatedStreams.toMap, inputStreams, outputStreams)
  }

  def createEnvForScope(scope: TypedTessla.Scope, parent: Env): Env = {
    val entries = scope.variables.mapValues(entry => NotYetTranslated(entry, null)).toMap
    val env = parent ++ entries.mapValues(EnvEntryWrapper)
    entries.values.foreach(_.closure = env)
    env
  }

  def createEnvForScopeWithParents(scope: TypedTessla.Scope): Env = {
    val outerEnv = scope.parent.map(createEnvForScopeWithParents).getOrElse(Map())
    createEnvForScope(scope, outerEnv)
  }

  def translateEnv(env: Env, overrideLoc: Option[Location]): Unit = {
    env.foreach {
      case (_, entryWrapper) =>
        translateEntry(env, entryWrapper, overrideLoc)
    }
  }

  def translateStreamType(typ: TypedTessla.Type, env: Env) = typ match {
    case TypedTessla.StreamType(elementType) =>
      TesslaCore.StreamType(translateValueType(elementType, env))
    case _ =>
      throw InternalError(s"Expected stream type, got $typ - should have been caught by type checker")
  }

  def getType(env: Env, id: TypedTessla.Identifier) = env.get(id).map(_.entry) match {
    case Some(TypeEntry(typ)) => typ
    case other =>
      warn(InternalError(s"Expected type entry, found $other"))
      TesslaCore.IntType
  }

  def translateValueType(typ: TypedTessla.Type, env: Env): TesslaCore.ValueType = typ match {
    case TypedTessla.IntType => TesslaCore.IntType
    case TypedTessla.StringType => TesslaCore.StringType
    case TypedTessla.BoolType => TesslaCore.BoolType
    case TypedTessla.UnitType => TesslaCore.UnitType
    case TypedTessla.MapType(k, v) => TesslaCore.MapType(translateValueType(k, env), translateValueType(v, env))
    case TypedTessla.SetType(t) => TesslaCore.SetType(translateValueType(t, env))
    case _ : TypedTessla.FunctionType =>
      throw InternalError(s"Function type where value type expected - should have been caught by type checker")
    case TypedTessla.StreamType(_) =>
      throw InternalError(s"Stream type where value type expected - should have been caught by type checker")
    case tvar: TypedTessla.TypeParameter =>
      getType(env, tvar.id)
  }

  def translateLiteral(literal: Tessla.LiteralValue, loc: Location): TesslaCore.Value = literal match {
    case Tessla.IntLiteral(x) =>
      TesslaCore.IntLiteral(x, loc)
    case Tessla.TimeLiteral(x, tu) =>
      baseTimeUnit match {
        case Some(base) =>
          TesslaCore.IntLiteral(tu.convertTo(base) * x, loc)
        case None =>
          error(UndefinedTimeUnit(tu.loc))
          TesslaCore.IntLiteral(x, loc)
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
    case NotYetTranslated(entry, closure) =>
      translateExpression(closure, wrapper, entry.expression, entry.id.nameOpt, entry.typeInfo, overrideLoc)
    case Translating =>
      // TODO Proper loc with stack trace
      throw InfiniteRecursion(Location.unknown)
    case _ =>
      /* Do nothing */
  }

  def translateExpression(env: Env, wrapper: EnvEntryWrapper, expression: TypedTessla.Expression, nameOpt: Option[String],
                          typ: TypedTessla.Type, overrideLoc: Option[Location]): Unit = {
    wrapper.entry = Translating
    expression match {
      case TypedTessla.BuiltInOperator(builtIn) =>
        wrapper.entry = BuiltInEntry(builtIn)
      case mac: TypedTessla.Macro =>
        wrapper.entry = MacroEntry(mac, env)
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
        def stream(coreExp: => TesslaCore.Expression) = {
          val id = makeIdentifier(nameOpt)
          val translatedType = translateStreamType(typ, env)
          wrapper.entry = StreamEntry(TesslaCore.Stream(id, call.loc), translatedType)
          translatedStreams(id) = TesslaCore.StreamDefinition(coreExp, translatedType)
          wrapper.entry
        }
        // No checks regarding arity or non-existing or duplicate named arguments because those mistakes
        // would be caught by the type checker
        val callee = translateVar(env, call.macroID, call.macroLoc)
        callee match {
          case BuiltInEntry(builtIn) =>
            // This is lazy, so the arguments don't get evaluated until they're used below, allowing us to
            // initialize entries where appropriate before the evaluation takes place
            lazy val args = call.args.map {
              case TypedTessla.PositionalArgument(id, loc) =>
                translateVar(env, id, loc)
              case TypedTessla.NamedArgument(name, _, loc) =>
                throw InternalError(s"Undefined keyword arg $name for built-in should've been caught by type checker", loc)
            }
            builtIn match {
              case BuiltIn.Nil =>
                wrapper.entry = NilEntry(TesslaCore.Nil(call.loc))
              case BuiltIn.Default =>
                stream {
                  TesslaCore.Default(getStream(args(0)), getValue(args(1)), call.loc)
                }
              case BuiltIn.DefaultFrom =>
                stream {
                  TesslaCore.DefaultFrom(getStream(args(0)), getStream(args(1)), call.loc)
                }
              case BuiltIn.Last =>
                stream {
                  TesslaCore.Last(getStream(args(0)), getStream(args(1)), call.loc)
                }
              case BuiltIn.DelayedLast =>
                stream {
                  TesslaCore.DelayedLast(getStream(args(0)), getStream(args(1)), call.loc)
                }
              case BuiltIn.Const =>
                stream {
                  TesslaCore.Const(getValue(args(0)), getStream(args(1)), call.loc)
                }
              case BuiltIn.Time =>
                stream {
                  TesslaCore.Time(getStream(args(0)), call.loc)
                }
              case BuiltIn.Merge =>
                stream {
                  TesslaCore.Merge(getStream(args(0)), getStream(args(1)), call.loc)
                }
              case op: BuiltIn.PrimitiveOperator =>
                typ match {
                  case _ : TypedTessla.StreamType =>
                    // TODO: Currently type parameters can only be instantiated to value types. Once that changes,
                    //       this code needs to be adjusted
                    val typeArgs = call.typeArgs.map(translateValueType(_, env))
                    stream {
                      TesslaCore.Lift(op, typeArgs, args.map(getStream), call.loc)
                    }
                  case _ =>
                    val value = evalPrimitiveOperator(op, args.map {
                      case ValueEntry(v) => v
                      case other => throw InternalError(s"Expected value entry, found $other")
                    }, call.loc)
                    wrapper.entry = ValueEntry(value.map(_.get))
                }
            }

          case MacroEntry(mac, closure) =>
            var posArgIdx = 0
            val args = call.args.map {
              case arg: TypedTessla.PositionalArgument =>
                val param = mac.parameters(posArgIdx)
                posArgIdx += 1
                param.id -> env(arg.id)
              case arg: TypedTessla.NamedArgument =>
                mac.parameters.find(_.name == arg.name).get.id -> env(arg.id)
            }.toMap
            val scopeWithoutParameters = new TypedTessla.Scope(mac.scope.parent)
            mac.scope.variables.foreach {
              case (_, entry) =>
                entry.expression match {
                  case _: TypedTessla.Parameter => // do nothing
                  case _ => scopeWithoutParameters.addVariable(entry)
                }
            }
            val innerEnv = createEnvForScope(scopeWithoutParameters, closure ++ args)
            translateExpression(innerEnv, wrapper, mac.body, None, typ, Some(call.loc))
          case other =>
            throw InternalError(s"Applying non-macro/builtin (${other.getClass.getSimpleName}) - should have been caught by the type checker.")
        }
    }
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

  def translateVar(env: Env, id: TypedTessla.Identifier, loc: Location): EnvEntry = {
    val visited = mutable.Set[TypedTessla.Identifier]()
    var currentId = id
    var currentEnv = env
    var loop = true
    while (loop) {
      if (visited.contains(currentId)) throw InfiniteRecursion(loc)
      visited += currentId
      currentEnv(currentId).entry match {
        case NotYetTranslated(entry, closure) =>
          entry.expression match {
            case v: TypedTessla.Variable =>
              currentId = v.id
              currentEnv = closure
            case _ =>
              loop = false
          }
        case _ =>
          loop = false
      }
    }
    val wrapper = currentEnv(currentId)
    translateEntry(currentEnv, wrapper, Some(loc))
    wrapper.entry
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

    def binIntComp(op: (BigInt, BigInt) => Boolean) = {
      Some(TesslaCore.BoolLiteral(op(getInt(arguments(0).get), getInt(arguments(1).get)), loc))
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
      case BuiltIn.Eq => Some(TesslaCore.BoolLiteral(arguments(0).get.value == arguments(1).get.value, loc))
      case BuiltIn.Neq => Some(TesslaCore.BoolLiteral(arguments(0).get.value != arguments(1).get.value, loc))
      case BuiltIn.Lt => binIntComp(_ < _)
      case BuiltIn.Lte => binIntComp(_ <= _)
      case BuiltIn.Gt => binIntComp(_ > _)
      case BuiltIn.Gte => binIntComp(_ >= _)
      case BuiltIn.IfThen =>
        if (getBool(arguments(0).get)) Some(arguments(1).get)
        else None
      case BuiltIn.IfThenElse =>
        if (getBool(arguments(0).get)) Some(arguments(1).get)
        else Some(arguments(2).get)
      case BuiltIn.First =>
        Some(arguments.head.get)
      case _ => ???
    }
  }
}
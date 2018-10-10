package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla.interpreter.BuildInfo

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TesslaCore.IdentifierFactory with TranslationPhase[TypedTessla.Specification, TesslaCore.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntryWrapper]
  private val translatedStreams = mutable.Map[TesslaCore.Identifier, TesslaCore.Expression]()
  private val stack = mutable.ArrayStack[Location]()

  case class EnvEntryWrapper(var entry: EnvEntry)

  sealed abstract class EnvEntry
  case class Translated(result: Lazy[TranslationResult]) extends EnvEntry
  case class Translating(loc: Location) extends EnvEntry
  case class NotYetTranslated(entry: TypedTessla.VariableEntry, var closure: Env) extends EnvEntry {
    override def toString = s"NotYetTranslated($entry, ...)"
  }

  sealed abstract class TranslationResult
  case class StreamEntry(streamId: TesslaCore.Identifier, typ: TesslaCore.StreamType) extends TranslationResult
  case class InputStreamEntry(name: String, typ: TesslaCore.StreamType) extends TranslationResult
  case class NilEntry(nil: TesslaCore.Nil, typ: TesslaCore.StreamType) extends TranslationResult
  case class ValueEntry(value: TesslaCore.Value) extends TranslationResult
  case class ObjectEntry(members: Map[String, Lazy[TranslationResult]]) extends TranslationResult
  case class MacroEntry(mac: TypedTessla.Macro, closure: Env) extends TranslationResult {
    override def toString = s"MacroEntry($mac, ...)"
  }
  case class BuiltInEntry(builtIn: BuiltIn) extends TranslationResult
  case class TypeEntry(typ: TesslaCore.ValueType) extends TranslationResult

  val stdlib = BuiltIn.builtIns.collect {
    case (name, op: BuiltIn.PrimitiveOperator) =>
      name -> (makeIdentifier(name), op)
  }

  override def translateSpec(spec: TypedTessla.Specification): TesslaCore.Specification = {
    try {
      val env = createEnvForScopeWithParents(spec.globalScope)
      translateEnv(env)
      val inputStreams = spec.globalScope.variables.collect {
        case (_, TypedTessla.VariableEntry(_, is: TypedTessla.InputStream, typ, _)) =>
          (is.name, translateStreamType(typ, env), is.loc)
      }
      val outputStreams = spec.outStreams.map { os =>
        (os.name, getStream(env(os.id).entry, os.loc), getStreamType(env(os.id).entry))
      }
      val values = stdlib.map {
        case (_, (id, op)) =>
          id -> TesslaCore.BuiltInOperator(op, Location.builtIn)
      }.toSeq
      TesslaCore.Specification(translatedStreams.toSeq, values, inputStreams.toSeq, outputStreams)
    } catch {
      case err: TesslaError =>
        throw WithStackTrace(err, stack)
      case _: StackOverflowError =>
        throw WithStackTrace(Errors.StackOverflow(stack.pop()), stack)
    }
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

  def translateEnv(env: Env): Unit = {
    env.foreach {
      case (_, entryWrapper) =>
        translateEntry(env, entryWrapper)
    }
  }

  def translateStreamType(typ: TypedTessla.Type, env: Env) = typ match {
    case TypedTessla.StreamType(elementType) =>
      TesslaCore.StreamType(translateValueType(elementType, env))
    case _ =>
      throw InternalError(s"Expected stream type, got $typ - should have been caught by type checker")
  }

  def getType(env: Env, id: TypedTessla.Identifier) = env.get(id).map(_.entry) match {
    case Some(Translated(Lazy(TypeEntry(typ)))) => typ
    case _ =>
      // TODO: Possibly incorrect type information in generated TesslaCore
      TesslaCore.IntType
  }

  def translateValueType(typ: TypedTessla.Type, env: Env): TesslaCore.ValueType = typ match {
    case TypedTessla.IntType => TesslaCore.IntType
    case TypedTessla.StringType => TesslaCore.StringType
    case TypedTessla.BoolType => TesslaCore.BoolType
    case TypedTessla.UnitType => TesslaCore.UnitType
    case TypedTessla.CtfType => TesslaCore.CtfType
    case TypedTessla.OptionType(t) => TesslaCore.OptionType(translateValueType(t, env))
    case TypedTessla.MapType(k, v) => TesslaCore.MapType(translateValueType(k, env), translateValueType(v, env))
    case TypedTessla.SetType(t) => TesslaCore.SetType(translateValueType(t, env))
    case _ : TypedTessla.ObjectType =>
      // TODO
      throw InternalError(s"Objects aren't yet supported as value types")
    case _ : TypedTessla.FunctionType =>
      throw InternalError(s"Function type where value type expected - should have been caught by type checker")
    case TypedTessla.StreamType(_) =>
      throw InternalError(s"Stream type where value type expected - should have been caught by type checker")
    case tvar: TypedTessla.TypeParameter =>
      getType(env, tvar.id)
  }

  def translateLiteral(literal: Tessla.LiteralValue, loc: Location): TesslaCore.Value = literal match {
    case Tessla.IntLiteral(x) =>
      TesslaCore.IntValue(x, loc)
    case Tessla.TimeLiteral(x, tu) =>
      baseTimeUnit match {
        case Some(base) =>
          val conversionFactor = tu.convertTo(base).getOrElse(throw Errors.TimeUnitConversionError(tu, base))
          TesslaCore.IntValue(conversionFactor * x, loc)
        case None =>
          error(UndefinedTimeUnit(tu.loc))
          TesslaCore.IntValue(x, loc)
      }
    case Tessla.StringLiteral(str) =>
      TesslaCore.StringValue(str, loc)
    case Tessla.BoolLiteral(bool) =>
      TesslaCore.BoolValue(bool, loc)
    case Tessla.Unit =>
      TesslaCore.Unit(loc)
  }

  def translateEntry(env: Env, wrapper: EnvEntryWrapper): Unit = wrapper.entry match {
    case NotYetTranslated(entry, closure) =>
      translateExpression(closure, wrapper, entry.expression, entry.id.nameOpt, entry.typeInfo)
    case Translating(loc) =>
      throw InfiniteRecursion(loc)
    case _ =>
      /* Do nothing */
  }

  def translateExpression(env: Env, wrapper: EnvEntryWrapper, expression: TypedTessla.Expression, nameOpt: Option[String],
                          typ: TypedTessla.Type): Unit = {
    wrapper.entry = Translating(expression.loc)
    expression match {
      case TypedTessla.BuiltInOperator(BuiltIn.TesslaInfo) =>
        val members = Map(
          "version" -> Lazy(ValueEntry(TesslaCore.StringValue(BuildInfo.version, expression.loc)))
        )
        wrapper.entry = Translated(Lazy(ObjectEntry(members)))
      case TypedTessla.BuiltInOperator(builtIn) =>
        wrapper.entry = Translated(Lazy(BuiltInEntry(builtIn)))
      case mac: TypedTessla.Macro =>
        wrapper.entry = Translated(Lazy(MacroEntry(mac, env)))
      case TypedTessla.Literal(lit, loc) =>
        // Evaluate the literal outside of the Lazy because we want TimeUnit-errors to appear even if
        // the expression is not used
        // TimeUnit-errors inside of macros *are* swallowed if the macro is never called, which is what
        // we want because a library might define macros using time units and, as long as you don't call
        // those macros, you should still be able to use the library when you don't have a time unit set.
        val translatedLit = translateLiteral(lit, loc)
        wrapper.entry = Translated(Lazy(ValueEntry(translatedLit)))
      case p: TypedTessla.Parameter =>
        wrapper.entry = translateVar(env, p.id, p.loc)
      case v: TypedTessla.Variable =>
        wrapper.entry = translateVar(env, v.id, v.loc)
      case i: TypedTessla.InputStream =>
        val typ = translateStreamType(i.streamType, env)
        wrapper.entry = Translated(Lazy(InputStreamEntry(i.name, typ)))
      case ite: TypedTessla.StaticIfThenElse =>
        val cond = translateVar(env, ite.condition.id, ite.condition.loc)
        wrapper.entry = Translated(Lazy {
          if (Evaluator.getBool(getValue(cond).forceValue)) {
            val thenCase = translateVar(env, ite.thenCase.id, ite.thenCase.loc)
            getResult(thenCase).get
          } else {
            val elseCase = translateVar(env, ite.elseCase.id, ite.elseCase.loc)
            getResult(elseCase).get
          }
        })
      case obj: TypedTessla.ObjectLiteral =>
        wrapper.entry = Translated(Lazy {
          ObjectEntry(obj.members.map {
            case (name, member) =>
              name -> getResult(translateVar(env, member.id, member.loc))
          })
        })
      case acc: TypedTessla.MemberAccess =>
        wrapper.entry = Translated(Lazy {
          getResult(translateVar(env, acc.receiver.id, acc.loc)).get match {
            case obj: ObjectEntry =>
              obj.members(acc.member).get
            case _ =>
              throw InternalError("Member access on non-object should've been caught by type checker", acc.receiver.loc)
          }
        })
      case call: TypedTessla.MacroCall =>
        def stream(coreExp: => TesslaCore.Expression) = {
          val id = makeIdentifier(nameOpt)
          val translatedType = translateStreamType(typ, env)
          wrapper.entry = Translated(Lazy(StreamEntry(id, translatedType)))
          translatedStreams(id) = coreExp
          wrapper.entry
        }
        // No checks regarding arity or non-existing or duplicate named arguments because those mistakes
        // would be caught by the type checker
        val callee = translateVar(env, call.macroID, call.macroLoc)
        callee match {
          case Translated(Lazy(BuiltInEntry(builtIn))) =>
            // This is lazy, so the arguments don't get evaluated until they're used below, allowing us to
            // initialize entries where appropriate before the evaluation takes place
            lazy val args = call.args.map {
              case TypedTessla.PositionalArgument(id, loc) =>
                translateVar(env, id, loc)
              case TypedTessla.NamedArgument(name, _, loc) =>
                throw InternalError(s"Undefined keyword arg $name for built-in should've been caught by type checker", loc)
            }
            def streamArg(i: Int) = getStream(args(i), call.args(i).loc)
            builtIn match {
              case BuiltIn.Nil =>
                val typ = TesslaCore.StreamType(translateValueType(call.typeArgs.head, env))
                wrapper.entry = Translated(Lazy(NilEntry(TesslaCore.Nil(call.loc), typ)))
              case BuiltIn.Default =>
                stream {
                  TesslaCore.Default(streamArg(0), getValue(args(1)), call.loc)
                }
              case BuiltIn.DefaultFrom =>
                stream {
                  TesslaCore.DefaultFrom(streamArg(0), streamArg(1), call.loc)
                }
              case BuiltIn.Last =>
                stream {
                  TesslaCore.Last(streamArg(0), streamArg(1), call.loc)
                }
              case BuiltIn.DelayedLast =>
                stream {
                  TesslaCore.DelayedLast(streamArg(0), streamArg(1), call.loc)
                }
              case BuiltIn.Const =>
                stream {
                  TesslaCore.Const(getValue(args(0)), streamArg(1), call.loc)
                }
              case BuiltIn.Time =>
                stream {
                  TesslaCore.Time(streamArg(0), call.loc)
                }
              case BuiltIn.Merge =>
                stream {
                  TesslaCore.Merge(streamArg(0), streamArg(1), call.loc)
                }
              case BuiltIn.Lift =>
                stream {
                  ???
                }
              case BuiltIn.Lift3 =>
                stream {
                  ???
                }
              case op: BuiltIn.PrimitiveOperator =>
                typ match {
                  case _ : TypedTessla.StreamType =>
                    stream {
                      TesslaCore.SignalLift(stdlib(op.name)._1, args.indices.map(streamArg), call.loc)
                    }
                  case _ =>
                    val value = Evaluator.evalPrimitiveOperator(op, args.map { arg =>
                      getResult(arg).get match {
                        case ValueEntry(v) => v
                        case other => throw InternalError(s"Expected value entry, found $other")
                      }
                    }, call.loc)
                    wrapper.entry = Translated(Lazy(ValueEntry(value.get.forceValue)))
                }
              case BuiltIn.TesslaInfo =>
                throw InternalError(s"Applying non-macro/builtin (${builtIn.getClass.getSimpleName}) - should have been caught by the type checker.")
            }

          case Translated(Lazy(MacroEntry(mac, closure))) =>
            var posArgIdx = 0
            val args = call.args.map {
              case arg: TypedTessla.PositionalArgument =>
                val param = mac.parameters(posArgIdx)
                posArgIdx += 1
                param.id -> env(arg.id)
              case arg: TypedTessla.NamedArgument =>
                mac.parameters.find(_.name == arg.name) match {
                  case Some(param) => param.id -> env(arg.id)
                  case None => throw UndefinedNamedArg(arg)
                }
            }.toMap
            val scopeWithoutParameters = new TypedTessla.Scope(mac.scope.parent)
            mac.scope.variables.foreach {
              case (_, entry) =>
                entry.expression match {
                  case _: TypedTessla.Parameter => // do nothing
                  case _ => scopeWithoutParameters.addVariable(entry)
                }
            }
            stack.push(call.loc)
            val innerEnv = createEnvForScope(scopeWithoutParameters, closure ++ args)
            translateExpression(innerEnv, wrapper, mac.body, nameOpt, typ)
            stack.pop()
          case other =>
            throw InternalError(s"Applying non-macro/builtin (${other.getClass.getSimpleName}) - should have been caught by the type checker.")
        }
    }
  }

  def getResult(envEntry: EnvEntry): Lazy[TranslationResult] = envEntry match {
    case Translated(result) => result
    case other => throw InternalError(s"Wrong type of environment entry: Expected Translated, found: $other")
  }

  def getStream(envEntry: EnvEntry, loc: Location): TesslaCore.StreamRef = getResult(envEntry).get match {
    case s : StreamEntry => TesslaCore.Stream(s.streamId, loc)
    case n: NilEntry => n.nil
    case i : InputStreamEntry => TesslaCore.InputStream(i.name, loc)
    case other => throw InternalError(s"Wrong type of environment entry: Expected StreamEntry, found: $other")
  }

  def getStreamType(envEntry: EnvEntry): TesslaCore.StreamType = getResult(envEntry).get match {
    case s : StreamEntry => s.typ
    case n: NilEntry => n.typ
    case i : InputStreamEntry => i.typ
    case other => throw InternalError(s"Wrong type of environment entry: Expected StreamEntry, found: $other")
  }

  def getValue(envEntry: EnvEntry): TesslaCore.ValueOrError = {
    try {
      getResult(envEntry).get match {
        case ValueEntry(v) => v
        case other => throw InternalError(s"Wrong type of environment entry: Expected ValueEntry, found: $other")
      }
    } catch {
      case e: TesslaError =>
        TesslaCore.Error(e)
    }
  }

  def translateVar(env: Env, id: TypedTessla.Identifier, loc: Location): EnvEntry = {
    if (id.nameOpt.isDefined) {
      stack.push(loc)
    }
    val wrapper = env(id)
    translateEntry(env, wrapper)
    if (id.nameOpt.isDefined) {
      stack.pop()
    }
    wrapper.entry
  }
}
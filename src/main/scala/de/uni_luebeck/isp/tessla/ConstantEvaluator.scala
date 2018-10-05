package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import ConstantEvaluator._
import org.eclipse.tracecompass.ctf.core.event.types.ICompositeDefinition
import de.uni_luebeck.isp.tessla.interpreter.BuildInfo

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TesslaCore.IdentifierFactory with TranslationPhase[TypedTessla.Specification, TesslaCore.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntryWrapper]
  private val translatedStreams = mutable.Map[TesslaCore.Identifier, TesslaCore.StreamDefinition]()
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
  case class InputStreamEntry(name: String) extends TranslationResult
  case class NilEntry(nil: TesslaCore.Nil) extends TranslationResult
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
        (os.name, getStream(env(os.id).entry, os.loc))
      }
      val values = stdlib.map {
        case (_, (id, op)) =>
          id -> TesslaCore.BuiltInOperator(op)
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
        wrapper.entry = Translated(Lazy(InputStreamEntry(i.name)))
      case ite: TypedTessla.StaticIfThenElse =>
        val cond = translateVar(env, ite.condition.id, ite.condition.loc)
        wrapper.entry = Translated(Lazy {
          if (getBool(getValue(cond).forceValue)) {
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
          translatedStreams(id) = TesslaCore.StreamDefinition(coreExp, translatedType)
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
                wrapper.entry = Translated(Lazy(NilEntry(TesslaCore.Nil(call.loc))))
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
              case op: BuiltIn.PrimitiveOperator =>
                typ match {
                  case _ : TypedTessla.StreamType =>
                    // TODO: Currently type parameters can only be instantiated to value types. Once that changes,
                    //       this code needs to be adjusted
                    val typeArgs = call.typeArgs.map(translateValueType(_, env))
                    stream {
                      TesslaCore.Lift(stdlib(op.name)._1, typeArgs, args.indices.map(streamArg), call.loc)
                    }
                  case _ =>
                    val value = evalPrimitiveOperator(op, call.typeArgs.map(translateValueType(_, env)), args.map { arg =>
                      Lazy {
                        getResult(arg).get match {
                          case ValueEntry(v) => v
                          case other => throw InternalError(s"Expected value entry, found $other")
                        }
                      }
                    }, call.loc)
                    wrapper.entry = Translated(value.map(v => ValueEntry(v.get)))
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
    case NilEntry(nil) => nil
    case i : InputStreamEntry => TesslaCore.InputStream(i.name, loc)
    case other => throw InternalError(s"Wrong type of environment entry: Expected StreamEntry, found: $other")
  }

  def getValue(envEntry: EnvEntry): TesslaCore.ValueOrError = {
    TesslaCore.ValueOrError.fromLazy(Lazy{
      getResult(envEntry).get match {
        case ValueEntry(v) => v
        case other => throw InternalError(s"Wrong type of environment entry: Expected ValueEntry, found: $other")
      }
    })
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

object ConstantEvaluator {
  private def getInt(v: TesslaCore.Value): BigInt = v match {
    case intLit: TesslaCore.IntValue => intLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Int, got: $v", v.loc)
  }

  private def getBool(v: TesslaCore.Value): Boolean = v match {
    case boolLit: TesslaCore.BoolValue => boolLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Bool, got: $v", v.loc)
  }

  private def getString(v: TesslaCore.Value): String = v match {
    case stringLit: TesslaCore.StringValue => stringLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: String, got: $v", v.loc)
  }

  private def getOption(v: TesslaCore.Value): TesslaCore.TesslaOption = v match {
    case optionValue: TesslaCore.TesslaOption => optionValue
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Option ${v.loc}, got: $v", v.loc)
  }

  private def getCtf(v: TesslaCore.Value): ICompositeDefinition = v match {
    case ctfLit: TesslaCore.Ctf => ctfLit.value
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: CTF, got: $v", v.loc)
  }

  private def getMap(v: TesslaCore.Value): TesslaCore.TesslaMap = v match {
    case mapLit: TesslaCore.TesslaMap => mapLit
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Map, got: $v", v.loc)
  }

  private def getSet(v: TesslaCore.Value): TesslaCore.TesslaSet = v match {
    case setLit: TesslaCore.TesslaSet => setLit
    case _ => throw InternalError(s"Type error should've been caught by type checker: Expected: Set, got: $v", v.loc)
  }

  def evalPrimitiveOperator(op: BuiltIn.PrimitiveOperator, typeArguments: Seq[TesslaCore.ValueType],
                            arguments: Seq[Lazy[TesslaCore.Value]],
                            loc: Location): Lazy[Option[TesslaCore.Value]] = Lazy {
    def binIntOp(op: (BigInt, BigInt) => BigInt) = {
      Some(TesslaCore.IntValue(op(getInt(arguments(0).get), getInt(arguments(1).get)), loc))
    }

    def binIntComp(op: (BigInt, BigInt) => Boolean) = {
      Some(TesslaCore.BoolValue(op(getInt(arguments(0).get), getInt(arguments(1).get)), loc))
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
      case BuiltIn.LeftShift => binIntOp(_ << _.toInt)
      case BuiltIn.RightShift => binIntOp(_ >> _.toInt)
      case BuiltIn.BitAnd => binIntOp(_ & _)
      case BuiltIn.BitOr => binIntOp(_ | _)
      case BuiltIn.BitXor => binIntOp(_ ^ _)
      case BuiltIn.BitFlip => Some(TesslaCore.IntValue(~getInt(arguments(0).get), loc))
      case BuiltIn.Negate => Some(TesslaCore.IntValue(-getInt(arguments(0).get), loc))
      case BuiltIn.Eq => Some(TesslaCore.BoolValue(arguments(0).get == arguments(1).get, loc))
      case BuiltIn.Neq => Some(TesslaCore.BoolValue(arguments(0).get != arguments(1).get, loc))
      case BuiltIn.Lt => binIntComp(_ < _)
      case BuiltIn.Lte => binIntComp(_ <= _)
      case BuiltIn.Gt => binIntComp(_ > _)
      case BuiltIn.Gte => binIntComp(_ >= _)
      case BuiltIn.And => Some(TesslaCore.BoolValue(getBool(arguments(0).get) && getBool(arguments(1).get), loc))
      case BuiltIn.Or => Some(TesslaCore.BoolValue(getBool(arguments(0).get) || getBool(arguments(1).get), loc))
      case BuiltIn.Not => Some(TesslaCore.BoolValue(!getBool(arguments(0).get), loc))
      case BuiltIn.IfThen =>
        if (getBool(arguments(0).get)) Some(arguments(1).get)
        else None
      case BuiltIn.IfThenElse =>
        if (getBool(arguments(0).get)) Some(arguments(1).get)
        else Some(arguments(2).get)
      case BuiltIn.First =>
        Some(arguments(0).get)
      case BuiltIn.None =>
        Some(TesslaCore.TesslaOption(None, TesslaCore.OptionType(typeArguments(0)), loc))
      case BuiltIn.Some =>
        Some(TesslaCore.TesslaOption(Some(arguments(0).get), TesslaCore.OptionType(arguments(0).get.typ), loc))
      case BuiltIn.IsNone =>
        Some(TesslaCore.BoolValue(getOption(arguments(0).get).value.isEmpty, loc))
      case BuiltIn.GetSome =>
        getOption(arguments(0).get).value match {
          case None => throw CannotGetValueOfNone(loc)
          case some => some
        }
      case BuiltIn.MapEmpty =>
        Some(TesslaCore.TesslaMap(Map(), TesslaCore.MapType(typeArguments(0), typeArguments(1)), loc))
      case BuiltIn.MapAdd =>
        val map = getMap(arguments(0).get)
        Some(TesslaCore.TesslaMap(map.value + (arguments(1).get -> arguments(2).get), map.typ, loc))
      case BuiltIn.MapGet =>
        val map = getMap(arguments(0).get)
        val key = arguments(1).get
        try {
          Some(map.value(key).withLoc(loc))
        } catch {
          case _: NoSuchElementException =>
            throw KeyNotFound(key, map.value, loc)
        }
      case BuiltIn.MapContains =>
        Some(TesslaCore.BoolValue(getMap(arguments(0).get).value.contains(arguments(1).get), loc))
      case BuiltIn.MapRemove =>
        val map = getMap(arguments(0).get)
        Some(TesslaCore.TesslaMap(map.value - arguments(1).get, map.typ, loc))
      case BuiltIn.MapSize =>
        val map = getMap(arguments(0).get)
        Some(TesslaCore.IntValue(map.value.size, loc))
      case BuiltIn.SetEmpty =>
        Some(TesslaCore.TesslaSet(Set(), TesslaCore.SetType(typeArguments(0)), loc))
      case BuiltIn.SetAdd =>
        val set = getSet(arguments(0).get)
        Some(TesslaCore.TesslaSet(set.value + arguments(1).get, set.typ, loc))
      case BuiltIn.SetContains =>
        Some(TesslaCore.BoolValue(getSet(arguments(0).get).value.contains(arguments(1).get), loc))
      case BuiltIn.SetRemove =>
        val set = getSet(arguments(0).get)
        Some(TesslaCore.TesslaSet(set.value - arguments(1).get, set.typ, loc))
      case BuiltIn.SetSize =>
        val set = getSet(arguments(0).get)
        Some(TesslaCore.IntValue(set.value.size, loc))
      case BuiltIn.SetUnion =>
        val set1 = getSet(arguments(0).get)
        val set2 = getSet(arguments(1).get)
        Some(TesslaCore.TesslaSet(set1.value | set2.value, set1.typ, loc))
      case BuiltIn.SetIntersection =>
        val set1 = getSet(arguments(0).get)
        val set2 = getSet(arguments(1).get)
        Some(TesslaCore.TesslaSet(set1.value & set2.value, set1.typ, loc))
      case BuiltIn.CtfGetInt =>
        val composite = getCtf(arguments(0).get)
        val key = getString(arguments(1).get)
        Some(TesslaCore.IntValue(Ctf.getInt(composite, key), loc))
      case BuiltIn.CtfGetString =>
        val composite = getCtf(arguments(0).get)
        val key = getString(arguments(1).get)
        Some(TesslaCore.StringValue(Ctf.getString(composite, key), loc))
    }
  }
}

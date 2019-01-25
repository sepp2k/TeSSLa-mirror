package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla.interpreter.BuildInfo
import de.uni_luebeck.isp.tessla.util._

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TesslaCore.IdentifierFactory with TranslationPhase[TypedTessla.Specification, TesslaCore.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntryWrapper]
  private val translatedStreams = mutable.Map[TesslaCore.Identifier, TesslaCore.Expression]()
  private val stack = mutable.ArrayStack[Location]()

  case class EnvEntryWrapper(var entry: EnvEntry)

  sealed abstract class EnvEntry
  case class Translated(result: Lazy[TranslationResult]) extends EnvEntry
  case class Translating(loc: Location) extends EnvEntry
  case class NotYetTranslated(entry: TypedTessla.VariableEntry, var closure: Env, inFunction: Boolean) extends EnvEntry {
    override def toString = s"NotYetTranslated($entry, ...)"
  }

  sealed abstract class TranslationResult
  case class StreamEntry(streamId: TesslaCore.Identifier, typ: TesslaCore.StreamType) extends TranslationResult
  case class InputStreamEntry(name: String, typ: TesslaCore.StreamType) extends TranslationResult
  case class NilEntry(nil: TesslaCore.Nil, typ: TesslaCore.StreamType) extends TranslationResult
  case class ValueEntry(value: TesslaCore.ValueOrError) extends TranslationResult
  case class ObjectEntry(members: Map[String, Lazy[TranslationResult]], loc: Location) extends TranslationResult
  case class FunctionEntry(f: TesslaCore.Function, id: TesslaCore.Identifier) extends TranslationResult
  case class MacroEntry(mac: TypedTessla.Macro, closure: Env, functionValue: Option[TesslaCore.Closure]) extends TranslationResult {
    override def toString = s"MacroEntry($mac, ...)"
  }
  case class BuiltInEntry(builtIn: BuiltIn) extends TranslationResult
  case class TypeEntry(typ: TesslaCore.ValueType) extends TranslationResult
  case class FunctionParameterEntry(id: TesslaCore.Identifier) extends TranslationResult
  case class ValueExpressionEntry(exp: TesslaCore.ValueExpression, id: TesslaCore.Identifier) extends TranslationResult

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
      TesslaCore.Specification(translatedStreams.toSeq, inputStreams.toSeq, outputStreams)
    } catch {
      case err: TesslaError =>
        throw WithStackTrace(err, stack)
      case _: StackOverflowError =>
        throw WithStackTrace(Errors.StackOverflow(stack.pop()), stack)
    }
  }

  def createEnvForScope(scope: TypedTessla.Scope, parent: Env): Env = {
    val entries = mapValues(scope.variables)(entry => NotYetTranslated(entry, null, inFunction = false)).toMap
    val env = parent ++ mapValues(entries)(EnvEntryWrapper)
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
    case TypedTessla.ListType(t) => TesslaCore.ListType(translateValueType(t, env))
    case ot : TypedTessla.ObjectType =>
      TesslaCore.ObjectType(mapValues(ot.memberTypes)(translateValueType(_, env)))
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
    case NotYetTranslated(entry, closure, inFunction) =>
      // TODO: replace with just entry.id.nameOpt once the resultWrapper-hack is fixed
      val nameOpt = Option(entry.id).flatMap(_.nameOpt)
      translateExpression(closure, wrapper, entry.expression, nameOpt, entry.typeInfo, inFunction)
    case Translating(loc) =>
      throw InfiniteRecursion(loc)
    case _ =>
      /* Do nothing */
  }

  def isValueFunction(mac: TypedTessla.Macro): Boolean = {
    isValueCompatibleType(mac.returnType) && mac.parameters.forall(p => isValueCompatibleType(p.parameterType))
  }

  def translateExpression(env: Env, wrapper: EnvEntryWrapper, expression: TypedTessla.Expression, nameOpt: Option[String],
                          typ: TypedTessla.Type, inFunction: Boolean): Unit = {
    wrapper.entry = Translating(expression.loc)
    expression match {
      case TypedTessla.BuiltInOperator(BuiltIn.TesslaInfo) =>
        val members = Map(
          "version" -> Lazy(ValueEntry(TesslaCore.StringValue(BuildInfo.version, expression.loc)))
        )
        wrapper.entry = Translated(Lazy(ObjectEntry(members, Location.builtIn)))
      case TypedTessla.BuiltInOperator(builtIn) =>
        wrapper.entry = Translated(Lazy(BuiltInEntry(builtIn)))
      case mac: TypedTessla.Macro if inFunction =>
        val f = translateFunction(env, mac)
        wrapper.entry = Translated(Lazy(FunctionEntry(f, makeIdentifier(nameOpt))))
      case mac: TypedTessla.Macro =>
        val functionValue = optionIf(isValueFunction(mac)) {
          val f = translateFunction(env, mac)
          TesslaCore.Closure(f, Map(), mac.loc)
        }
        wrapper.entry = Translated(Lazy(MacroEntry(mac, env, functionValue)))
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
      case ite: TypedTessla.StaticIfThenElse if inFunction =>
        wrapper.entry = Translated(Lazy {
          val cond = getValueArg(translateVar(env, ite.condition.id, ite.condition.loc))
          val thenCase = Lazy(getValueArg(translateVar(env, ite.thenCase.id, ite.thenCase.loc)))
          val elseCase = Lazy(getValueArg(translateVar(env, ite.elseCase.id, ite.elseCase.loc)))
          val id = makeIdentifier(nameOpt)
          ValueExpressionEntry(TesslaCore.IfThenElse(cond, thenCase, elseCase, ite.loc), id)
        })
      case ite: TypedTessla.StaticIfThenElse =>
        val cond = translateVar(env, ite.condition.id, ite.condition.loc)
        wrapper.entry = Translated(Lazy {
          try {
            if (Evaluator.getBool(getValue(cond).forceValue)) {
              val thenCase = translateVar(env, ite.thenCase.id, ite.thenCase.loc)
              getResult(thenCase).get
            } else {
              val elseCase = translateVar(env, ite.elseCase.id, ite.elseCase.loc)
              getResult(elseCase).get
            }
          } catch {
            case e: TesslaError =>
              ValueEntry(TesslaCore.Error(e))
          }
        })
      case obj: TypedTessla.ObjectLiteral =>
        wrapper.entry = Translated(Lazy {
          ObjectEntry(mapValues(obj.members) { member =>
              getResult(translateVar(env, member.id, member.loc))
          }, obj.loc)
        })
      case acc: TypedTessla.MemberAccess if inFunction =>
        wrapper.entry = Translated(Lazy {
          val arg = getValueArg(translateVar(env, acc.receiver.id, acc.loc))
          val ma = TesslaCore.MemberAccess(arg, acc.member, acc.loc)
          ValueExpressionEntry(ma, makeIdentifier(nameOpt))
        })
      case acc: TypedTessla.MemberAccess =>
        wrapper.entry = Translated(Lazy {
          getResult(translateVar(env, acc.receiver.id, acc.loc)).get match {
            case obj: ObjectEntry =>
              obj.members(acc.member).get
            case ValueEntry(obj: TesslaCore.TesslaObject) =>
              ValueEntry(obj.value(acc.member))
            case other =>
              throw InternalError(s"Member access on non-object ($other) should've been caught by type checker", acc.receiver.loc)
          }
        })
      case call: TypedTessla.MacroCall if inFunction =>
        wrapper.entry = Translated(Lazy {
          val f = Lazy(getFunction(env, call.macroID, call.loc))
          val args = call.args.map(arg => Lazy(getValueArg(translateVar(env, arg.id, arg.loc))))
          val id = makeIdentifier(nameOpt)
          ValueExpressionEntry(TesslaCore.Application(f, args, call.loc), id)
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
              case BuiltIn.Delay =>
                stream {
                  TesslaCore.Delay(streamArg(0), streamArg(1), call.loc)
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
                  val f = getFunction(env, call.args(2).id, call.args(2).loc)
                  val liftArgs = Seq(getStream(args(0), call.args(0).loc), getStream(args(1), call.args(1).loc))
                  TesslaCore.Lift(f, liftArgs, call.loc)
                }
              case BuiltIn.Lift3 =>
                stream {
                  val f = getFunction(env, call.args(3).id, call.args(3).loc)
                  val liftArgs = Seq(
                    getStream(args(0), call.args(0).loc),
                    getStream(args(1), call.args(1).loc),
                    getStream(args(2), call.args(2).loc)
                  )
                  TesslaCore.Lift(f, liftArgs, call.loc)
                }
              case BuiltIn.StdLibCount =>
                stream {
                  TesslaCore.StdLibCount(streamArg(0), call.loc)
                }
              case op: BuiltIn.PrimitiveOperator =>
                typ match {
                  case _ : TypedTessla.StreamType =>
                    stream {
                      TesslaCore.SignalLift(op, args.indices.map(streamArg), call.loc)
                    }
                  case _ =>
                    val value = Evaluator.evalPrimitiveOperator(op, args.map(getValue), call.loc)
                    wrapper.entry = Translated(Lazy(ValueEntry(value.get)))
                }
              case BuiltIn.TesslaInfo =>
                throw InternalError(s"Applying non-macro/builtin (${builtIn.getClass.getSimpleName}) - should have been caught by the type checker.")
            }

          case Translated(Lazy(me: MacroEntry)) =>
            var posArgIdx = 0
            val args = call.args.map {
              case arg: TypedTessla.PositionalArgument =>
                val param = me.mac.parameters(posArgIdx)
                posArgIdx += 1
                param.id -> env(arg.id)
              case arg: TypedTessla.NamedArgument =>
                me.mac.parameters.find(_.name == arg.name) match {
                  case Some(param) => param.id -> env(arg.id)
                  case None => throw UndefinedNamedArg(arg)
                }
            }.toMap
            val scopeWithoutParameters = new TypedTessla.Scope(me.mac.scope.parent)
            me.mac.scope.variables.foreach {
              case (_, entry) =>
                entry.expression match {
                  case _: TypedTessla.Parameter => // do nothing
                  case _ => scopeWithoutParameters.addVariable(entry)
                }
            }
            stack.push(call.loc)
            val innerEnv = createEnvForScope(scopeWithoutParameters, me.closure ++ args)
            translateExpression(innerEnv, wrapper, me.mac.body, nameOpt, typ, inFunction)
            stack.pop()
          case other =>
            throw InternalError(s"Applying non-macro/builtin (${other.getClass.getSimpleName}) - should have been caught by the type checker.")
        }
    }
  }

  def getFunction(env: Env, id: TypedTessla.Identifier, loc: Location): TesslaCore.ValueArg = {
    translateVar(env, id, loc) match {
      case Translated(Lazy(me: MacroEntry)) =>
        me.functionValue match {
          case Some(f) => f
          case None => throw InternalError("Lifting non-value macro - should have been caught by type checker")
        }
      case Translated(Lazy(fe: FunctionEntry)) =>
        TesslaCore.ValueExpressionRef(fe.id)
      case Translated(Lazy(BuiltInEntry(primOp: BuiltIn.PrimitiveOperator))) =>
        TesslaCore.BuiltInOperator(primOp, loc)
      case Translated(Lazy(param: FunctionParameterEntry)) =>
        TesslaCore.ValueExpressionRef(param.id)
      case Translated(Lazy(vee: ValueExpressionEntry)) =>
        TesslaCore.ValueExpressionRef(vee.id)
      case other => throw InternalError(s"Wrong type of environment entry: Expected macro or primitive function, found: $other")
    }
  }

  /***
    * Checks whether the given type can be used inside a value function definition
    */
  def isValueCompatibleType(typ: TypedTessla.Type): Boolean = typ match {
    case _: TypedTessla.StreamType =>
      false
    case ft: TypedTessla.FunctionType =>
      isValueCompatibleType(ft.returnType) && ft.parameterTypes.forall(isValueCompatibleType)
    case ot: TypedTessla.ObjectType =>
      ot.memberTypes.values.forall(isValueCompatibleType)
    case _ =>
      true
  }

  def translateFunction(closure: Env, mac: TypedTessla.Macro): TesslaCore.Function = {
    val params = mac.parameters.map { param =>
      param.id -> makeIdentifier(param.name)
    }
    val scopeNotYetTranslateds = mac.scope.variables.values.filter { entry =>
      !entry.expression.isInstanceOf[TypedTessla.Parameter] && isValueCompatibleType(entry.typeInfo)
    }.map { entry =>
      NotYetTranslated(entry, null, inFunction = true)
    }
    val scopeWrappers = scopeNotYetTranslateds.map(nyt => nyt.entry.id -> EnvEntryWrapper(nyt))
    val env: Env = closure ++ mapValues(params.toMap) { newId =>
        EnvEntryWrapper(Translated(Lazy(FunctionParameterEntry(newId))))
    } ++ scopeWrappers
    scopeNotYetTranslateds.foreach(_.closure = env)
    scopeWrappers.foreach {
      case (_, wrapper) =>
        translateEntry(env, wrapper)
    }
    // This null should be safe because we only use this entry to put into a NYT and no one ever uses the ID of a
    // VariableEntry in an NYT
    val resultEntry = TypedTessla.VariableEntry(null, mac.body, mac.returnType, mac.body.loc)
    val resultWrapper = EnvEntryWrapper(NotYetTranslated(resultEntry, env, inFunction = true))
    translateEntry(env, resultWrapper)
    val scope = (resultWrapper.entry +: scopeWrappers.map(_._2.entry).toSeq).flatMap {
      case Translated(Lazy(fe: FunctionEntry)) =>
        Some(fe.id -> fe.f)
      case Translated(Lazy(ae: ValueExpressionEntry)) =>
        Some(ae.id -> ae.exp)
      case _ =>
        None
    }
    TesslaCore.Function(params.map(_._2), scope.toMap, getValueArg(resultWrapper.entry), mac.loc)
  }

  def getValueArg(entry: EnvEntry): TesslaCore.ValueArg = entry match {
    case Translated(Lazy(BuiltInEntry(prim: BuiltIn.PrimitiveOperator))) =>
      TesslaCore.BuiltInOperator(prim, Location.builtIn)
    case Translated(Lazy(me: MacroEntry)) =>
      me.functionValue match {
        case Some(f) => f
        case None => throw InternalError("Expected value-level entry, but found non-function macro entry")
      }
    case Translated(Lazy(fe: FunctionEntry)) =>
      TesslaCore.ValueExpressionRef(fe.id)
    case Translated(Lazy(ve: ValueEntry)) =>
      ve.value
    case Translated(Lazy(ae: ValueExpressionEntry)) =>
      TesslaCore.ValueExpressionRef(ae.id)
    case Translated(Lazy(param: FunctionParameterEntry)) =>
      TesslaCore.ValueExpressionRef(param.id)
    case Translated(Lazy(oe: ObjectEntry)) =>
      val members = mapValues(oe.members)(value => getValueArg(Translated(value)))
      TesslaCore.ObjectCreation(members, oe.loc)
    case _ =>
      throw InternalError(s"Expected value-level entry, but found $entry")
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
        case ve: ValueEntry => ve.value
        case oe: ObjectEntry =>
          val members = mapValues(oe.members)(v => getValue(Translated(v)).forceValue)
          TesslaCore.TesslaObject(members, oe.loc)
        case me: MacroEntry =>
          me.functionValue match {
            case Some(f) => f
            case None => throw InternalError("Using non-value macro as value - should have been caught by type checker")
          }
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
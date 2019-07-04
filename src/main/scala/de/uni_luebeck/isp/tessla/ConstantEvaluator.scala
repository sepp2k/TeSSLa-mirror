package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.TesslaCore.CurriedPrimitiveOperator
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla.util._

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit]) extends TranslationPhase[TypedTessla.TypedSpecification, TesslaCore.Specification] {
  override def translate(spec: TypedTessla.TypedSpecification) = {
    new ConstantEvaluatorWorker(spec, baseTimeUnit).translate()
  }
}

class ConstantEvaluatorWorker(spec: TypedTessla.TypedSpecification, baseTimeUnit: Option[TimeUnit])
  extends TesslaCore.IdentifierFactory with TranslationPhase.Translator[TesslaCore.Specification]  {
  type Env = Map[TypedTessla.Identifier, EnvEntryWrapper]
  private val translatedStreams = mutable.Map[TesslaCore.Identifier, TesslaCore.StreamDescription]()
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
  case class BuiltInEntry(builtIn: TypedTessla.BuiltInOperator, typ: TypedTessla.Type) extends TranslationResult {
    def name = builtIn.name
    def parameters = builtIn.parameters
  }
  case class TypeEntry(typ: TesslaCore.ValueType) extends TranslationResult
  case class FunctionParameterEntry(id: TesslaCore.Identifier) extends TranslationResult
  case class ValueExpressionEntry(exp: TesslaCore.ValueExpression, id: TesslaCore.Identifier) extends TranslationResult

  override def translateSpec(): TesslaCore.Specification = {
    try {
      val env = createEnvForDefsWithParents(spec.globalDefs)
      translateEnv(env)
      val inputStreams = spec.globalDefs.variables.collect {
        case (_, TypedTessla.VariableEntry(_, is: TypedTessla.InputStream, typ, annotations, _)) =>
          TesslaCore.InStreamDescription(is.name, translateStreamType(typ, env), annotations, is.loc)
      }
      val outputStreams = spec.outStreams.map { os =>
        val s = getStream(env(os.id).entry, os.loc)
        TesslaCore.OutStreamDescription(os.nameOpt, s, getStreamType(env(os.id).entry))
      }
      TesslaCore.Specification(translatedStreams.values.toSeq, inputStreams.toSeq, outputStreams, identifierCounter)
    } catch {
      case err: TesslaError =>
        throw WithStackTrace(err, stack)
      case _: StackOverflowError =>
        throw WithStackTrace(Errors.StackOverflow(stack.pop()), stack)
    }
  }

  def createEnvForDefs(defs: TypedTessla.Definitions, parent: Env): Env = {
    val entries = mapValues(defs.variables)(entry => NotYetTranslated(entry, null, inFunction = false)).toMap
    val env = parent ++ mapValues(entries)(EnvEntryWrapper)
    entries.values.foreach(_.closure = env)
    env
  }

  def createEnvForDefsWithParents(defs: TypedTessla.Definitions): Env = {
    val outerEnv = defs.parent.map(createEnvForDefsWithParents).getOrElse(Map())
    createEnvForDefs(defs, outerEnv)
  }

  def translateEnv(env: Env): Unit = {
    env.foreach {
      case (_, entryWrapper) =>
        translateEntry(env, entryWrapper)
    }
  }

  def translateStreamType(typ: TypedTessla.Type, env: Env) = typ match {
    case b @ TypedTessla.BuiltInType(_, Seq(elementType)) if b.isStreamType =>
      TesslaCore.StreamType(translateValueType(elementType, env))
    case _ =>
      throw InternalError(s"Expected stream type, got $typ - should have been caught by type checker")
  }

  def getType(env: Env, id: TypedTessla.Identifier) = env.get(id).map(_.entry) match {
    case Some(Translated(Lazy(TypeEntry(typ)))) => typ
    case _ =>
      // TODO: Possibly incorrect type information in generated TesslaCore
      TesslaCore.BuiltInType("???", Seq())
  }

  def translateValueType(typ: TypedTessla.Type, env: Env): TesslaCore.ValueType = typ match {
    case b: TypedTessla.BuiltInType if b.isValueType => TesslaCore.BuiltInType(b.name, b.typeArgs.map(translateValueType(_, env)))
    case _: TypedTessla.BuiltInType =>
      throw InternalError(s"Non-value type where value type expected - should have been caught by type checker")
    case ot : TypedTessla.ObjectType =>
      TesslaCore.ObjectType(mapValues(ot.memberTypes)(translateValueType(_, env)))
    case _ : TypedTessla.FunctionType =>
      throw InternalError(s"Function type where value type expected - should have been caught by type checker")
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
    case Tessla.FloatLiteral(f) =>
      TesslaCore.FloatValue(f, loc)
    case Tessla.StringLiteral(str) =>
      TesslaCore.StringValue(str, loc)
  }

  def translateEntry(env: Env, wrapper: EnvEntryWrapper): Unit = wrapper.entry match {
    case NotYetTranslated(entry, closure, inFunction) =>
      // TODO: replace with just entry.id.nameOpt once the resultWrapper-hack is fixed
      val nameOpt = Option(entry.id).flatMap(_.nameOpt)
      translateExpression(closure, wrapper, entry.expression, nameOpt, entry.typeInfo, inFunction, entry.annotations)
    case Translating(loc) =>
      throw InfiniteRecursion(loc)
    case _ =>
      /* Do nothing */
  }

  def isValueFunction(mac: TypedTessla.Macro): Boolean = {
    isValueCompatibleType(mac.returnType) && mac.parameters.forall(p => isValueCompatibleType(p.parameterType))
  }

  def translateExpression(env: Env, wrapper: EnvEntryWrapper, expression: TypedTessla.Expression, nameOpt: Option[String],
                          typ: TypedTessla.Type, inFunction: Boolean, annotations: Seq[TesslaCore.Annotation]): Unit = {
    wrapper.entry = Translating(expression.loc)
    expression match {
      case b: TypedTessla.BuiltInOperator if b.name == "true" =>
        wrapper.entry = Translated(Lazy(ValueEntry(TesslaCore.BoolValue(true, b.loc))))
      case b: TypedTessla.BuiltInOperator if b.name == "false" =>
        wrapper.entry = Translated(Lazy(ValueEntry(TesslaCore.BoolValue(false, b.loc))))
      case b: TypedTessla.BuiltInOperator =>
        wrapper.entry = Translated(Lazy(BuiltInEntry(b, typ)))
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
          translatedStreams(id) = TesslaCore.StreamDescription(id, coreExp, translatedType, annotations)
          wrapper.entry
        }
        // No checks regarding arity or non-existing or duplicate named arguments because those mistakes
        // would be caught by the type checker
        val callee = translateVar(env, call.macroID, call.macroLoc)

        def applyMacro(me: MacroEntry): Unit = {
          var posArgIdx = 0
          val args = call.args.map {
            case arg: TypedTessla.PositionalArgument =>
              val param = me.mac.parameters(posArgIdx)
              posArgIdx += 1
              param.id -> env(arg.id)
            case arg: TypedTessla.NamedArgument =>
              me.mac.parameters.find(_.name == arg.name) match {
                case Some(param) => param.id -> env(arg.id)
                case None => throw UndefinedNamedArg(arg.name, arg.idLoc.loc)
              }
          }.toMap
          val defsWithoutParameters = new TypedTessla.Definitions(me.mac.body.parent)
          me.mac.body.variables.foreach {
            case (_, entry) =>
              entry.expression match {
                case _: TypedTessla.Parameter => // do nothing
                case _ => defsWithoutParameters.addVariable(entry)
              }
          }


          if (me.mac.returnType.isStreamType) {
            stream {
              val innerEnv = createEnvForDefs(defsWithoutParameters, me.closure ++ args)
              translateVar(innerEnv, me.mac.result.id, me.mac.result.loc) match {
                case Translated(Lazy(t: StreamEntry)) => translatedStreams(t.streamId).expression
                // TODO: Allow id1 = id2 in TeSSLa Core to get rid of this hack
                case Translated(Lazy(t: InputStreamEntry)) => TesslaCore.DefaultFrom(TesslaCore.InputStream(t.name, Location.unknown), TesslaCore.Nil(t.typ, Location.unknown), Location.unknown)
                case other => throw InternalError("Expected stream entry but got: " + other.toString)
              }
            }
          } else {
            stack.push(call.loc)
            val innerEnv = createEnvForDefs(defsWithoutParameters, me.closure ++ args)
            wrapper.entry = translateVar(innerEnv, me.mac.result.id, me.mac.result.loc)
            stack.pop()
          }
        }

        callee match {
          case Translated(Lazy(be: BuiltInEntry)) =>
            var posArgIdx = 0
            // This is lazy, so the arguments don't get evaluated until they're used below, allowing us to
            // initialize entries where appropriate before the evaluation takes place
            lazy val args = call.args.map {
              case arg: TypedTessla.PositionalArgument =>
                val param = be.parameters(posArgIdx)
                posArgIdx += 1
                param.id -> translateVar(env, arg.id, arg.loc)
              case arg: TypedTessla.NamedArgument =>
                be.parameters.find(_.name == arg.name) match {
                  case Some(param) => param.id -> translateVar(env, arg.id, arg.loc)
                  case None => throw UndefinedNamedArg(arg.name, arg.idLoc.loc)
                }
            }.toMap
            def argAt(i: Int) = args(be.parameters(i).id)
            def streamArg(i: Int) = getStream(argAt(i), call.args(i).loc)
            def arg(i: Int) = getArg(argAt(i), call.args(i).loc)
            be.name match {
              case "nil" =>
                val typ = TesslaCore.StreamType(translateValueType(call.typeArgs.head, env))
                wrapper.entry = Translated(Lazy(NilEntry(TesslaCore.Nil(typ, call.loc), typ)))
              case "default" =>
                stream {
                  TesslaCore.Default(streamArg(0), getValue(argAt(1)), call.loc)
                }
              case "defaultFrom" =>
                stream {
                  TesslaCore.DefaultFrom(streamArg(0), streamArg(1), call.loc)
                }
              case "last" =>
                stream {
                  TesslaCore.Last(streamArg(0), streamArg(1), call.loc)
                }
              case "delay" =>
                stream {
                  TesslaCore.Delay(streamArg(0), streamArg(1), call.loc)
                }
              case "time" =>
                stream {
                  TesslaCore.Time(streamArg(0), call.loc)
                }
              case "lift" =>
                stream {
                  val f = getFunctionForLift(env, call.args(2).id, call.args(2).loc)
                  val liftArgs = Seq(getStream(argAt(0), call.args(0).loc), getStream(argAt(1), call.args(1).loc))
                  TesslaCore.Lift(f, liftArgs, call.loc)
                }
              case op =>
                if (typ.isStreamType) {
                  if (be.typ.isLiftableFunctionType) {
                    stream {
                      TesslaCore.SignalLift(CurriedPrimitiveOperator(op), (0 until args.size).map(streamArg), call.loc)
                    }
                  } else {
                    stream {
                      TesslaCore.CustomBuiltInCall(op, (0 until args.size).map(arg), call.loc)
                    }
                  }
                } else {
                  be.builtIn.referenceImplementation match {
                    case None =>
                      val value = Evaluator.evalPrimitiveOperator(op, (0 until args.size).map(i => getValue(argAt(i))), call.loc)
                      wrapper.entry = Translated(Lazy(ValueEntry(value)))
                    case Some(refImpl) =>
                      val me = translateVar(env, refImpl, call.macroLoc) match {
                        case Translated(Lazy(me: MacroEntry)) => me
                        case _ => throw InternalError(s"Reference implementation of called built-in resolved to non-macro entry", call.macroLoc)
                      }
                      applyMacro(me)
                  }
                }
            }

          case Translated(Lazy(me: MacroEntry)) =>
            applyMacro(me)
          case other =>
            throw InternalError(s"Applying non-macro/builtin (${other.getClass.getSimpleName}) - should have been caught by the type checker.")
        }
    }
  }

  def getFunctionForLift(env: Env, id: TypedTessla.Identifier, loc: Location): TesslaCore.Function = {
    translateVar(env, id, loc) match {
      case Translated(Lazy(me: MacroEntry)) =>
        me.functionValue match {
          case Some(f) => f.function
          case None => throw InternalError("Lifting non-value macro - should have been caught by type checker")
        }
      case Translated(Lazy(BuiltInEntry(b, typ: TypedTessla.FunctionType))) =>
        val arity = typ.parameterTypes.length
        val ids = (1 to arity).map(_ => makeIdentifier())
        val defsEntryID = makeIdentifier()
        val builtIn = TesslaCore.BuiltInOperator(b.name, loc)
        val app = TesslaCore.Application(Lazy(builtIn), ids.map(id => Lazy(TesslaCore.ValueExpressionRef(id))), loc)
        TesslaCore.Function(ids, Map(defsEntryID -> app), TesslaCore.ValueExpressionRef(defsEntryID), loc)
      case other => throw InternalError(s"Wrong type of environment entry: Expected macro or primitive function, found: $other")
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
      case Translated(Lazy(be: BuiltInEntry)) =>
        TesslaCore.BuiltInOperator(be.name, loc)
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
    case t if t.isStreamType =>
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
    val defsNotYetTranslateds = mac.body.variables.values.filter { entry =>
      !entry.expression.isInstanceOf[TypedTessla.Parameter] && isValueCompatibleType(entry.typeInfo)
    }.map { entry =>
      NotYetTranslated(entry, null, inFunction = true)
    }
    val defsWrappers = defsNotYetTranslateds.map(nyt => nyt.entry.id -> EnvEntryWrapper(nyt))
    val env: Env = closure ++ mapValues(params.toMap) { newId =>
        EnvEntryWrapper(Translated(Lazy(FunctionParameterEntry(newId))))
    } ++ defsWrappers
    defsNotYetTranslateds.foreach(_.closure = env)
    defsWrappers.foreach {
      case (_, wrapper) =>
        translateEntry(env, wrapper)
    }
    val defs = defsWrappers.map(_._2.entry).toSeq.flatMap {
      case Translated(Lazy(fe: FunctionEntry)) =>
        Some(fe.id -> fe.f)
      case Translated(Lazy(ae: ValueExpressionEntry)) =>
        Some(ae.id -> ae.exp)
      case _ =>
        None
    }.toMap
    TesslaCore.Function(params.map(_._2), defs, getValueArg(env(mac.result.id).entry), mac.loc)
  }

  def getValueArg(entry: EnvEntry): TesslaCore.ValueArg = entry match {
    case Translated(Lazy(be: BuiltInEntry)) =>
      TesslaCore.BuiltInOperator(be.name, Location.builtIn)
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
    case other => throw InternalError(s"Wrong type of environment entry: Expected stream entry, found: $other")
  }

  def getArg(envEntry: EnvEntry, loc: Location): TesslaCore.Arg = getResult(envEntry).get match {
    case _: StreamEntry | _: NilEntry | _: InputStreamEntry => getStream(envEntry, loc)
    case _: ValueEntry | _: MacroEntry => getValue(envEntry)
    case other => throw InternalError(s"Wrong type of environment entry: Expected stream or value entry, found: $other")
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
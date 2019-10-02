package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util._
import scala.util.Try

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit], evaluator: Evaluator) extends TranslationPhase[TypedTessla.TypedSpecification, TesslaCore.Specification] {
  override def translate(spec: TypedTessla.TypedSpecification) = {
    new ConstantEvaluatorWorker(spec, baseTimeUnit, evaluator).translate()
  }
}

class ConstantEvaluatorWorker(spec: TypedTessla.TypedSpecification, baseTimeUnit: Option[TimeUnit], evaluator: Evaluator)
  extends TesslaCore.IdentifierFactory with TranslationPhase.Translator[TesslaCore.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntry]
  type TypeEnv = Map[TypedTessla.Identifier, TesslaCore.ValueType]
  private val translatedStreams = mutable.Map[TesslaCore.Identifier, TesslaCore.StreamDescription]()
  private val deferredQueue = mutable.Queue[() => Unit]()

  case class EnvEntry(result: LazyWithStack[TranslationResult, Location], loc: Location)

  sealed abstract class TranslationResult

  case class StreamEntry(streamId: TesslaCore.Identifier, typ: TesslaCore.StreamType) extends TranslationResult

  case class InputStreamEntry(name: String, typ: TesslaCore.StreamType) extends TranslationResult

  case class NilEntry(nil: TesslaCore.Nil, typ: TesslaCore.StreamType) extends TranslationResult

  case class ValueEntry(value: TesslaCore.ValueOrError) extends TranslationResult

  case class ObjectEntry(members: Map[String, TranslationResult], loc: Location) extends TranslationResult

  case class FunctionEntry(f: TesslaCore.Function, id: TesslaCore.Identifier) extends TranslationResult

  case class MacroEntry(mac: TypedTessla.Macro, closure: Env, functionValue: Option[TesslaCore.Closure]) extends TranslationResult {
    override def toString = s"MacroEntry($mac, ...)"
  }

  case class BuiltInEntry(builtIn: TypedTessla.BuiltInOperator, typ: TypedTessla.Type) extends TranslationResult {
    def name = builtIn.name

    def parameters = builtIn.parameters
  }

  case class FunctionParameterEntry(id: TesslaCore.Identifier) extends TranslationResult

  case class ValueExpressionEntry(exp: TesslaCore.ValueExpression, id: TesslaCore.Identifier, typ: TesslaCore.ValueType) extends TranslationResult

  override def translateSpec(): TesslaCore.Specification = {
    val env = createEnvForDefs(spec.globalDefs, Map(), Map())
    val resultEnv = translateEnv(env, Nil)
    val inputStreams = spec.globalDefs.variables.collect {
      case (_, TypedTessla.VariableEntry(_, is: TypedTessla.InputStream, typ, annotations, _)) =>
        TesslaCore.InStreamDescription(is.name, translateStreamType(typ, Map()), annotations, is.loc)
    }
    val outputStreams = spec.outStreams.map { os =>
      val s = getStream(resultEnv(os.id), os.loc)
      TesslaCore.OutStreamDescription(os.nameOpt, s, getStreamType(resultEnv(os.id)))
    }
    while (deferredQueue.nonEmpty) {
      deferredQueue.dequeue()()
    }
    TesslaCore.Specification(translatedStreams.values.toSeq, inputStreams.toSeq, outputStreams, identifierCounter)
  }

  def createEnvForDefs(defs: TypedTessla.Definitions, parent: Env, typeEnv: TypeEnv): Env = {
    lazy val entries: Env = parent ++ mapValues(defs.variables) {entry =>
      EnvEntry(LazyWithStack {stack: List[Location] =>
        translateExpression(entries, typeEnv, entry.expression, entry.id.nameOpt, entry.typeInfo, inFunction = false, entry.annotations, stack)
      }, entry.expression.loc)
    }.toMap
    entries
  }

  def translateEnv(env: Env, stack: List[Location]) = mapValues(env) { entryWrapper =>
    translateEntry(entryWrapper, stack)
  }

  def translateStreamType(typ: TypedTessla.Type, typeEnv: TypeEnv) = typ match {
    case b@TypedTessla.BuiltInType(_, Seq(elementType)) if b.isStreamType =>
      TesslaCore.StreamType(translateValueType(elementType, typeEnv))
    case _ =>
      throw InternalError(s"Expected stream type, got $typ - should have been caught by type checker")
  }

  def translateValueType(typ: TypedTessla.Type, typeEnv: TypeEnv): TesslaCore.ValueType = typ match {
    case b: TypedTessla.BuiltInType if b.isValueType =>
      TesslaCore.BuiltInType(b.name, b.typeArgs.map(translateValueType(_, typeEnv)))
    case otherBuiltIn: TypedTessla.BuiltInType =>
      throw InternalError(s"Non-value type ($otherBuiltIn) where value type expected - should have been caught by type checker")
    case ot: TypedTessla.ObjectType =>
      TesslaCore.ObjectType(mapValues(ot.memberTypes)(translateValueType(_, typeEnv)))
    case ft: TypedTessla.FunctionType =>
      TesslaCore.FunctionType
    case tvar: TypedTessla.TypeParameter =>
      typeEnv.getOrElse(tvar.id, TesslaCore.UnresolvedGenericType)
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

  def translateEntry(wrapper: EnvEntry, stack: List[Location]): TranslationResult = if (stack.size > 1000) {
    throw WithStackTrace(Errors.StackOverflow(stack.head), stack.tail)
  } else if (wrapper.result.isComputing) {
    throw WithStackTrace(InfiniteRecursion(wrapper.loc), stack)
  } else {
    try {
      wrapper.result.get(stack)
    } catch {
      case err: InternalError =>
        throw WithStackTrace(err, stack)
      case _: StackOverflowError =>
        throw WithStackTrace(Errors.StackOverflow(stack.head), stack.tail)
    }
  }

  def isValueFunction(mac: TypedTessla.Macro): Boolean = {
    isValueCompatibleType(mac.returnType) && mac.parameters.forall(p => isValueCompatibleType(p.parameterType))
  }

  def translateExpression(env: Env, typeEnv: TypeEnv, expression: TypedTessla.Expression, nameOpt: Option[String],
    typ: TypedTessla.Type, inFunction: Boolean, annotations: Seq[TesslaCore.Annotation], stack: List[Location]
  ): TranslationResult = {
    expression match {
      case b: TypedTessla.BuiltInOperator if b.name == "true" =>
        ValueEntry(TesslaCore.BoolValue(true, b.loc))
      case b: TypedTessla.BuiltInOperator if b.name == "false" =>
        ValueEntry(TesslaCore.BoolValue(false, b.loc))
      case b: TypedTessla.BuiltInOperator =>
        BuiltInEntry(b, typ)
      case mac: TypedTessla.Macro if inFunction =>
        val f = translateFunction(env, typeEnv, mac, stack)
        FunctionEntry(f, makeIdentifier(nameOpt))
      case mac: TypedTessla.Macro =>
        val functionValue = optionIf(isValueFunction(mac)) {
          val f = translateFunction(env, typeEnv, mac, stack)
          TesslaCore.Closure(f, Map(), mac.loc)
        }
        MacroEntry(mac, env, functionValue)
      case TypedTessla.Literal(lit, loc) =>
        // Evaluate the literal outside of the Lazy because we want TimeUnit-errors to appear even if
        // the expression is not used
        // TimeUnit-errors inside of macros *are* swallowed if the macro is never called, which is what
        // we want because a library might define macros using time units and, as long as you don't call
        // those macros, you should still be able to use the library when you don't have a time unit set.
        val translatedLit = translateLiteral(lit, loc)
        ValueEntry(translatedLit)
      case p: TypedTessla.Parameter =>
        translateVar(env, p.id, p.loc, stack)
      case v: TypedTessla.Variable =>
        translateVar(env, v.id, v.loc, stack)
      case i: TypedTessla.InputStream =>
        val typ = translateStreamType(i.streamType, typeEnv)
        InputStreamEntry(i.name, typ)
      case ite: TypedTessla.StaticIfThenElse if inFunction =>
        val cond = getValueArg(translateVar(env, ite.condition.id, ite.condition.loc, stack))
        val thenCase = Lazy(getValueArg(translateVar(env, ite.thenCase.id, ite.thenCase.loc, stack)))
        val elseCase = Lazy(getValueArg(translateVar(env, ite.elseCase.id, ite.elseCase.loc, stack)))
        val id = makeIdentifier(nameOpt)
        ValueExpressionEntry(TesslaCore.IfThenElse(cond, thenCase, elseCase, ite.loc), id, translateValueType(typ, typeEnv))
      case ite: TypedTessla.StaticIfThenElse =>
        val cond = translateVar(env, ite.condition.id, ite.condition.loc, stack)
        try {
          if (Evaluator.getBool(getValue(cond).forceValue)) {
            translateVar(env, ite.thenCase.id, ite.thenCase.loc, stack)
          } else {
            translateVar(env, ite.elseCase.id, ite.elseCase.loc, stack)
          }
        } catch {
          case e: TesslaError =>
            ValueEntry(TesslaCore.Error(e))
        }
      case obj: TypedTessla.ObjectLiteral =>
        ObjectEntry(mapValues(obj.members) { member =>
          translateVar(env, member.id, member.loc, stack)
        }, obj.loc)
      case acc: TypedTessla.MemberAccess if inFunction =>
        val arg = getValueArg(translateVar(env, acc.receiver.id, acc.loc, stack))
        val ma = TesslaCore.MemberAccess(arg, acc.member, acc.loc)
        ValueExpressionEntry(ma, makeIdentifier(nameOpt), translateValueType(typ, typeEnv))
      case acc: TypedTessla.MemberAccess =>
        translateVar(env, acc.receiver.id, acc.loc, stack) match {
          case obj: ObjectEntry =>
            obj.members(acc.member)
          case ValueEntry(obj: TesslaCore.TesslaObject) =>
            ValueEntry(obj.value(acc.member))
          case ValueEntry(er: TesslaCore.Error) =>
            ValueEntry(er)
          case other =>
            throw InternalError(s"Member access on non-object ($other) should've been caught by type checker", acc.receiver.loc)
        }
      case call: TypedTessla.MacroCall if inFunction =>
        val args = call.args.map(arg => getValueArg(translateVar(env, arg.id, arg.loc, stack)))
        val id = makeIdentifier(nameOpt)
        ValueExpressionEntry(TesslaCore.Application(Lazy(getFunction(env, call.macroID, call.loc, stack)), args, call.loc), id, translateValueType(typ, typeEnv))
      case call: TypedTessla.MacroCall =>
        // No checks regarding arity or non-existing or duplicate named arguments because those mistakes
        // would be caught by the type checker
        val callee = translateVar(env, call.macroID, call.macroLoc, stack)

        def applyMacro(me: MacroEntry): TranslationResult = {
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

          val innerTypeEnv: TypeEnv = me.mac.typeParameters.zip(call.typeArgs.map(translateValueType(_, typeEnv))).toMap
          val innerEnv = createEnvForDefs(defsWithoutParameters, me.closure ++ args, innerTypeEnv)
          translateVar(innerEnv, me.mac.result.id, me.mac.result.loc, call.loc :: stack)
        }

        callee match {
          case be: BuiltInEntry =>
            val innterTypeEnv: TypeEnv = be.builtIn.typeParameters.zip(call.typeArgs.map(translateValueType(_, typeEnv))).toMap
            // Lazy because we don't want translateStreamType to be called when not dealing wiht streams
            lazy val translatedType = translateStreamType(typ, typeEnv ++ innterTypeEnv)
            var posArgIdx = 0
            // This is lazy, so the arguments don't get evaluated until they're used below, allowing us to
            // initialize entries where appropriate before the evaluation takes place
            val args = call.args.map {
              case arg: TypedTessla.PositionalArgument =>
                val param = be.parameters(posArgIdx)
                posArgIdx += 1
                param.id -> Lazy(translateVar(env, arg.id, arg.loc, stack))
              case arg: TypedTessla.NamedArgument =>
                be.parameters.find(_.name == arg.name) match {
                  case Some(param) => param.id -> Lazy(translateVar(env, arg.id, arg.loc, stack))
                  case None => throw UndefinedNamedArg(arg.name, arg.idLoc.loc)
                }
            }.toMap

            def argAt(i: Int) = args(be.parameters(i).id).get

            def streamArg(i: Int) = getStream(argAt(i), call.args(i).loc)

            def arg(i: Int) = getArg(argAt(i), call.args(i).loc)


            val id = makeIdentifier(nameOpt)
            be.name match {
              case "nil" =>
                val typ = TesslaCore.StreamType(translateValueType(call.typeArgs.head, typeEnv))
                NilEntry(TesslaCore.Nil(typ, call.loc), typ)
              case "default" =>
                translatedStreams(id) = TesslaCore.StreamDescription(id, TesslaCore.Default(streamArg(0), getValue(argAt(1)), call.loc), translatedType, annotations)
                StreamEntry(id, translatedType)
              case "defaultFrom" =>
                translatedStreams(id) = TesslaCore.StreamDescription(id, TesslaCore.DefaultFrom(streamArg(0), streamArg(1), call.loc), translatedType, annotations)
                StreamEntry(id, translatedType)
              case "last" =>
                val strictArg = streamArg(1)
                deferredQueue += (() => translatedStreams(id) = TesslaCore.StreamDescription(id, TesslaCore.Last(streamArg(0), strictArg, call.loc), translatedType, annotations))
                StreamEntry(id, translatedType)
              case "delay" =>
                val id = makeIdentifier(nameOpt)
                val strictArg = streamArg(1)
                deferredQueue += (() => translatedStreams(id) = TesslaCore.StreamDescription(id, TesslaCore.Delay(streamArg(0), strictArg, call.loc), translatedType, annotations))
                StreamEntry(id, translatedType)
              case "time" =>
                translatedStreams(id) = TesslaCore.StreamDescription(id, TesslaCore.Time(streamArg(0), call.loc), translatedType, annotations)
                StreamEntry(id, translatedType)
              case "lift" =>
                val f = getFunctionForLift(env, call.args(2).id, translateStreamType(typ, typeEnv).elementType, call.args(2).loc, stack)
                val liftArgs = Seq(getStream(argAt(0), call.args(0).loc), getStream(argAt(1), call.args(1).loc))
                translatedStreams(id) = TesslaCore.StreamDescription(id, TesslaCore.Lift(f, liftArgs, call.loc), translatedType, annotations)
                StreamEntry(id, translatedType)
              case op =>
                if (typ.isStreamType) {
                  translatedStreams(id) = TesslaCore.StreamDescription(id, TesslaCore.CustomBuiltInCall(op, (0 until args.size).map(arg), call.loc), translatedType, annotations)
                  StreamEntry(id, translatedType)
                } else {
                  be.builtIn.referenceImplementation match {
                    case None =>
                      val resultType = translateValueType(typ, typeEnv)
                      val argList = (0 until args.size).map(i => getValue(argAt(i)))
                      val value = evaluator.evalPrimitiveOperator(op, argList, resultType, call.loc)
                      ValueEntry(value)
                    case Some(refImpl) =>
                      val me = translateVar(env, refImpl, call.macroLoc, stack) match {
                        case me: MacroEntry => me
                        case _ => throw InternalError(s"Reference implementation of called built-in resolved to non-macro entry", call.macroLoc)
                      }
                      applyMacro(me)
                  }
                }
            }

          case me: MacroEntry =>
            applyMacro(me)
          case other =>
            throw InternalError(s"Applying non-macro/builtin (${other.getClass.getSimpleName}) - should have been caught by the type checker.")
        }
    }
  }

  def getFunctionForLift(env: Env, id: TypedTessla.Identifier, returnType: TesslaCore.ValueType, loc: Location, stack: List[Location]): TesslaCore.Function = {
    translateVar(env, id, loc, stack) match {
      case me: MacroEntry =>
        me.functionValue match {
          case Some(f) => f.function
          case None => throw InternalError("Lifting non-value macro - should have been caught by type checker")
        }
      case BuiltInEntry(b, ft: TypedTessla.FunctionType) =>
        val arity = ft.parameterTypes.length
        val ids = (1 to arity).map(_ => makeIdentifier())
        val defsEntryID = makeIdentifier()
        val builtIn = TesslaCore.BuiltInOperator(b.name, loc)
        val app = TesslaCore.ValueExpressionDescription(
          TesslaCore.Application(Lazy(builtIn), ids.map(id => TesslaCore.ValueExpressionRef(id)), loc),
          returnType
        )
        TesslaCore.Function(ids, Map(defsEntryID -> app), TesslaCore.ValueExpressionRef(defsEntryID), loc)
      case other => throw InternalError(s"Wrong type of environment entry: Expected macro or primitive function, found: $other")
    }

  }

  def getFunction(env: Env, id: TypedTessla.Identifier, loc: Location, stack: List[Location]): TesslaCore.ValueArg = {
    translateVar(env, id, loc, stack) match {
      case me: MacroEntry =>
        me.functionValue match {
          case Some(f) => f
          case None => throw InternalError("Lifting non-value macro - should have been caught by type checker")
        }
      case fe: FunctionEntry =>
        TesslaCore.ValueExpressionRef(fe.id)
      case be: BuiltInEntry =>
        TesslaCore.BuiltInOperator(be.name, loc)
      case param: FunctionParameterEntry =>
        TesslaCore.ValueExpressionRef(param.id)
      case vee: ValueExpressionEntry =>
        TesslaCore.ValueExpressionRef(vee.id)
      case other => throw InternalError(s"Wrong type of environment entry: Expected macro or primitive function, found: $other")
    }
  }

  /** *
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

  def translateFunction(closure: Env, typeEnv: TypeEnv, mac: TypedTessla.Macro, stack: List[Location]): TesslaCore.Function = {
    val params = mac.parameters.map { param =>
      param.id -> makeIdentifier(param.name)
    }

    lazy val innerEnv: Env = closure ++ mapValues(params.toMap) { newId =>
      EnvEntry(LazyWithStack(_ => FunctionParameterEntry(newId)), Location.unknown)
    } ++  env
    lazy val env = mac.body.variables.values.filter { entry =>
      !entry.expression.isInstanceOf[TypedTessla.Parameter] && isValueCompatibleType(entry.typeInfo)
    }.map { entry =>
      entry.id -> EnvEntry(LazyWithStack {stack: List[Location] =>
        translateExpression(innerEnv, typeEnv, entry.expression, entry.id.nameOpt, entry.typeInfo, inFunction = true, entry.annotations, stack)
      }, entry.expression.loc)
    }.toMap

    val translatedDefs = translateEnv(env, stack)
    val defs = translatedDefs.values.flatMap {
      case fe: FunctionEntry =>
        Some(fe.id -> TesslaCore.ValueExpressionDescription(fe.f, TesslaCore.FunctionType))
      case ae: ValueExpressionEntry =>
        Some(ae.id -> TesslaCore.ValueExpressionDescription(ae.exp, ae.typ))
      case _ =>
        None
    }.toMap
    TesslaCore.Function(params.map(_._2), defs, getValueArg(innerEnv(mac.result.id).result.get(stack)), mac.loc)
  }

  def getValueArg(entry: TranslationResult): TesslaCore.ValueArg = entry match {
    case be: BuiltInEntry =>
      TesslaCore.BuiltInOperator(be.name, Location.builtIn)
    case me: MacroEntry =>
      me.functionValue match {
        case Some(f) => f
        case None => throw InternalError("Expected value-level entry, but found non-function macro entry")
      }
    case fe: FunctionEntry =>
      TesslaCore.ValueExpressionRef(fe.id)
    case ve: ValueEntry =>
      ve.value
    case ae: ValueExpressionEntry =>
      TesslaCore.ValueExpressionRef(ae.id)
    case param: FunctionParameterEntry =>
      TesslaCore.ValueExpressionRef(param.id)
    case oe: ObjectEntry =>
      val members = oe.members.flatMap {
        case (name, entry) =>
          // Filter out the object entries that aren't valid ValueArgs
          Try(name -> getValueArg(entry)).toOption
      }
      TesslaCore.ObjectCreation(members, oe.loc)
    case _ =>
      throw InternalError(s"Expected value-level entry, but found $entry")
  }

  def getStream(result: TranslationResult, loc: Location): TesslaCore.StreamRef = result match {
    case s: StreamEntry => TesslaCore.Stream(s.streamId, s.typ, loc)
    case n: NilEntry => n.nil
    case i: InputStreamEntry => TesslaCore.InputStream(i.name, i.typ, loc)
    case other => throw InternalError(s"Wrong type of environment entry: Expected stream entry, found: $other")
  }

  def getArg(result: TranslationResult, loc: Location): TesslaCore.Arg = result match {
    case _: StreamEntry | _: NilEntry | _: InputStreamEntry => getStream(result, loc)
    case other => getValue(other)
  }

  def getStreamType(result: TranslationResult): TesslaCore.StreamType = result match {
    case s: StreamEntry => s.typ
    case n: NilEntry => n.typ
    case i: InputStreamEntry => i.typ
    case other => throw InternalError(s"Wrong type of environment entry: Expected StreamEntry, found: $other")
  }

  def getValue(result: TranslationResult): TesslaCore.ValueOrError = {
    try {
      result match {
        case ve: ValueEntry => ve.value
        case oe: ObjectEntry =>
          val members = mapValues(oe.members)(v => getValue(v).forceValue)
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

  def translateVar(env: Env, id: TypedTessla.Identifier, loc: Location, stack: List[Location]) = {
    translateEntry(env(id), if (id.nameOpt.isDefined) loc :: stack else stack)
  }
}

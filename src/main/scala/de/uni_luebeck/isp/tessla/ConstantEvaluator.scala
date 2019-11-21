package de.uni_luebeck.isp.tessla

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util._

import scala.util.Try
import TesslaAST.Core
import TesslaAST.Identifier

class ConstantEvaluator(baseTimeUnit: Option[TimeUnit], evaluator: Evaluator) extends TranslationPhase[TypedTessla.TypedSpecification, Core.Specification] {
  override def translate(spec: TypedTessla.TypedSpecification) = {
    new ConstantEvaluatorWorker(spec, baseTimeUnit, evaluator).translate()
  }
}

class ConstantEvaluatorWorker(spec: TypedTessla.TypedSpecification, baseTimeUnit: Option[TimeUnit], evaluator: Evaluator)
  extends TranslationPhase.Translator[Core.Specification] {
  type Env = Map[TypedTessla.Identifier, EnvEntry]
  type TypeEnv = Map[TypedTessla.Identifier, TesslaCore.ValueType]

  class TranslatedExpressions {
    val expressions = mutable.Map[Identifier, Core.Expression]()
    val deferredQueue = mutable.Queue[() => Unit]()

    def complete(): Unit = {
      while (deferredQueue.nonEmpty) {
        deferredQueue.dequeue()()
      }
    }
  }

  case class EnvEntry(result: LazyWithStack[TranslationResult, Location], loc: Location)

  sealed abstract class TranslationResult

  case class StreamEntry(streamId: Identifier) extends TranslationResult

  case class InputStreamEntry(name: String) extends TranslationResult

  case class NilEntry(nil: Core.Expression) extends TranslationResult

  case class ValueEntry(value: TesslaCore.ValueOrError) extends TranslationResult

  case class ObjectEntry(members: Map[String, TranslationResult], loc: Location) extends TranslationResult

  case class FunctionEntry(f: Core.FunctionExpression, id: Identifier) extends TranslationResult

  case class MacroEntry(mac: TypedTessla.Macro, closure: Env, functionValue: Option[Core.ExpressionRef], translatedExpressions: TranslatedExpressions) extends TranslationResult {
    override def toString = s"MacroEntry($mac, ...)"
  }

  case class BuiltInEntry(builtIn: TypedTessla.BuiltInOperator, typ: TypedTessla.Type) extends TranslationResult {
    def name = builtIn.name

    def parameters = builtIn.parameters
  }

  sealed abstract class ExpressionEntry extends TranslationResult

  case class FunctionParameterEntry(id: Identifier) extends ExpressionEntry

  case class ValueExpressionEntry(exp: Core.Expression, id: Identifier) extends ExpressionEntry

  override def translateSpec(): Core.Specification = {
    val translatedExpressions = new TranslatedExpressions
    val env = createEnvForDefs(spec.globalDefs, Map(), Map(), translatedExpressions)
    val resultEnv = translateEnv(env, Nil)
    val inputStreams = spec.globalDefs.variables.collect {
      case (_, TypedTessla.VariableEntry(_, is: TypedTessla.InputStream, typ, annotations, _)) =>
        Identifier(is.name, is.loc) -> (null, Nil)
    }.toMap
    val outputStreams = spec.outStreams.toList.map { os =>
      val s = getStream(resultEnv(os.id), os.loc)
      (s.asInstanceOf[Core.ExpressionRef].id, os.nameOpt, Nil)
    }

    translatedExpressions.complete()

    Core.Specification(inputStreams, translatedExpressions.expressions.toMap, outputStreams)
  }

  def createEnvForDefs(defs: TypedTessla.Definitions, parent: Env, typeEnv: TypeEnv, translatedFunctions: TranslatedExpressions): Env = {
    lazy val entries: Env = parent ++ mapValues(defs.variables) { entry =>
      EnvEntry(LazyWithStack { stack: List[Location] =>
        translateExpression(entries, typeEnv, entry.expression, entry.id.nameOpt, entry.typeInfo, inFunction = false, stack, translatedFunctions)
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
    typ: TypedTessla.Type, inFunction: Boolean, stack: List[Location],
    translatedExpressions: TranslatedExpressions
  ): TranslationResult = {
    expression match {
      case b: TypedTessla.BuiltInOperator if b.name == "true" =>
        ValueEntry(TesslaCore.BoolValue(true, b.loc))
      case b: TypedTessla.BuiltInOperator if b.name == "false" =>
        ValueEntry(TesslaCore.BoolValue(false, b.loc))
      case b: TypedTessla.BuiltInOperator =>
        BuiltInEntry(b, typ)
      case mac: TypedTessla.Macro =>
        val functionValue = optionIf(isValueFunction(mac)) {
          translateFunction(env, typeEnv, mac, stack, translatedExpressions, nameOpt)
        }
        MacroEntry(mac, env, functionValue, translatedExpressions)
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
        val typ = null //translateStreamType(i.streamType, typeEnv)
        InputStreamEntry(i.name)
      case ite: TypedTessla.StaticIfThenElse =>
        val cond = translateVar(env, ite.condition.id, ite.condition.loc, stack)
        if (cond.isInstanceOf[ExpressionEntry]) {
          val cond = getValueArg(translateVar(env, ite.condition.id, ite.condition.loc, stack))
          val thenCase = getValueArg(translateVar(env, ite.thenCase.id, ite.thenCase.loc, stack))
          val elseCase = getValueArg(translateVar(env, ite.elseCase.id, ite.elseCase.loc, stack))
          val id = makeIdentifier(nameOpt)
          val staticITEExternA = Identifier("A")
          val staticITEExtern = Core.ExternExpression(List(staticITEExternA),
            List(
              (Identifier("cond"), TesslaAST.StrictEvaluation, Core.BoolType),
              (Identifier("then"), TesslaAST.LazyEvaluation, Core.TypeParam(staticITEExternA, Location.unknown)),
              (Identifier("else"), TesslaAST.LazyEvaluation, Core.TypeParam(staticITEExternA, Location.unknown))),
            Core.TypeParam(staticITEExternA, Location.unknown), "staticite")
          ValueExpressionEntry(Core.ApplicationExpression(staticITEExtern, List(cond, thenCase, elseCase), ite.loc), id)
        } else {
          try {
            if (Evaluator.getBool(getValue(cond).forceValue)) {
              translateVar(env, ite.thenCase.id, ite.thenCase.loc, stack)
            } else {
              translateVar(env, ite.elseCase.id, ite.elseCase.loc, stack)
            }
          } catch {
            case e: TesslaError if !e.isInstanceOf[InternalError] =>
              ValueEntry(TesslaCore.Error(e))
          }
        }
      case obj: TypedTessla.ObjectLiteral =>
        ObjectEntry(mapValues(obj.members) { member =>
          translateVar(env, member.id, member.loc, stack)
        }, obj.loc)
      case acc: TypedTessla.MemberAccess =>
        translateVar(env, acc.receiver.id, acc.loc, stack) match {
          case obj: ObjectEntry =>
            obj.members(acc.member)
          case ValueEntry(obj: TesslaCore.TesslaObject) =>
            ValueEntry(obj.value(acc.member))
          case ValueEntry(er: TesslaCore.Error) =>
            ValueEntry(er)
          case e: ExpressionEntry =>
            val arg = getValueArg(e)
            val ma = Core.RecordAccesorExpression(Identifier(acc.member), arg, acc.loc)
            ValueExpressionEntry(ma, makeIdentifier(nameOpt))
          case other =>
            throw InternalError(s"Member access on non-object ($other) should've been caught by type checker", acc.receiver.loc)
        }
      case call: TypedTessla.MacroCall =>
        // No checks regarding arity or non-existing or duplicate named arguments because those mistakes
        // would be caught by the type checker
        lazy val callee = translateVar(env, call.macroID, call.macroLoc, stack)

        // TODO make less ugly !!!
        if ((inFunction || (callee.isInstanceOf[BuiltInEntry] && callee.asInstanceOf[BuiltInEntry].builtIn.name != "last" && callee.asInstanceOf[BuiltInEntry].builtIn.name != "delay")) && (call.args.exists { arg => translateVar(env, arg.id, arg.loc, stack).isInstanceOf[ExpressionEntry] } || callee.isInstanceOf[ExpressionEntry])) {
          val args = call.args.map(arg => getValueArg(translateVar(env, arg.id, arg.loc, stack))).toList
          val id = makeIdentifier(nameOpt)
          val function: Core.ExpressionArg = callee match {
            case me: MacroEntry =>
              me.functionValue match {
                case Some(f) => f
                case None => throw InternalError("Lifting non-value macro - should have been caught by type checker")
              }
            case fe: FunctionEntry =>
              Core.ExpressionRef(fe.id, null)
            case be: BuiltInEntry =>
              Core.ExternExpression(List(), be.parameters.toList.map(x => (Identifier(x.param.name), TesslaAST.LazyEvaluation, null)), null, be.name) // NOTE: call all builtins at runtime lazy for now as it is needed for some as ite.
            case param: FunctionParameterEntry =>
              Core.ExpressionRef(param.id, null)
            case vee: ValueExpressionEntry =>
              Core.ExpressionRef(vee.id, null)
            case other => throw InternalError(s"Wrong type of environment entry: Expected macro or primitive function, found: $other")
          }
          ValueExpressionEntry(Core.ApplicationExpression(function, args, call.loc), id)
        } else {

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
            val innerEnv = createEnvForDefs(defsWithoutParameters, me.closure ++ args, innerTypeEnv, me.translatedExpressions)
            translateVar(innerEnv, me.mac.result.id, me.mac.result.loc, call.loc :: stack)
          }

          callee match {
            case be: BuiltInEntry =>
              val innerTypeEnv: TypeEnv = be.builtIn.typeParameters.zip(call.typeArgs.map(translateValueType(_, typeEnv))).toMap
              // Lazy because we don't want translateStreamType to be called when not dealing with streams
              lazy val translatedType = translateStreamType(typ, typeEnv ++ innerTypeEnv)
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


              be.name match {
                case "nil" =>
                  val typ = TesslaCore.StreamType(translateValueType(call.typeArgs.head, typeEnv))
                  val nilExtern = Core.ExternExpression(List(Identifier("A")), Nil, Core.InstatiatedType("Events", List(Core.TypeParam(Identifier("A")))), "nil")
                  NilEntry(Core.ApplicationExpression(nilExtern, Nil))
                case "last" =>
                  val id = makeIdentifier(nameOpt)
                  val strictArg = streamArg(1)
                  val lastExternA = Identifier("A")
                  val lastExternB = Identifier("B")
                  val lastExtern = Core.ExternExpression(List(lastExternA, lastExternB), List(
                    (Identifier("value"), TesslaAST.LazyEvaluation, Core.InstatiatedType("Events", List(Core.TypeParam(lastExternA)))),
                    (Identifier("trigger"), TesslaAST.StrictEvaluation, Core.InstatiatedType("Events", List(Core.TypeParam(lastExternB))))
                  ), Core.InstatiatedType("Events", List(Core.TypeParam(lastExternA))), "last")
                  translatedExpressions.deferredQueue += (() => translatedExpressions.expressions(id) = Core.ApplicationExpression(lastExtern,
                    List(
                      streamArg(0),
                      strictArg),
                    call.loc))
                  StreamEntry(id)
                case "delay" =>
                  val delayExternA = Identifier("A")
                  val delayExtern = Core.ExternExpression(List(delayExternA), List(
                    (Identifier("amount"), TesslaAST.LazyEvaluation, Core.InstatiatedType("Events", List(Core.IntType))),
                    (Identifier("reset"), TesslaAST.StrictEvaluation, Core.InstatiatedType("Events", List(Core.TypeParam(delayExternA))))
                  ), Core.InstatiatedType("Events", List(Core.UnitType)), "delay")
                  val id = makeIdentifier(nameOpt)
                  val strictArg = streamArg(1)
                  translatedExpressions.deferredQueue += (() => translatedExpressions.expressions(id) = Core.ApplicationExpression(delayExtern,
                    List(
                      streamArg(0),
                      strictArg), call.loc))
                  StreamEntry(id)
                case op =>
                  if (typ.isStreamType) {
                    val id = makeIdentifier(nameOpt)
                    val extern = Core.ExternExpression(List(), be.parameters.toList.map(x => (Identifier(x.param.name), TesslaAST.StrictEvaluation, null)), null, op)
                    translatedExpressions.expressions(id) = Core.ApplicationExpression(extern, (0 until args.size).map(x => arg(x)).toList, call.loc)
                    StreamEntry(id)
                  } else {
                    be.builtIn.referenceImplementation match {
                      case None =>
                        val resultType = translateValueType(typ, typeEnv)
                        // TODO broken if argument is a function
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

  def translateFunction(closure: Env, typeEnv: TypeEnv, mac: TypedTessla.Macro, stack: List[Location], outerTranslatedFunction: TranslatedExpressions, nameOpt: Option[String]) = {
    val id = makeIdentifier(nameOpt)

    outerTranslatedFunction.deferredQueue += (() => {
      val translatedFunctions = new TranslatedExpressions

      val params = mac.parameters.toList.map { param =>
        param.id -> makeIdentifier(param.name)
      }

      lazy val innerEnv: Env = closure ++ mapValues(params.toMap) { newId =>
        EnvEntry(LazyWithStack(_ => FunctionParameterEntry(newId)), Location.unknown)
      } ++ env
      lazy val env = mac.body.variables.values.filter { entry =>
        !entry.expression.isInstanceOf[TypedTessla.Parameter] && isValueCompatibleType(entry.typeInfo)
      }.map { entry =>
        entry.id -> EnvEntry(LazyWithStack { stack: List[Location] =>
          translateExpression(innerEnv, typeEnv, entry.expression, entry.id.nameOpt, entry.typeInfo, inFunction = true, stack, translatedFunctions)
        }, entry.expression.loc)
      }.toMap

      val translatedDefs = translateEnv(env, stack)
      val defs = translatedDefs.values.flatMap {
        case fe: FunctionEntry =>
          Some(fe.id -> fe.f)
        case ae: ValueExpressionEntry =>
          Some(ae.id -> ae.exp)
        case _ =>
          None
      }.toMap

      translatedFunctions.complete()

      val f = Core.FunctionExpression(List(), params.map(x => (x._2, TesslaAST.StrictEvaluation, null)), defs ++ translatedFunctions.expressions, getValueArg(innerEnv(mac.result.id).result.get(stack)), mac.loc)
      outerTranslatedFunction.expressions += id -> f
    })

    Core.ExpressionRef(id, Core.FunctionType(Nil, mac.parameters.toList.map(_ => (TesslaAST.StrictEvaluation, null)), null)) // FIXME: this type is only a hack to encode the arity of the function without giving a real type
  }

  def getValueArg(entry: TranslationResult): Core.ExpressionArg = entry match {
    case be: BuiltInEntry =>
      Core.ExternExpression(Nil, be.parameters.toList.map(x => null), null, be.name, Location.builtIn)
    case me: MacroEntry =>
      me.functionValue match {
        case Some(f) => f
        case None => throw InternalError("Expected value-level entry, but found non-function macro entry")
      }
    case fe: FunctionEntry =>
      Core.ExpressionRef(fe.id, null, Location.unknown)
    case ve: ValueEntry =>
      reify(ve.value.forceValue)
    case ae: ValueExpressionEntry =>
      Core.ExpressionRef(ae.id, null, Location.unknown)
    case param: FunctionParameterEntry =>
      Core.ExpressionRef(param.id, null, Location.unknown)
    case oe: ObjectEntry =>
      val members = oe.members.flatMap {
        case (name, entry) =>
          // Filter out the object entries that aren't valid ValueArgs
          Try(Identifier(name) -> getValueArg(entry)).toOption
      }
      Core.RecordConstructorExpression(members, oe.loc)
    case _ =>
      throw InternalError(s"Expected value-level entry, but found $entry")
  }

  def getStream(result: TranslationResult, loc: Location): Core.ExpressionArg = result match {
    case s: StreamEntry => Core.ExpressionRef(s.streamId, null, loc)
    case n: NilEntry => n.nil
    case i: InputStreamEntry => Core.ExpressionRef(Identifier(i.name), null, loc)
    case other => throw InternalError(s"Wrong type of environment entry: Expected stream entry, found: $other")
  }

  def getArg(result: TranslationResult, loc: Location): Core.ExpressionArg = result match {
    case me: MacroEntry => Core.ExpressionRef(me.functionValue.get.id, null, me.mac.loc)
    case _: StreamEntry | _: NilEntry | _: InputStreamEntry => getStream(result, loc)
    case other => reify(getValue(other).forceValue)
  }

  def getValue(result: TranslationResult): TesslaCore.ValueOrError = {
    try {
      result match {
        case ve: ValueEntry => ve.value
        case oe: ObjectEntry =>
          val members = mapValues(oe.members)(v => getValue(v).forceValue)
          TesslaCore.TesslaObject(members, oe.loc)
        case be: BuiltInEntry =>
          TesslaCore.BuiltInOperator(be.name, Location.builtIn)
        case other => throw InternalError(s"Wrong type of environment entry: Expected ValueEntry, found: $other")
      }
    } catch {
      case e: TesslaError if !e.isInstanceOf[InternalError] =>
        TesslaCore.Error(e)
    }
  }

  def translateVar(env: Env, id: TypedTessla.Identifier, loc: Location, stack: List[Location]) = {
    translateEntry(env(id), if (id.nameOpt.isDefined) loc :: stack else stack)
  }

  private var counter = 0

  def makeIdentifier(name: String): Identifier = {
    makeIdentifier(Some(name))
  }

  def makeIdentifier(nameOpt: Option[String]): Identifier = {
    counter += 1
    new Identifier(nameOpt.getOrElse("") + "$" + counter)
  }

  val noneExtern = Core.ExternExpression(Nil, Nil, null, "None")
  val someExtern = Core.ExternExpression(Nil, List((Identifier("a"), TesslaAST.StrictEvaluation, null)), null, "Some")
  val setEmptyExtern = Core.ExternExpression(Nil, Nil, null, "Set_empty")
  val setAddExtern = Core.ExternExpression(Nil, List(
    (Identifier("a"), TesslaAST.StrictEvaluation, null),
    (Identifier("b"), TesslaAST.StrictEvaluation, null)
  ), null, "Set_add")
  val mapEmptyExtern = Core.ExternExpression(Nil, Nil, null, "Map_empty")
  val mapAddExtern = Core.ExternExpression(Nil, List(
    (Identifier("a"), TesslaAST.StrictEvaluation, null),
    (Identifier("b"), TesslaAST.StrictEvaluation, null),
    (Identifier("c"), TesslaAST.StrictEvaluation, null)
  ), null, "Map_add")
  val listEmptyExtern = Core.ExternExpression(Nil, Nil, null, "List_empty")
  val listAddExtern = Core.ExternExpression(Nil, List(
    (Identifier("a"), TesslaAST.StrictEvaluation, null),
    (Identifier("b"), TesslaAST.StrictEvaluation, null)
  ), null, "List_prepend")

  def reify(value: Any): Core.Expression = value match {
    case TesslaCore.IntValue(value, _) => Core.IntLiteralExpression(value, Location.unknown)
    case TesslaCore.BoolValue(value, _) => Core.BoolLiteralExpression(value, Location.unknown)
    case TesslaCore.FloatValue(value, _) => Core.FloatLiteralExpression(value, Location.unknown)
    case TesslaCore.StringValue(value, _) => Core.StringLiteralExpression(value, Location.unknown)
    case TesslaCore.BuiltInOperator(name, _) => Core.ExternExpression(Nil, Nil, null, name, Location.unknown)
    case TesslaCore.TesslaOption(Some(value), _, _) => Core.ApplicationExpression(someExtern, List(reify(value.forceValue)))
    case TesslaCore.TesslaOption(None, _, _) => Core.ApplicationExpression(noneExtern, Nil)
    case TesslaCore.TesslaSet(value, t, l) => if (value.isEmpty) {
      Core.ApplicationExpression(setEmptyExtern, Nil)
    } else {
      Core.ApplicationExpression(setAddExtern, List(reify(TesslaCore.TesslaSet(value.tail, t, l)), reify(value.head)))
    }
    case TesslaCore.TesslaMap(value, t, l) => if (value.isEmpty) {
      Core.ApplicationExpression(mapEmptyExtern, Nil)
    } else {
      Core.ApplicationExpression(mapAddExtern, List(reify(TesslaCore.TesslaMap(value.tail, t, l)), reify(value.head._1), reify(value.head._2)))
    }
    case TesslaCore.TesslaList(value, t, l) => if (value.isEmpty) {
      Core.ApplicationExpression(listEmptyExtern, Nil)
    } else {
      Core.ApplicationExpression(listAddExtern, List(reify(value.head), reify(TesslaCore.TesslaList(value.tail, t, l))))
    }
    case TesslaCore.TesslaObject(value, _) =>
      Core.RecordConstructorExpression(value.map(x => (Identifier(x._1), reify(x._2.forceValue))))

    //    case value: BigInt => Core.IntLiteralExpression(value, Location.unknown)
    //    case value: Double => Core.FloatLiteralExpression(value, Location.unknown)
    //    case value: Boolean => Core.BoolLiteralExpression(value, Location.unknown)
    //    case value: String => Core.StringLiteralExpression(value, Location.unknown)
    case _ => throw InternalError("Could not reify value of type: " + value.getClass)
  }
}

package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla.{Compiler, Evaluator, Errors, Location, TesslaCore, TesslaSource, TimeUnit, TranslationPhase}

import scala.collection.mutable

class Interpreter(val spec: TesslaCore.Specification) extends Specification {
  val inStreams: Map[String, (Input, TesslaCore.ValueType)] = spec.inStreams.map {
    case (name, typ, _) =>
      name -> (new Input, typ.elementType)
  }.toMap

  lazy val defs: Map[TesslaCore.Identifier, Lazy[Stream]] = spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp)))
  }.toMap

  lazy val outStreams: Seq[(String, Stream, TesslaCore.Type)] = spec.outStreams.map {
    case (name, streamRef: TesslaCore.Stream, typ) => (name, defs(streamRef.id).get, typ)
    case (name, streamRef: TesslaCore.InputStream, typ) => (name, inStreams(streamRef.name)._1, typ)
    case (name, _: TesslaCore.Nil, typ) => (name, nil, typ)
  }

  lazy val globalFunctions = spec.functions.toMap

  private def evalStream(arg: TesslaCore.StreamRef): Stream = arg match {
    case TesslaCore.Stream(id, loc) =>
      defs.getOrElse(id, throw InternalError(s"Couldn't find stream named $id", loc)).get
    case TesslaCore.InputStream(name, loc) =>
      inStreams.getOrElse(name, throw InternalError(s"Couldn't find input stream named $name", loc))._1
    case TesslaCore.Nil(_) => nil
  }

  private def eval(exp: TesslaCore.Expression): Stream = exp match {
    case TesslaCore.SignalLift(op, argStreams, loc) =>
      if (argStreams.isEmpty) {
        throw Errors.InternalError("Lift without arguments should be impossible", loc)
      }
      lift(argStreams.map(evalStream)) { arguments =>
        val args = arguments.zip(argStreams).map {
          case (arg, stream) => Lazy(arg.forceValue.withLoc(stream.loc))
        }
        globalFunctions(op) match {
          case TesslaCore.BuiltInOperator(builtIn, _) =>
            val result = Evaluator.evalPrimitiveOperator (builtIn, args, exp.loc)
            TesslaCore.ValueOrError.fromLazyOption (result)
          case other =>
            throw InternalError(s"TODO: $other")
        }
      }
    case TesslaCore.Lift(f, _, _) =>
      throw InternalError(s"TODO: ${globalFunctions(f)}")
    case TesslaCore.Default(values, defaultValue, _) =>
      evalStream(values).default(defaultValue)
    case TesslaCore.DefaultFrom(values, defaults, _) =>
      evalStream(values).default(evalStream(defaults))
    case TesslaCore.Const(value, stream, _) =>
      lift(Seq(evalStream(stream))) { _ =>
        Some(value)
      }
    case TesslaCore.Last(values, clock, _) =>
      last(evalStream(clock), evalStream(values))
    case TesslaCore.DelayedLast(values, delays, _) =>
      delayedLast(evalStream(delays), evalStream(values))
    case TesslaCore.Time(values, loc) =>
      evalStream(values).time(loc)
    case TesslaCore.Merge(arg1, arg2, loc) =>
      val stream1 = evalStream(arg1)
      val stream2 = evalStream(arg2)
      val zero = TesslaCore.IntValue(0, loc)
      lift(Seq(
        stream1.time(loc).default(zero),
        stream2.time(loc).default(zero),
        stream1.default(stream2),
        stream2.default(stream1)
      )) {
        case Seq(time1, time2, value1, value2) =>
          if (getInt(time1.forceValue) >= getInt(time2.forceValue)) Some(value1)
          else Some(value2)
      }
  }

  def getInt(value: TesslaCore.Value): BigInt = value match {
    case TesslaCore.IntValue(i, _) => i
    case _ => throw InternalError(s"Int expected, but $value found", value.loc)
  }
}

object Interpreter {
  class CoreToInterpreterSpec extends TranslationPhase[TesslaCore.Specification, Interpreter] {
    def translateSpec(spec: TesslaCore.Specification): Interpreter = new Interpreter(spec)
  }

  class RunInterpreter(inputTrace: Trace, stopOn: Option[String]) extends TranslationPhase[Interpreter, Trace] {

    override def translateSpec(spec: Interpreter): Trace = {
      val eventIterator = new Iterator[Trace.Event] {
        private var nextEvents = new mutable.Queue[Trace.Event]
        private var stopped = false
        private val seen = mutable.Set.empty[String]

        spec.outStreams.foreach {
          case (name, stream, _) =>
            stream.addListener {
              case Some(value) =>
                if (!stopped) {
                  if (stopOn.contains(name)) stopped = true
                  val timeStamp = Trace.TimeStamp(Location.unknown, spec.getTime)
                  val id = Trace.Identifier(Location.unknown, name)
                  nextEvents += Trace.Event(Location.unknown, timeStamp, id, value.forceValue)
                }
              case None =>
            }
        }

        private def typeCheck(value: TesslaCore.Value,
                              elementType: TesslaCore.ValueType,
                              name: String): Unit = value match {
          case _: TesslaCore.IntValue =>
            if (elementType != TesslaCore.IntType) {
              throw InputTypeMismatch(value, "Int", name, elementType, value.loc)
            }
          case _: TesslaCore.StringValue =>
            if (elementType != TesslaCore.StringType) {
              throw InputTypeMismatch(value, "String", name, elementType, value.loc)
            }
          case _: TesslaCore.BoolValue =>
            if (elementType != TesslaCore.BoolType) {
              throw InputTypeMismatch(value, "Bool", name, elementType, value.loc)
            }
          case _: TesslaCore.Unit =>
            if (elementType != TesslaCore.UnitType) {
              throw InputTypeMismatch(value, "Unit", name, elementType, value.loc)
            }
          case o: TesslaCore.TesslaOption =>
            elementType match {
              case ot: TesslaCore.OptionType =>
                o.value.foreach(typeCheck(_, ot.elementType, name))
              case _ =>
                throw InputTypeMismatch(value, "Option[?]", name, elementType, value.loc)
            }
          case s: TesslaCore.TesslaSet =>
            elementType match {
              case st: TesslaCore.SetType =>
                s.value.foreach(typeCheck(_, st.elementType, name))
              case _ =>
                throw InputTypeMismatch(value, "Set[?]", name, elementType, value.loc)
            }
          case m: TesslaCore.TesslaMap =>
            elementType match {
              case mt: TesslaCore.MapType =>
                m.value.foreach {
                  case (k, v) =>
                    typeCheck(k, mt.keyType, name)
                    typeCheck(v, mt.valueType, name)
                }
              case _ =>
                throw InputTypeMismatch(value, "Map[?, ?]", name, elementType, value.loc)
            }
          case _: TesslaCore.Closure | _: TesslaCore.BuiltInOperator =>
            throw InternalError("Functions should not currently be able to appear in input streams")
          case _: TesslaCore.Ctf =>
            if (elementType != TesslaCore.CtfType) {
              throw InputTypeMismatch(value, "CTF", name, elementType, value.loc)
            }
        }

        private def gatherValues(): Unit = {
          while (nextEvents.isEmpty && inputTrace.events.hasNext) {
            val specTime = spec.getTime
            val event = inputTrace.events.next
            val eventTime = event.timeStamp.time
            if (eventTime > specTime) {
              try {
                spec.step(eventTime - specTime)
                seen.clear()
              } catch {
                case err: TesslaError => throw TesslaErrorWithTimestamp(err, specTime)
              }
            }else if (eventTime < specTime){
              throw DecreasingTimeStampsError(specTime, eventTime, event.timeStamp.loc)
            }
            spec.inStreams.get(event.stream.name) match {
              case Some((inStream, elementType)) =>
                typeCheck(event.value, elementType, event.stream.name)
                if (seen.contains(event.stream.name)) {
                  throw SameTimeStampError(eventTime, event.stream.name, event.timeStamp.loc)
                }
                inStream.provide(event.value)
                seen += event.stream.name
                if (stopped) return
              case None =>
                // ignore undeclared input streams
            }
          }
          if (nextEvents.isEmpty) {
            try {
              spec.step()
            } catch {
              case err: TesslaError => throw TesslaErrorWithTimestamp(err, spec.getTime)
            }
            stopped = true
          }
        }

        override def hasNext = {
          if (!stopped) gatherValues()
          nextEvents.nonEmpty
        }

        override def next = {
          if (!stopped) gatherValues()
          nextEvents.dequeue
        }
      }
      new Trace(inputTrace.timeStampUnit, eventIterator)
    }
  }

  def runSpec(specSource: TesslaSource,
              traceSource: TesslaSource,
              stopOn: Option[String] = None,
              timeUnit: Option[TesslaSource] = None,
              printCore: Boolean = false,
              abortAt: Option[BigInt] = None,
             ): Result[Trace] = {
    val flatTrace = flattenInput(traceSource, timeUnit, abortAt)
    val core = new Compiler().applyPasses(specSource, flatTrace.timeStampUnit)
    if (printCore) core.foreach(println)
    core.andThen(new CoreToInterpreterSpec).andThen(new RunInterpreter(flatTrace, stopOn))
  }

  def flattenInput(traceSource: TesslaSource,
              timeUnit: Option[TesslaSource] = None,
              abortAt: Option[BigInt] = None,
             ): Trace = {
    val rawTrace: RawTrace = TraceParser.parseTrace(traceSource)
    val tu = timeUnit.map(TimeUnit.parse).orElse(rawTrace.timeStampUnit)

    new Trace(tu, new FlatEventIterator(rawTrace.eventRanges, abortAt))
  }

  def runCtf(specSource: TesslaSource,
             ctfFileName: String,
             stopOn: Option[String] = None,
             timeUnit: Option[TesslaSource] = None,
             printCore: Boolean = false,
             abortAt: Option[BigInt] = None,
            ): Result[Trace] = {
    val tu = timeUnit.map(TimeUnit.parse)
    val core = new Compiler().applyPasses(specSource, tu)
    if (printCore) core.foreach(println)
    val trace = Trace.fromCtfFile(ctfFileName, abortAt)
    core.andThen(new CoreToInterpreterSpec).andThen(new RunInterpreter(trace, stopOn))
  }
}
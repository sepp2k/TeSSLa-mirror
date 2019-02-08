package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla._
import org.antlr.v4.runtime.CharStream

import scala.collection.mutable

class Interpreter(val spec: TesslaCore.Specification) extends Specification {
  val inStreams: Map[String, (Input, TesslaCore.ValueType)] = spec.inStreams.map {
    case (name, typ, _) =>
      name -> (new Input, typ.elementType)
  }.toMap

  type Env = Map[TesslaCore.Identifier, TesslaCore.ValueOrError]

  lazy val defs: Map[TesslaCore.Identifier, Lazy[Stream]] = spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp)))
  }.toMap

  lazy val outStreams: Seq[(Option[String], Stream, TesslaCore.Type)] = spec.outStreams.map {
    case (name, streamRef: TesslaCore.Stream, typ) => (name, defs(streamRef.id).get, typ)
    case (name, streamRef: TesslaCore.InputStream, typ) => (name, inStreams(streamRef.name)._1, typ)
    case (name, _: TesslaCore.Nil, typ) => (name, nil, typ)
  }

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
          case (arg, stream) => arg.mapValue(_.withLoc(stream.loc))
        }
        Evaluator.evalPrimitiveOperator(op, args, exp.loc)
      }
    case TesslaCore.Lift(f, argStreams, loc) =>
      if (argStreams.isEmpty) {
        throw Errors.InternalError("Lift without arguments should be impossible", loc)
      }
      simpleLift(argStreams.map(evalStream)) { arguments =>
        val args = arguments.zip(argStreams).map {
          case (Some(arg), stream) => arg.mapValue(a => TesslaCore.TesslaOption(Some(a), stream.loc))
          case (None, stream) => TesslaCore.TesslaOption(None, stream.loc)
        }
        Evaluator.evalApplication(f, args, loc) match {
          case to: TesslaCore.TesslaOption => to.value
          case other =>
            throw InternalError(s"Used lift on non-option function (return value: $other) - should have been caught by type checker")
        }
      }
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
    case TesslaCore.Delay(delays, resets, _) =>
      delay(evalStream(delays), evalStream(resets))
    case TesslaCore.Time(values, loc) =>
      evalStream(values).time(loc)
    case TesslaCore.StdLibCount(values, loc) =>
      val x = evalStream(values)
      lazy val y: Stream = lift(Seq(last(x, y), nil.default(TesslaCore.IntValue(1, loc)))) { arguments =>
        val args = arguments.map(_.mapValue(_.withLoc(loc)))
        Evaluator.evalPrimitiveOperator(BuiltIn.Add, args, exp.loc)
      }.default(TesslaCore.IntValue(0, loc))
      y
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

object ValueTypeChecker {
  def check(value: TesslaCore.Value,
            elementType: TesslaCore.ValueType,
            name: String): Unit = value match {
    case _: TesslaCore.IntValue =>
      if (elementType != TesslaCore.IntType) {
        throw InputTypeMismatch(value, "Int", name, elementType, value.loc)
      }
    case _: TesslaCore.FloatValue =>
      if (elementType != TesslaCore.FloatType) {
        throw InputTypeMismatch(value, "Float", name, elementType, value.loc)
      }
    case _: TesslaCore.StringValue =>
      if (elementType != TesslaCore.StringType) {
        throw InputTypeMismatch(value, "String", name, elementType, value.loc)
      }
    case _: TesslaCore.BoolValue =>
      if (elementType != TesslaCore.BoolType) {
        throw InputTypeMismatch(value, "Bool", name, elementType, value.loc)
      }
    case o: TesslaCore.TesslaOption =>
      elementType match {
        case ot: TesslaCore.OptionType =>
          o.value.foreach(check(_, ot.elementType, name))
        case _ =>
          throw InputTypeMismatch(value, "Option[?]", name, elementType, value.loc)
      }
    case s: TesslaCore.TesslaSet =>
      elementType match {
        case st: TesslaCore.SetType =>
          s.value.foreach(check(_, st.elementType, name))
        case _ =>
          throw InputTypeMismatch(value, "Set[?]", name, elementType, value.loc)
      }
    case m: TesslaCore.TesslaMap =>
      elementType match {
        case mt: TesslaCore.MapType =>
          m.value.foreach {
            case (k, v) =>
              check(k, mt.keyType, name)
              check(v, mt.valueType, name)
          }
        case _ =>
          throw InputTypeMismatch(value, "Map[?, ?]", name, elementType, value.loc)
      }
    case s: TesslaCore.TesslaList =>
      elementType match {
        case st: TesslaCore.ListType =>
          s.value.foreach(check(_, st.elementType, name))
        case _ =>
          throw InputTypeMismatch(value, "List[?]", name, elementType, value.loc)
      }
    case o: TesslaCore.TesslaObject =>
      val actual = o.value.keys.map {n => s"$n: ?"}.mkString("{", ", ", "}")
      elementType match {
        case ot: TesslaCore.ObjectType =>
          if (ot.memberTypes.keySet != o.value.keySet) {
            throw InputTypeMismatch(value, actual, name, elementType, value.loc)
          }
          o.value.foreach {
            case (n, v) => check(v, ot.memberTypes(n), s"$name.$n")
          }
        case _ =>
          throw InputTypeMismatch(value, actual, name, elementType, value.loc)
      }
    case _: TesslaCore.Closure | _: TesslaCore.BuiltInOperator =>
      throw InternalError("Functions should not currently be able to appear in input streams")
    case _: TesslaCore.Ctf =>
      if (elementType != TesslaCore.CtfType) {
        throw InputTypeMismatch(value, "CTF", name, elementType, value.loc)
      }
  }
}

object Interpreter {
  type Trace = Iterator[Trace.Event]

  class CoreToInterpreterSpec extends TranslationPhase[TesslaCore.Specification, Interpreter] {
    def translateSpec(spec: TesslaCore.Specification): Interpreter = new Interpreter(spec)
  }

  class RunInterpreter(inputTrace: Trace, stopOn: Option[String]) extends TranslationPhase[Interpreter, Trace] {

    override def translateSpec(spec: Interpreter): Trace = {
      new Iterator[Trace.Event] {
        private var nextEvents = new mutable.Queue[Trace.Event]
        private var stopped = false
        private val seen = mutable.Set.empty[String]

        spec.outStreams.foreach {
          case (nameOpt, stream, _) =>
            stream.addListener {
              case Some(value) =>
                if (!stopped) {
                  if (stopOn.isDefined && stopOn == nameOpt) stopped = true
                  val timeStamp = Trace.TimeStamp(Location.unknown, spec.getTime)
                  val idOpt = nameOpt.map(Trace.Identifier(_, Location.unknown))
                  nextEvents += Trace.Event(Location.unknown, timeStamp, idOpt, value.forceValue)
                }
              case None =>
            }
        }

        private def gatherValues(): Unit = {
          while (nextEvents.isEmpty && inputTrace.hasNext) {
            val specTime = spec.getTime
            val event = inputTrace.next
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
                ValueTypeChecker.check(event.value, elementType, event.stream.name)
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
    }
  }

  def runSpec(specSource: CharStream,
              traceSource: CharStream,
              stopOn: Option[String] = None,
              timeUnit: Option[String] = None,
              printCore: Boolean = false,
              abortAt: Option[BigInt] = None,
             ): Result[Trace] = {
    val flatTrace = flattenInput(traceSource, abortAt)
    val tu = timeUnit.map(TimeUnit.fromString(_, Location.option("timeunit")))
    val core = new Compiler().applyPasses(specSource, tu)
    if (printCore) core.foreach(println)
    core.andThen(new CoreToInterpreterSpec).andThen(new RunInterpreter(flatTrace, stopOn))
  }

  def flattenInput(traceSource: CharStream,
                   abortAt: Option[BigInt] = None,
                  ): Trace = {
    val rawTrace = new TraceParser().parseTrace(traceSource)
    new FlatEventIterator(rawTrace, abortAt)
  }

  def runCtf(specSource: CharStream,
             ctfFileName: String,
             stopOn: Option[String] = None,
             timeUnit: Option[String] = None,
             printCore: Boolean = false,
             abortAt: Option[BigInt] = None,
            ): Result[Trace] = {
    val tu = timeUnit.map(TimeUnit.fromString(_, Location.option("timeunit")))
    val core = new Compiler().applyPasses(specSource, tu)
    if (printCore) core.foreach(println)
    val trace = Trace.fromCtfFile(ctfFileName, abortAt)
    core.andThen(new CoreToInterpreterSpec).andThen(new RunInterpreter(trace, stopOn))
  }
}
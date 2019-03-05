package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla._
import org.antlr.v4.runtime.CharStream

import scala.collection.mutable

class Interpreter(val spec: TesslaCore.Specification) extends Specification {
  val inStreams: Map[String, (Input, TesslaCore.ValueType)] = spec.inStreams.map {inStream =>
    inStream.name -> (new Input, inStream.typ.elementType)
  }.toMap

  type Env = Map[TesslaCore.Identifier, TesslaCore.ValueOrError]

  lazy val defs: Map[TesslaCore.Identifier, Lazy[Stream]] = spec.streams.map { stream =>
    stream.id -> Lazy(eval(stream.expression))
  }.toMap

  lazy val outStreams: Seq[(Option[String], Stream, TesslaCore.Type)] = spec.outStreams.map { os =>
    os.stream match {
      case streamRef: TesslaCore.Stream => (os.nameOpt, defs(streamRef.id).get, os.typ)
      case streamRef: TesslaCore.InputStream => (os.nameOpt, inStreams(streamRef.name)._1, os.typ)
      case _: TesslaCore.Nil => (os.nameOpt, nil, os.typ)
    }
  }

  private def evalStream(arg: TesslaCore.StreamRef): Stream = arg match {
    case TesslaCore.Stream(id, loc) =>
      defs.getOrElse(id, throw InternalError(s"Couldn't find stream named $id", loc)).get
    case TesslaCore.InputStream(name, loc) =>
      inStreams.getOrElse(name, throw InternalError(s"Couldn't find input stream named $name", loc))._1
    case TesslaCore.Nil(_) => nil
  }

  def merge(stream1: Stream, stream2: Stream): Stream = {
    lift(Seq(stream1, stream2)) {
      case Seq(Some(value1), _) => Some(value1)
      case Seq(None, value2Opt) => value2Opt
    }
  }

  def filter(events: Stream, condition: Stream): Stream = {
    val latestCondition = merge(condition, last(condition, events))
    lift(Seq(events, latestCondition)) {
      case Seq(value, Some(TesslaCore.BoolValue(true, _))) =>
        value
      case _ => None
    }
  }

  def slift(streams: Seq[Stream])(op: Seq[TesslaCore.ValueOrError] => Option[TesslaCore.ValueOrError]): Stream = {
    val ticks = lift(streams) { _ =>
      Some(TesslaCore.TesslaObject(Map(), Location.builtIn))
    }
    val recentValueStreams = streams.map { stream =>
      merge(stream, last(stream, ticks))
    }
    lift(recentValueStreams) { valueOptions =>
      if (valueOptions.exists(_.isEmpty)) {
        None
      } else {
        op(valueOptions.flatten)
      }
    }
  }


  private def eval(exp: TesslaCore.Expression): Stream = exp match {
    case TesslaCore.SignalLift(op, argStreams, loc) =>
      if (argStreams.isEmpty) {
        throw Errors.InternalError("Lift without arguments should be impossible", loc)
      }
      slift(argStreams.map(evalStream)) { arguments =>
        val args = arguments.zip(argStreams).map {
          case (arg, stream) => arg.mapValue(_.withLoc(stream.loc))
        }
        Some(Evaluator.evalPrimitiveOperator(op, args, exp.loc))
      }
    case TesslaCore.Lift(f, argStreams, loc) =>
      if (argStreams.isEmpty) {
        throw Errors.InternalError("Lift without arguments should be impossible", loc)
      }
      lift(argStreams.map(evalStream)) { arguments =>
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
      slift(Seq(evalStream(stream))) { _ =>
        Some(value)
      }
    case TesslaCore.Last(values, clock, _) =>
      last(evalStream(values), evalStream(clock))
    case TesslaCore.DelayedLast(values, delays, _) =>
      delayedLast(evalStream(delays), evalStream(values))
    case TesslaCore.Delay(delays, resets, _) =>
      delay(evalStream(delays), evalStream(resets))
    case TesslaCore.Time(values, loc) =>
      evalStream(values).time(loc)
    case TesslaCore.StdLibCount(values, loc) =>
      val x = evalStream(values)
      lazy val y: Stream = lift(Seq(x, last(y, x))) {
        case Seq(Some(_), Some(b)) =>
          Some(b.mapValue(value => TesslaCore.IntValue(getInt(value) + 1, loc)))
        case _ => Some(TesslaCore.IntValue(0, loc))
      }.default(TesslaCore.IntValue(0, loc))
      y
    case TesslaCore.Merge(arg1, arg2, _) =>
      val stream1 = evalStream(arg1)
      val stream2 = evalStream(arg2)
      merge(stream1, stream2)
    case TesslaCore.Filter(eventsArg, conditionArg, _) =>
      val eventsStream = evalStream(eventsArg)
      val conditionStream = evalStream(conditionArg)
      filter(eventsStream, conditionStream)
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

  class RunInterpreter(inputTrace: Trace, stopOn: Option[String] = None) extends TranslationPhase[Interpreter, Trace] {

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
}
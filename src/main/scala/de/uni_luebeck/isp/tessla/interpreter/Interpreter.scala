package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla._

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
        val closure = TesslaCore.Closure(f, Map(), loc)
        Evaluator.evalApplication(closure, args, loc) match {
          case to: TesslaCore.TesslaOption => to.value
          case other =>
            throw InternalError(s"Used lift on non-option function (return value: $other) - should have been caught by type checker")
        }
      }
    case TesslaCore.Default(values, defaultValue, _) =>
      evalStream(values).default(defaultValue)
    case TesslaCore.DefaultFrom(values, defaults, _) =>
      evalStream(values).default(evalStream(defaults))
    case TesslaCore.Last(values, clock, _) =>
      last(evalStream(values), evalStream(clock))
    case TesslaCore.Delay(delays, resets, _) =>
      delay(evalStream(delays), evalStream(resets))
    case TesslaCore.Time(values, loc) =>
      evalStream(values).time(loc)
    case customCall: TesslaCore.CustomBuiltInCall =>
      throw InternalError(s"The interpreter does not support any custom built-ins (${customCall.name})")
  }

  def getInt(value: TesslaCore.Value): BigInt = value match {
    case TesslaCore.IntValue(i, _) => i
    case _ => throw InternalError(s"Int expected, but $value found", value.loc)
  }
}

object Interpreter {
  type Trace = Iterator[Trace.Event]

  def run(spec: TesslaCore.Specification, input: Trace, stopOn: Option[String]): Trace = {
    val interpreter = new Interpreter(spec)
    new Iterator[Trace.Event] {
      private var nextEvents = new mutable.Queue[Trace.Event]
      private var stopped = false
      private val seen = mutable.Set.empty[String]

      interpreter.outStreams.foreach {
        case (nameOpt, stream, _) =>
          stream.addListener {
            case Some(value) =>
              if (!stopped) {
                if (stopOn.isDefined && stopOn == nameOpt) stopped = true
                val timeStamp = Trace.TimeStamp(Location.unknown, interpreter.getTime)
                val idOpt = nameOpt.map(Trace.Identifier(_, Location.unknown))
                nextEvents += Trace.Event(Location.unknown, timeStamp, idOpt, value.forceValue)
              }
            case None =>
          }
      }

      private def gatherValues(): Unit = {
        while (nextEvents.isEmpty && input.hasNext) {
          val specTime = interpreter.getTime
          val event = input.next
          val eventTime = event.timeStamp.time
          if (eventTime > specTime) {
            try {
              interpreter.step(eventTime - specTime)
              seen.clear()
            } catch {
              case err: TesslaError => throw TesslaErrorWithTimestamp(err, specTime)
            }
          } else if (eventTime < specTime) {
            throw DecreasingTimeStampsError(specTime, eventTime, event.timeStamp.loc)
          }
          interpreter.inStreams.get(event.stream.name) match {
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
            interpreter.step()
          } catch {
            case err: TesslaError => throw TesslaErrorWithTimestamp(err, interpreter.getTime)
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
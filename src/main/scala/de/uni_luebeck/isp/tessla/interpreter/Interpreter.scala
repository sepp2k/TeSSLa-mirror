package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla._

import scala.collection.mutable

class Interpreter(val spec: TesslaCore.Specification, val evaluator: Evaluator) extends Specification {
  val inStreams: Map[String, (Input, TesslaCore.ValueType)] = spec.inStreams.map {inStream =>
    inStream.name -> (new Input, inStream.typ.elementType)
  }.toMap

  type Env = Map[TesslaCore.Identifier, TesslaCore.ValueOrError]

  lazy val defs: Map[TesslaCore.Identifier, Lazy[Stream]] = spec.streams.map { stream =>
    stream.id -> Lazy(eval(stream))
  }.toMap

  lazy val outStreams: Seq[(Option[String], Stream, TesslaCore.Type)] = spec.outStreams.map { os =>
    os.stream match {
      case streamRef: TesslaCore.Stream => (os.nameOpt, defs(streamRef.id).get, os.typ)
      case streamRef: TesslaCore.InputStream => (os.nameOpt, inStreams(streamRef.name)._1, os.typ)
      case _: TesslaCore.Nil => (os.nameOpt, nil, os.typ)
    }
  }

  private def toStreamRef(arg: TesslaCore.Arg): TesslaCore.StreamRef = arg match {
    case stream: TesslaCore.StreamRef => stream
    case _ => throw InternalError(s"Expected stream argument but got value argument. This should have been caught by the type checker.")
  }

  private def toValue(arg: TesslaCore.Arg): TesslaCore.ValueOrError = arg match {
    case value: TesslaCore.ValueOrError => value
    case _ => throw InternalError(s"Expected value argument but got stream argument. This should have been caught by the type checker.")
  }

  private def evalStream(arg: TesslaCore.StreamRef): Stream = arg match {
    case s: TesslaCore.Stream =>
      defs.getOrElse(s.id, throw InternalError(s"Couldn't find stream named ${s.id}", s.loc)).get
    case i: TesslaCore.InputStream =>
      inStreams.getOrElse(i.name, throw InternalError(s"Couldn't find input stream named ${i.name}", i.loc))._1
    case _: TesslaCore.Nil => nil
  }

  private def eval(sd: TesslaCore.StreamDescription): Stream = sd.expression match {
    case TesslaCore.Last(values, clock, _) =>
      last(evalStream(values), evalStream(clock))
    case TesslaCore.Delay(delays, resets, _) =>
      delay(evalStream(delays), evalStream(resets))
    case customCall: TesslaCore.CustomBuiltInCall =>
      customCall.name match {
        case "slift" =>
          val argStreams = customCall.args.init.map(toStreamRef)
          val f = toValue(customCall.args.last)
          if (argStreams.isEmpty) {
            throw Errors.InternalError("slift without arguments should be impossible", customCall.loc)
          }
          slift(argStreams.map(evalStream)) { arguments =>
            evaluator.evalApplication(f, arguments, sd.typ.elementType, customCall.loc)
          }
        case "lift" =>
          val argStreams = customCall.args.init.map(toStreamRef)
          val f = toValue(customCall.args.last)
          if (argStreams.isEmpty) {
            throw Errors.InternalError("lift without arguments should be impossible", customCall.loc)
          }
          lift(argStreams.map(evalStream)) { arguments =>
            val args = arguments.zip(argStreams).map {
              case (Some(arg), stream) =>
                TesslaCore.TesslaOption(Some(arg), stream.typ.elementType, stream.loc)
              case (None, stream) => TesslaCore.TesslaOption(None, sd.typ.elementType, stream.loc)
            }
            evaluator.evalApplication(f, args, sd.typ.elementType, customCall.loc) match {
              case to: TesslaCore.TesslaOption => to.value
              case err: TesslaCore.Error => Some(err)
              case other =>
                throw InternalError(s"Used lift on non-option function (return value: $other) - should have been caught by type checker")
            }
          }
        case "merge" =>
          val streams = customCall.args.map(toStreamRef).map(evalStream)
          merge(streams)
        case "default" =>
          val stream = toStreamRef(customCall.args.head)
          val defaultValue = toValue(customCall.args(1))
          evalStream(stream).default(defaultValue)
        case "defaultFrom" =>
          val stream = toStreamRef(customCall.args.head)
          val defaultStream = toStreamRef(customCall.args(1))
          evalStream(stream).default(evalStream(defaultStream))
        case "time" =>
          val stream = toStreamRef(customCall.args.head)
          evalStream(stream).time(customCall.loc)
        case name => throw InternalError(s"The interpreter does not support $name")
      }
  }

  def getInt(value: TesslaCore.Value): BigInt = value match {
    case TesslaCore.IntValue(i, _) => i
    case _ => throw InternalError(s"Int expected, but $value found", value.loc)
  }
}

object Interpreter {
  type Trace = Iterator[Trace.Event]

  def run(spec: TesslaCore.Specification, input: Trace, stopOn: Option[String], evaluator: Evaluator): Trace = {
    val interpreter = new Interpreter(spec, evaluator)
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
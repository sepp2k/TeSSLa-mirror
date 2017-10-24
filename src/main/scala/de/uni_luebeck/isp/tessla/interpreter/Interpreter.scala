package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import de.uni_luebeck.isp.tessla.{Compiler, Location, TesslaCore, TesslaSource, TimeUnit, TranslationPhase, Types}

import scala.collection.mutable

class Interpreter(val spec: TesslaCore.Specification) extends Specification {
  val inStreams: Map[String, (Input, Types.ValueType)] = spec.inStreams.map {
    case (name, typ, _) =>
      name -> (new Input, typ.elementType)
  }.toMap


  lazy val defs: Map[String, Lazy[Stream]] = inStreams.mapValues {
    case (inputStream, _) => Lazy(inputStream)
  } ++ spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp)))
  }

  lazy val outStreams: Map[String, Stream] = spec.outStreams.map {
    case (name, streamRef) => name -> defs(streamRef.name).get
  }.toMap

  private def evalStream(arg: TesslaCore.StreamRef): Stream = arg match {
    case TesslaCore.Stream(name, loc) =>
      defs.getOrElse(name, throw InternalError(s"Couldn't find stream named $name", loc)).get
    case TesslaCore.InputStream(name, loc) =>
      inStreams.getOrElse(name, throw InternalError(s"Couldn't find stream named $name", loc))._1
    case TesslaCore.Nil(_) => nil
  }

  private def eval(exp: TesslaCore.Expression): Stream = exp match {
    case TesslaCore.Lift(op, Seq(), loc) =>
      try {
        op.eval(Seq(), loc) match {
          case None => nil
          case Some(x) =>
            nil.default(x)
        }
      } catch {
        case c: TesslaError =>
          nil.default(TesslaCore.ErrorValue(c))
      }
    case TesslaCore.Lift(op, argStreams, loc) =>
      lift(argStreams.map(evalStream)) { args =>
        try {
          op.eval((args, argStreams).zipped.map((arg, s) => arg.withLoc(s.loc)), loc)
        } catch {
          case c: TesslaError =>
            Some(TesslaCore.ErrorValue(c))
        }
      }
    case TesslaCore.Default(values, defaultValue, _) =>
      evalStream(values).default(defaultValue)
    case TesslaCore.DefaultFrom(values, defaults, _) =>
      evalStream(values).default(evalStream(defaults))
    case TesslaCore.Last(values, clock, _) =>
      last(evalStream(clock), evalStream(values))
    case TesslaCore.DelayedLast(values, delays, loc) =>
      delayedLast(intStream(evalStream(delays), loc), evalStream(values))
    case TesslaCore.Time(values, loc) =>
      evalStream(values).time(loc)
  }

  def intStream(stream: Stream, loc: Location): Stream = {
    lift(Seq(stream)) {
      (args: Seq[TesslaCore.Value]) =>
        args.head match {
          case TesslaCore.IntLiteral(i, _) => Some(TesslaCore.IntLiteral(i, loc))
          case value => throw TypeMismatch(Types.Int, value.typ, loc)
        }
    }
  }
}

object Interpreter {
  class CoreToInterpreterSpec extends TranslationPhase[TesslaCore.Specification, Interpreter] {
    def translateSpec(spec: TesslaCore.Specification): Interpreter = new Interpreter(spec)
  }

  class RunInterpreter(inputTrace: Trace, stopOn: Option[String]) extends TranslationPhase[Interpreter, Trace] {
    override def translateSpec(spec: Interpreter): Trace = {
      val eventIterator = new Iterator[Trace.Event] {
        var nextEvents = new mutable.Queue[Trace.Event]
        var atEnd = false

        spec.outStreams.foreach {
          case (name, stream) =>
            stream.addListener {
              case Some(value: TesslaCore.LiteralValue) =>
                val timeStamp = Trace.TimeStamp(Location.unknown, spec.getTime)
                nextEvents += Trace.Event(Location.unknown, timeStamp, Trace.Identifier(Location.unknown, name), value)
              case Some(TesslaCore.ErrorValue(error)) =>
                throw error
              case None =>
            }
        }

        def gatherValues(): Unit = {
          while (nextEvents.isEmpty && inputTrace.events.hasNext) {
            val event = inputTrace.events.next()
            val specTime = spec.getTime
            val eventTime = event.timeStamp.time
            if (eventTime > specTime) {
              spec.step(eventTime - specTime)
            } else if (eventTime < specTime) {
              throw DecreasingTimeStampsError(specTime, eventTime, event.timeStamp.loc)
            }
            spec.inStreams.get(event.stream.name) match {
              case Some((inStream, elementType)) =>
                if (event.value.typ != elementType) {
                  throw InputTypeMismatch(event.value, event.stream.name, elementType, event.loc)
                }
                inStream.provide(event.value)

              case None =>
                throw UndeclaredInputStreamError(event.stream.name, event.stream.loc)
            }
            if (stopOn.contains(event.stream.name)) {
              spec.step()
              atEnd = true
              return
            }
          }
          if (!inputTrace.events.hasNext) {
            spec.step()
            atEnd = true
          }
        }

        override def hasNext = {
          if(!atEnd) gatherValues()
          nextEvents.nonEmpty
        }

        override def next() = {
          if(!atEnd) gatherValues()
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
              printCore: Boolean = false
             ): Result[Trace] = {
    val inputTrace: Trace = TraceParser.parseTrace(traceSource)
    val tu = timeUnit.map(TimeUnit.parse).orElse(inputTrace.timeStampUnit)
    val core = new Compiler().applyPasses(specSource, tu)
    if (printCore) core.foreach(println)
    core.andThen(new CoreToInterpreterSpec).andThen(new RunInterpreter(inputTrace, stopOn))
  }
}
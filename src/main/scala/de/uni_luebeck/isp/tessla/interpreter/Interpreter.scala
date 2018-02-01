package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.TranslationPhase.Result
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla.{Compiler, ConstantEvaluator, Errors, Location, TesslaCore, TesslaSource, TimeUnit, TranslationPhase}

import scala.collection.mutable

class Interpreter(val spec: TesslaCore.Specification) extends Specification {
  val inStreams: Map[String, (Input, TesslaCore.ValueType)] = spec.inStreams.map {
    case (name, typ, _) =>
      name -> (new Input, typ.elementType)
  }.toMap


  lazy val defs: Map[TesslaCore.Identifier, Lazy[Stream]] = spec.streams.map {
    case (name, exp) => (name, Lazy(eval(exp.expression)))
  }

  lazy val outStreams: Map[String, Stream] = spec.outStreams.map {
    case (name, streamRef: TesslaCore.Stream) => name -> defs(streamRef.id).get
    case (name, streamRef: TesslaCore.InputStream) => name -> inStreams(streamRef.name)._1
    case (name, _: TesslaCore.Nil) => name -> nil
  }.toMap

  private def evalStream(arg: TesslaCore.StreamRef): Stream = arg match {
    case TesslaCore.Stream(id, loc) =>
      defs.getOrElse(id, throw InternalError(s"Couldn't find stream named $id", loc)).get
    case TesslaCore.InputStream(name, loc) =>
      inStreams.getOrElse(name, throw InternalError(s"Couldn't find input stream named $name", loc))._1
    case TesslaCore.Nil(_) => nil
  }

  private def eval(exp: TesslaCore.Expression): Stream = exp match {
    case TesslaCore.Lift(op, typeArgs, Seq(), loc) =>
      throw Errors.InternalError("Lift without arguments should be impossible")
    case TesslaCore.Lift(op, typeArgs, argStreams, loc) =>
      lift(argStreams.map(evalStream)) { arguments =>
        val result = ConstantEvaluator.evalPrimitiveOperator(op, arguments.map(arg => Lazy(arg.forceValue)), exp.loc)
        TesslaCore.ValueOrError.fromLazyOption(result)
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
          case value => throw InternalError("Type error (expected: Int) should've been caught by type checker", value.loc)
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
        var stopped = false

        spec.outStreams.foreach {
          case (name, stream) =>
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

        def gatherValues(): Unit = {
          while (nextEvents.isEmpty && inputTrace.events.hasNext) {
            val specTime = spec.getTime
            val event = inputTrace.events.next
            val eventTime = event.timeStamp.time
            if (eventTime > specTime) {
              spec.step(eventTime - specTime)
            }else if (eventTime < specTime){
              throw DecreasingTimeStampsError(specTime, eventTime, event.timeStamp.loc)
            }
            spec.inStreams.get(event.stream.name) match {
              case Some((inStream, elementType)) =>
                if (event.value.typ != elementType) {
                  throw InputTypeMismatch(event.value, event.stream.name, elementType, event.loc)
                }
                inStream.provide(event.value)
                if (stopped) return
              case None =>
                throw UndeclaredInputStreamError(event.stream.name, event.stream.loc)
            }
          }
          if (nextEvents.isEmpty) {
            spec.step()
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
}
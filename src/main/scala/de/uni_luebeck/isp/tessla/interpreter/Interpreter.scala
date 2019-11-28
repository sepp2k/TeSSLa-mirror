package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla._

import scala.collection.mutable
import TesslaAST.Core

class Interpreter(val spec: Core.Specification) extends Specification(RuntimeEvaluator.Record(Map())) {
  val inStreams: Map[String, (Input, TesslaCore.ValueType)] = spec.in.map { inStream =>
    inStream._1.id -> (new Input, null)
  }

  val streamExterns: Map[String, List[Lazy[Any]] => Any] = Map(
    "last" -> ((arguments: List[Lazy[Any]]) => last(arguments(0).get.asInstanceOf[Stream], arguments(1).get.asInstanceOf[Stream])),
    "delay" -> ((arguments: List[Lazy[Any]]) => delay(arguments(0).get.asInstanceOf[Stream], arguments(1).get.asInstanceOf[Stream])),
    "slift" -> ((arguments: List[Lazy[Any]]) => {
      val op = arguments.last.get.asInstanceOf[List[Lazy[Any]] => Any]
      slift(arguments.init.map(_.get.asInstanceOf[Stream])) { args =>
        op(args.map(Lazy(_)))
      }
    }),
    "lift" -> ((arguments: List[Lazy[Any]]) => {
      val op = arguments.last.get.asInstanceOf[List[Lazy[Any]] => Any]
      lift(arguments.init.map(_.get.asInstanceOf[Stream])) { args =>
        op(args.map(a => Lazy(a))).asInstanceOf[Option[Any]]
      }
    }),
    "merge" -> ((arguments: List[Lazy[Any]]) => merge(arguments.map(_.get.asInstanceOf[Stream]))),
    "default" -> ((arguments: List[Lazy[Any]]) =>
      arguments(0).get.asInstanceOf[Stream].default(arguments(1).get)),
    "defaultFrom" -> ((arguments: List[Lazy[Any]]) => arguments(0).get.asInstanceOf[Stream].default(arguments(1).get.asInstanceOf[Stream])),
    "time" -> ((arguments: List[Lazy[Any]]) => arguments(0).get.asInstanceOf[Stream].time()),
    "nil" -> ((_: List[Lazy[Any]]) => nil),
  )


  val runtimeEvaluator: RuntimeEvaluator = new RuntimeEvaluator(RuntimeEvaluator.commonExterns ++ streamExterns)

  lazy val definitions: RuntimeEvaluator.Env = inStreams.mapValues(x => Lazy(x._1)) ++ spec.definitions.map(d => (d._1.id, Lazy(runtimeEvaluator.evalExpressionArg(d._2, definitions))))

  lazy val outStreams: Seq[(Option[String], Stream, TesslaCore.Type)] = spec.out.map { os =>
    (os._2, definitions(os._1.id).get.asInstanceOf[Stream], null)
  }

}

object Interpreter {
  type Trace = Iterator[Trace.Event]

  def run(spec: Core.Specification, input: Trace, stopOn: Option[String]): Trace = {
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
                nextEvents += Trace.Event(Location.unknown, timeStamp, idOpt, value) // TODO: handle somewhere if value contains RuntimExecption
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
              // TODO: reenable line below to check type of input values
              //ValueTypeChecker.check(event.value, elementType, event.stream.name)
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
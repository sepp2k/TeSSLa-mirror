package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._
import de.uni_luebeck.isp.tessla.util.Lazy
import de.uni_luebeck.isp.tessla._

import scala.collection.mutable
import TesslaAST.Core

import scala.collection.immutable.ArraySeq
import scala.util.Try

class Interpreter(val spec: Core.Specification) extends Specification(RuntimeEvaluator.Record(Map())) {
  val inStreams: Map[String, (Input, Core.Type)] = spec.in.map { inStream =>
    inStream._1.idOrName.left.get -> (new Input, inStream._2._1)
  }

  val streamExterns: Map[String, ArraySeq[Lazy[Any]] => Lazy[Any]] = Map(
    "last" -> ((arguments: ArraySeq[Lazy[Any]]) => Lazy(last(arguments(0).get.asInstanceOf[Stream], arguments(1).get.asInstanceOf[Stream]))),
    "delay" -> ((arguments: ArraySeq[Lazy[Any]]) => Lazy(delay(arguments(0).get.asInstanceOf[Stream], arguments(1).get.asInstanceOf[Stream]))),
    "slift" -> ((arguments: ArraySeq[Lazy[Any]]) => {
      val op = arguments.last.get.asInstanceOf[ArraySeq[Lazy[Any]] => Lazy[Any]]
      Lazy(slift(arguments.init.map(_.get.asInstanceOf[Stream])) { args =>
        op(args.map(Lazy(_))).get
      })
    }),
    "lift" -> ((arguments: ArraySeq[Lazy[Any]]) => {
      val op = arguments.last.get.asInstanceOf[ArraySeq[Lazy[Any]] => Lazy[Any]]
      Lazy(lift(arguments.init.map(_.get.asInstanceOf[Stream])) { args =>
        op(args.map(a => Lazy(a))).get.asInstanceOf[Option[Any]]
      })
    }),
    "merge" -> ((arguments: ArraySeq[Lazy[Any]]) => Lazy(merge(arguments.map(_.get.asInstanceOf[Stream])))),
    "default" -> ((arguments: ArraySeq[Lazy[Any]]) =>
      Lazy(arguments(0).get.asInstanceOf[Stream].default(arguments(1).get))),
    "defaultFrom" ->((arguments: ArraySeq[Lazy[Any]]) =>  Lazy(arguments(0).get.asInstanceOf[Stream].default(arguments(1).get.asInstanceOf[Stream]))),
    "time" -> ((arguments: ArraySeq[Lazy[Any]]) => Lazy(arguments(0).get.asInstanceOf[Stream].time())),
    "nil" -> ((_: ArraySeq[Lazy[Any]]) => Lazy(nil)),
  )


  val runtimeEvaluator: RuntimeEvaluator = new RuntimeEvaluator(RuntimeExterns.runtimeCommonenExterns ++ streamExterns)

  lazy val definitions: RuntimeEvaluator.Env = inStreams.view.mapValues(x => Lazy(x._1)).toMap ++ spec.definitions.map(d => (d._1.fullName, runtimeEvaluator.evalExpressionArg(d._2, definitions)))

  lazy val outStreams: Seq[(Option[String], Stream, Core.Type)] = spec.out.map { os =>
    val definition = definitions(os._1.fullName).get
    val nameOpt = Try(os._2("name")(0).asInstanceOf[Core.StringLiteralExpression].value).toOption
    (nameOpt, definition.asInstanceOf[Stream], null) // TODO find type of output stream
  }

}

object Interpreter {
  type Trace = Iterator[Trace.Event]

  case class InterperterError()

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
                value match {
                  case RuntimeEvaluator.RuntimeError(msg) => throw Errors.RuntimeError(msg)
                  case _ => nextEvents += Trace.Event(Location.unknown, timeStamp, idOpt, value)
                }
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
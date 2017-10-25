package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._

import scala.collection.immutable.SortedMap
import de.uni_luebeck.isp.tessla.{Location, TesslaCore}

object Specification {
  type Time = BigInt
}

import Specification._

class Specification() {
  private var timeVar: Time = 0
  private var trigger: (Map[Any, Time], SortedMap[Time, Set[Any]]) = (Map(), SortedMap())
  private var acceptInput = true

  def getTime: Time = timeVar

  private var inputs: List[Triggered] = Nil

  /**
    * Propagates all inputs without progressing time.
    * Can only be called once per point in time.
    * No more input values can be provided for the current time afterwards.
    */
  def step(): Unit = {
    if (acceptInput) {
      acceptInput = false
      for (input <- inputs) {
        input.step()
      }
    } else {
      throw ProvideAfterPropagationError(timeVar)
    }
  }

  /**
    * Propagates all inputs and progresses time.
    */
  def step(timeDelta: Time): Unit = {
    if (timeDelta > 0) {
      if (acceptInput) {
        step()
      }
      acceptInput = true
      val newTime = timeVar + timeDelta
      while (trigger._2.nonEmpty && trigger._2.head._1 < newTime) {
        val active = trigger._2.head
        trigger = (trigger._1 -- active._2, trigger._2.tail)
        timeVar = active._1
        for (input <- inputs) {
          input.step()
        }
      }

      val removed = trigger._2.takeWhile { case (t, _) => t <= newTime }
      val remaining = trigger._2.dropWhile { case (t, _) => t <= newTime }
      val map = removed.foldLeft(trigger._1)((x, y) => x -- y._2)
      trigger = (map, remaining)

      timeVar = newTime
    } else {
      throw InternalError(s"Specification.step called with non-positive delta: $timeDelta")
    }
  }

  private def updateTrigger(stream: Stream, newTime: Time): Unit = {
    if (newTime > getTime) {
      val remaining = trigger._1.get(stream) match {
        case Some(t) =>
          val set = trigger._2(t) - stream
          (trigger._1 - stream, if (set.isEmpty) trigger._2 - t else trigger._2 + (t -> set))
        case None => trigger
      }
      val temp: Set[Any] = remaining._2.get(newTime) match {
        case Some(set) => set + stream
        case None => Set(stream)
      }
      trigger = (remaining._1 + (stream -> newTime), remaining._2 + (newTime -> temp))
    } else {
      throw InternalError(s"updateTrigger has been called with newTime ($newTime) <= current time ($getTime)")
    }
  }

  def lift(streams: Seq[Stream])
          (op: Seq[TesslaCore.Value] => Option[TesslaCore.Value]): Stream =
    new Stream {
      private var state: Seq[Option[TesslaCore.Value]] = streams.map(_ => None)
      private var inputs: Array[Option[TesslaCore.Value]] = streams.map(_ => None).toArray
      private var counter = 0

      override protected def init(): Unit = {
        for ((stream, i) <- streams.zipWithIndex) {
          stream.addListener { value =>
            counter += 1
            inputs(i) = value
            if (counter == streams.length) {
              val newState = inputs.zip(state).map {
                case (in, st) => in.orElse(st)
              }
              val newOutput =
                if (inputs.exists(_.isDefined)) {
                  val tmp = newState.foldLeft(Some(Nil): Option[Seq[TesslaCore.Value]]) {
                    case (Some(list), Some(v)) => Some(v +: list)
                    case _ => None
                  }
                  tmp.flatMap(x => op(x.reverse))
                } else None
              counter = 0
              inputs = streams.map(_ => None).toArray
              state = newState
              propagate(newOutput)
            }
          }
        }
      }
    }

  def last(times: Stream, values: => Stream): Stream =
    new Stream {
      private var done = false
      private var oldValue: Option[TesslaCore.Value] = None
      private var newValue: Option[TesslaCore.Value] = None

      def update(): Unit = {
        if (done) {
          oldValue = newValue.orElse(oldValue)
        }
        done = !done
      }

      protected override def init(): Unit = {
        times.addListener {
          case Some(_) =>
            propagate(oldValue)
            update()
          case None =>
            propagate(None)
            update()
        }
        values.addListener {
          case Some(v) =>
            newValue = Some(v)
            update()
          case None => update()
        }
      }
    }

  def delayedLast(delays: => Stream, values: => Stream): Stream =
    new Triggered {
      private var oldValue: Option[TesslaCore.Value] = None
      private var newValue: Option[TesslaCore.Value] = None
      private var newDelay: Option[Time] = None
      private var targetTime: Option[Time] = None
      private var counter = 0

      def update(): Unit = {
        if (counter == 2) {
          counter = 0
          oldValue = newValue
          newDelay match {
            case Some(delay) =>
              val t = getTime + delay
              targetTime = Some(t)
              updateTrigger(this, t)
            case None =>
          }
        } else {
          counter += 1
        }
      }

      override def init(): Unit = {
        delays.addListener(delay => {
          newDelay = delay.map {
            case TesslaCore.IntLiteral(value, loc) =>
              if (value > 0) {
                value
              } else {
                throw NegativeDelayError(value, loc)
              }
            case _ =>
              throw InternalError("Uncaught type error: delayedLast called with non-int delay")
          }: Option[Time]
          update()
        })
        values.addListener {
          case Some(value) =>
            newValue = Some(value)
            update()
          case None => update()
        }
      }

      override def step() = {
        targetTime match {
          case Some(target) if target == getTime =>
            targetTime = None
            propagate(oldValue)
          case _ => propagate(None)
        }
        update()
      }
    }

  def nil: Stream =
    new Triggered {
      override def step() = {
        propagate(None)
      }
    }

  sealed class Stream {
    self =>

    /*List of listeners which get invoked on every single value propagation*/
    private var listeners: Seq[Option[TesslaCore.Value] => Unit] = Nil

    /*Register a new listener*/
    def addListener(listener: Option[TesslaCore.Value] => Unit): Unit = {
      if (listeners.isEmpty) {
        listeners +:= listener
        init()
      } else {
        listeners +:= listener
      }
    }

    protected def init(): Unit = {}

    def propagate(value: Option[TesslaCore.Value]): Unit = {
      for (listener <- listeners) {
        listener(value)
      }
    }

    def time(loc: Location): Stream =
      new Stream {
        override protected def init(): Unit = {
          self.addListener {
            value => propagate(value.map{_ => TesslaCore.IntLiteral(getTime, loc)})
          }
        }
      }

    def default(defaultValue: TesslaCore.Value): Stream =
      new Stream {
        override protected def init(): Unit = {
          self.addListener {
            case Some(v) => propagate(Some(v))
            case None => propagate(if (getTime == 0) Some(defaultValue) else None)
          }
        }
      }

    def default(when: Stream): Stream = {
      new Stream {
        override protected def init(): Unit = {
          var other: Option[Option[TesslaCore.Value]] = None
          var hasValue = false

          def listener(flip: Boolean)(value: Option[TesslaCore.Value]) = {
            other match {
              case Some(otherValue) =>
                val (newHasValue, result) =
                  (hasValue, if (flip) otherValue else value, if (flip) value else otherValue) match {
                    case (false, Some(v), _) => (true, Some(v))
                    case (false, _, Some(v)) => (true, Some(v))
                    case (s, v, _) => (s, v)
                  }
                hasValue = newHasValue
                propagate(result)
                other = None
              case None =>
                other = Some(value)
              }
          }

          self.addListener(listener(false))
          when.addListener(listener(true))
        }
      }
    }
  }

  sealed abstract class Triggered extends Stream {
    inputs +:= this

    def step(): Unit
  }

  final class Input extends Triggered {
    private var value: Option[TesslaCore.Value] = None

    def provide(value: TesslaCore.Value): Unit = {
      if (acceptInput) {
        this.value = Some(value)
      } else {
        throw ProvideAfterPropagationError(timeVar, value.loc)
      }
    }

    override def step(): Unit = {
      propagate(value)
      value = None
    }
  }

  def printStream(stream: Stream, name: String): Unit =
    stream.addListener {
      case Some(value) => println(s"$getTime: $name = $value")
      case None =>
    }
}

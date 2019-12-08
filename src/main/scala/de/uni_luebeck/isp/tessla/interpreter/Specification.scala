package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors._

import scala.collection.immutable.{ArraySeq, SortedMap}
import de.uni_luebeck.isp.tessla.Location

import scala.annotation.tailrec


object Specification {
  type Time = BigInt
}

import Specification._

// TODO: Streams or at least InputStreams should have locations for error messages
class Specification(unitValue: Any) {
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

  def lift(streams: ArraySeq[Stream])(op: ArraySeq[Option[Any]] => Option[Any]): Stream =
    new Stream {
      private var inputs: Array[Option[Any]] = streams.map(_ => None).toArray
      private var counter = 0

      override protected def init(): Unit = {
        for ((stream, i) <- streams.zipWithIndex) {
          stream.addListener { value =>
            counter += 1
            inputs(i) = value
            if (counter == streams.length) {
              val newOutput =
                if (inputs.exists(_.isDefined))
                  op(inputs.to(ArraySeq))
                else
                  None
              counter = 0
              inputs = streams.map(_ => None).toArray
              propagate(newOutput)
            }
          }
        }
      }
    }

  def last(values: => Stream, times: Stream): Stream =
    new Stream {
      private var done = false
      private var oldValue: Option[Any] = None
      private var newValue: Option[Any] = None

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

  def delay(delays: => Stream, resets: => Stream): Stream =
    new Triggered {
      private var newDelay: Option[Time] = None
      private var newReset: Boolean = false
      private var newTriggered: Boolean = false
      private var targetTime: Option[Time] = None
      private var counter = 0

      def update(): Unit = {
        if (counter == 2) {
          counter = 0
          if (newReset || newTriggered) {
            targetTime = newDelay.map(_ + getTime)
            targetTime.foreach(updateTrigger(this, _))
          }
        } else {
          counter += 1
        }
      }

      override def init(): Unit = {
        delays.addListener(delay => {
          newDelay = delay.map {d =>
            val d2 = d.asInstanceOf[BigInt]
            if (d2 <= 0) {
              throw NonPositiveDelayError(d2, Location.unknown)
            }
            d2
          }: Option[Time]
          update()
        })
        resets.addListener(reset => {
          newReset = reset.isDefined
          update()
        })
      }

      override def step(): Unit = {
        targetTime match {
          case Some(target) if target == getTime =>
            newTriggered = true
            targetTime = None
            propagate(Some(unitValue))
          case _ =>
            newTriggered = false
            propagate(None)
        }
        update()
      }
    }

  def nil: Stream =
    new Triggered {
      override def step(): Unit = {
        propagate(None)
      }
    }

  def merge(streams: ArraySeq[Stream]): Stream = {
    @tailrec
    def firstSome(values: ArraySeq[Option[Any]]): Option[Any] = values match {
      case Some(value1) +: _ => Some(value1)
      case _ +: tail => firstSome(tail)
      case ArraySeq() => None
    }
    lift(streams)(firstSome)
  }

  def slift(streams: ArraySeq[Stream])(op: ArraySeq[Any] => Any): Stream = {
    val ticks = lift(streams) { _ =>
      Some(())
    }
    val recentValueStreams = streams.map { stream =>
      merge(ArraySeq(stream, last(stream, ticks)))
    }
    lift(recentValueStreams) { valueOptions =>
      if (valueOptions.exists(_.isEmpty)) {
        None
      } else {
        Some(op(valueOptions.flatten))
      }
    }
  }

  sealed class Stream {
    self =>

    /*List of listeners which get invoked on every single value propagation*/
    private var listeners: Seq[Option[Any] => Unit] = Nil

    /*Register a new listener*/
    def addListener(listener: Option[Any] => Unit): Unit = {
      if (listeners.isEmpty) {
        listeners +:= listener
        init()
      } else {
        listeners +:= listener
      }
    }

    protected def init(): Unit = {}

    def propagate(value: Option[Any]): Unit = {
      for (listener <- listeners) {
        listener(value)
      }
    }

    def time(): Stream =
      new Stream {
        override protected def init(): Unit = {
          self.addListener {
            value => propagate(value.map(_ => getTime))
          }
        }
      }

    def default(defaultValue: Any): Stream =
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
          var other: Option[Option[Any]] = None
          var hasValue = false

          def listener(flip: Boolean)(value: Option[Any]): Unit = {
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

          self.addListener(listener(flip = false))
          when.addListener(listener(flip = true))
        }
      }
    }
  }

  sealed abstract class Triggered extends Stream {
    inputs +:= this

    def step(): Unit
  }

  final class Input extends Triggered {
    private var value: Option[Any] = None

    def provide(value: Any): Unit = {
      if (acceptInput) {
        this.value = Some(value)
      } else {
        throw ProvideAfterPropagationError(timeVar, Location.unknown)
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

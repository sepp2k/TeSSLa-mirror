package de.uni_luebeck.isp.tessla

import shapeless._

import scala.collection.immutable.SortedMap

class Specification[Time: Numeric]() {
  private val numeric = implicitly[Numeric[Time]]

  import numeric.{mkOrderingOps, mkNumericOps, zero, one}

  private var timeVar: Time = zero
  private var trigger: (Map[Any, Time], SortedMap[Time, Set[Any]]) = (Map(), SortedMap())
  private var acceptInput = true

  def currentTime = timeVar

  private var inputs: List[Triggered[_]] = Nil

  /**
    * Propagates all inputs without progressing time.
    * Can only be called once per point in time.
    * No more input values can be provided for the current time afterwards.
    */
  def step(): Unit = {
    require(acceptInput)
    acceptInput = false
    for (input <- inputs) {
      input.step()
    }
  }

  /**
    * Propagates all inputs and progresses time.z
    *
    * @param timeDelta
    */
  def step(timeDelta: Time): Unit = {
    require(timeDelta > zero)
    if (acceptInput) {
      step()
    }
    acceptInput = true
    val newTime = timeVar + timeDelta
    while (!trigger._2.isEmpty && trigger._2.head._1 < newTime) {
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
  }

  private def updateTrigger(stream: Stream[_], newTime: Time): Unit = {
    require(newTime > currentTime)
    val remaining = trigger._1.get(stream) match {
      case Some(t) => {
        val set = trigger._2(t) - stream
        (trigger._1 - stream, if (set.isEmpty) trigger._2 - t else trigger._2 + (t -> set))
      }
      case None => trigger
    }
    val temp: Set[Any] = remaining._2.get(newTime) match {
      case Some(set) => set + stream
      case None => Set(stream)
    }
    trigger = (remaining._1 + (stream -> newTime), remaining._2 + (newTime -> temp))
  }

  def lift[Value, Complete <: HList, Inputs <: HList, Streams <: HList](streams: Streams)(op: Complete => Option[Value])(implicit constraint: StreamConstraint[Complete, Inputs, Streams])
  = new Operation[Value, Inputs, Inputs, Streams](constraint.init, streams)((_, state, inputs) => {
    val newState = constraint.orElse(inputs, state)
    val newOutput = if (constraint.hasSome(inputs)) constraint.complete(newState).flatMap(x => op(x)) else None
    (newState, newOutput)
  })

  def last[Value](times: Stream[_], values: => Stream[Value]) = new Stream[Value] {
    private var done = false;
    private var oldValue: Option[Value] = None
    private var newValue: Option[Value] = None

    def update(): Unit = {
      if (done) {
        oldValue = newValue.orElse(oldValue)
      }
      done = !done
    }

    protected override def init(): Unit = {
      times.addListener{
        case Some(_) => {
          propagate(oldValue)
          update()
        }
        case None => update()
      }
      values.addListener {
        case Some(v) => {
          newValue = Some(v)
          update()
        }
        case None => update()
      }
    }
  }

  def delayedLast[Value](delays: => Stream[Time], values: => Stream[Value]) = new Triggered[Value] {
    private var oldValue: Option[Value] = None
    private var newValue: Option[Value] = None
    private var newDelay: Option[Time] = None
    private var targetTime: Option[Time] = None
    private var count = 0

    def update(): Unit = {
      if (count == 2) {
        count = 0
        oldValue = newValue
        newDelay match {
          case Some(delay) => {
            val t = targetTime.getOrElse(currentTime) + delay
            targetTime = Some(t)
            updateTrigger(this, t)
          }
          case None =>
        }
      } else {
        count += 1
      }
    }

    override def init(): Unit = {
      delays.addListener(delay => {
        newDelay = delay
        update()
      })
      values.addListener {
        case Some(value) => {
          newValue = Some(value)
          update()
        }
        case None => update()
      }
    }

    override private[Specification] def step() = {
      targetTime match {
        case Some(t) if (t equiv currentTime) => {
          targetTime = None
          propagate(oldValue)
        }
        case _ => propagate(None)
      }
      update()
    }
  }

  sealed class Stream[Value] {
    self =>
    private var listeners: List[Option[Value] => Unit] = Nil

    implicit def nilSteamConstraintFix: StreamConstraint[HNil, HNil, HNil] = nilSteamConstraint

    implicit def consStreamConstraintFix[Value, Complete <: HList, Inputs <: HList, InputStreams <: HList](implicit ev: StreamConstraint[Complete, Inputs, InputStreams]): StreamConstraint[Value :: Complete, Option[Value] :: Inputs, Stream[Value] :: InputStreams] = consStreamConstraint(ev)

    def addListener(listener: Option[Value] => Unit): Unit = {
      if (listeners.isEmpty) {
        listeners +:= listener
        init()
      } else {
        listeners +:= listener
      }
    }

    protected def init(): Unit = {

    }

    private[Specification] def propagate(value: Option[Value]): Unit = {
      for (listener <- listeners) {
        listener(value)
      }
    }

    def +[T: Numeric](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] = {
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Numeric[T]].plus(x.head, other))
      )
    }

    def time() = new Operation[Time, Unit, Option[Value] :: HNil, Stream[Value] :: HNil]((), this :: HNil)((t, s, i) => (s, Some(t)))

    def default(value: Value) = new Operation[Value, Unit, Option[Value] :: HNil, Stream[Value] :: HNil]((), self :: HNil)((t, _, i) => i.head match {
      case Some(value) => ((), Some(value))
      case None => ((), if (t equiv zero) Some(value) else None)
    })
  }

  sealed abstract class Triggered[Value]() extends Stream[Value] {
    inputs +:= this

    private[Specification] def step()

  }

  final class Input[Value]() extends Triggered[Value] {
    private var value: Option[Value] = None

    def provide(value: Value): Unit = {
      require(acceptInput)
      this.value = Some(value)
    }

    private[Specification] def step(): Unit = {
      propagate(value)
      value = None
    }

  }

  trait StreamConstraint[Complete <: HList, Inputs <: HList, InputStreams <: HList] {
    def length: Int

    def init: Inputs

    def register(streams: InputStreams, getState: () => Inputs, setState: Inputs => Unit): Unit

    def orElse(inputs: Inputs, fallback: Inputs): Inputs

    def hasSome(inputs: Inputs): Boolean

    def complete(inputs: Inputs): Option[Complete]
  }

  implicit def nilSteamConstraint = new StreamConstraint[HNil, HNil, HNil] {
    override val length = 0
    override val init = HNil

    override def register(streams: HNil, getState: () => HNil, setState: HNil => Unit): Unit = {

    }

    override def orElse(inputs: HNil, fallback: HNil) = HNil

    override def complete(inputs: HNil) = Some(HNil)

    override def hasSome(inputs: HNil) = false
  }

  implicit def consStreamConstraint[Value, Complete <: HList, Inputs <: HList, InputStreams <: HList](implicit ev: StreamConstraint[Complete, Inputs, InputStreams]) = new StreamConstraint[Value :: Complete, Option[Value] :: Inputs, Stream[Value] :: InputStreams]() {
    override val length = ev.length + 1
    override val init = None :: ev.init

    override def register(streams: Stream[Value] :: InputStreams, getState: () => Option[Value] :: Inputs, setState: (Option[Value] :: Inputs) => Unit): Unit = {
      streams.head.addListener {
        value => setState(value :: getState().tail)
      }
      ev.register(streams.tail, () => getState().tail, state => getState().head :: state)
    }

    override def orElse(inputs: Option[Value] :: Inputs, fallback: Option[Value] :: Inputs) = inputs.head.orElse(fallback.head) :: ev.orElse(inputs.tail, fallback.tail)

    override def complete(inputs: Option[Value] :: Inputs) = inputs.head.flatMap(x => ev.complete(inputs.tail).map(y => x :: y))

    override def hasSome(inputs: Option[Value] :: Inputs) = inputs.head.isDefined || ev.hasSome(inputs.tail)

  }

  final class Operation[Value, State, Inputs <: HList, InputStreams <: HList]
  (init: State, inputStreams: InputStreams)
  (op: (Time, State, Inputs) => (State, Option[Value]))
  (implicit constraint: StreamConstraint[_, Inputs, InputStreams]) extends Stream[Value] {
    private var state: State = init
    private var inputs: Inputs = constraint.init
    private var count = 0

    override protected def init(): Unit = {
      constraint.register(inputStreams, () => inputs, (newInputs) => {
        count += 1
        inputs = newInputs
        if (count == constraint.length) {
          val (newState, output) = op(Specification.this.currentTime, state, inputs)
          count = 0
          inputs = constraint.init
          state = newState
          propagate(output)
        }
      })
    }
  }

  def Input[Value]() = new Input[Value]()

  def printStream(stream: Stream[_], name: String): Unit = {
    stream.addListener {
      case Some(value) => println(s"$currentTime $name: $value")
      case None =>
    }
  }

}

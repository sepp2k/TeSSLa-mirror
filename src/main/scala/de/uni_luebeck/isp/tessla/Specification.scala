package de.uni_luebeck.isp.tessla

import shapeless._

import scala.collection.immutable.SortedMap

import ImplicitConstraints._

import scala.language.implicitConversions

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
  = Operation[Value, Inputs, Inputs, Streams](constraint.init, streams)((_, state, inputs) => {
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
      times.addListener {
        case Some(_) => {
          propagate(oldValue)
          update()
        }
        case None => {
          propagate(None)
          update()
        }
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
    private var counter = 0

    def update(): Unit = {
      if (counter == 2) {
        counter = 0
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
        counter += 1
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

  def nil[Value]() = new Triggered[Value] {
    override private[Specification] def step() = {
      propagate(None)
    }
  }

  def const[Value](value: Value) = nil().default(value)

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

    def const[T](other: T): Stream[T] =
      lift(this :: HNil)(
        (_: Value :: HNil) => Some(other)
      )

    def alsoAt[T](other: Stream[T]): Stream[Value] =
      lift(this :: other :: HNil)(
        (x: Value :: T :: HNil) => Some(x.head)
      )

    def defined[T](other: Stream[T]): Stream[T] = (other.alsoAt(this).time() === time().alsoAt(other)).ifThen(other)

    def &&(other: Boolean)(implicit ev: Stream[Value] =:= Stream[Boolean]): Stream[Boolean] =
      lift(ev(this) :: HNil)(
        (x: Boolean :: HNil) => Some(x.head && other)
      )

    def &&(other: Stream[Boolean])(implicit ev: Stream[Value] =:= Stream[Boolean]): Stream[Boolean] =
      lift(ev(this) :: other :: HNil)(
        (x: Boolean :: Boolean :: HNil) => Some(x.head && x.tail.head)
      )

    def ||(other: Boolean)(implicit ev: Stream[Value] =:= Stream[Boolean]): Stream[Boolean] =
      lift(ev(this) :: HNil)(
        (x: Boolean :: HNil) => Some(x.head || other)
      )

    def ||(other: Stream[Boolean])(implicit ev: Stream[Value] =:= Stream[Boolean]): Stream[Boolean] =
      lift(ev(this) :: other :: HNil)(
        (x: Boolean :: Boolean :: HNil) => Some(x.head || x.tail.head)
      )

    def unary_!()(implicit ev: Stream[Value] =:= Stream[Boolean]): Stream[Boolean] =
      lift(ev(this) :: HNil)(
        (x: Boolean :: HNil) => Some(!x.head)
      )

    def <[T: PartialOrdering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[PartialOrdering[T]].lt(x.head, other))
      )

    def <[T: PartialOrdering](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[PartialOrdering[T]].lt(x.head, x.tail.head))
      )

    def <=[T: PartialOrdering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[PartialOrdering[T]].lteq(x.head, other))
      )

    def <=[T: PartialOrdering](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[PartialOrdering[T]].lteq(x.head, x.tail.head))
      )

    def >[T: Ordering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Ordering[T]].gt(x.head, other))
      )

    def >[T: PartialOrdering](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[PartialOrdering[T]].gt(x.head, x.tail.head))
      )

    def >=[T: PartialOrdering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[PartialOrdering[T]].gteq(x.head, other))
      )

    def >=[T: PartialOrdering](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[PartialOrdering[T]].gteq(x.head, x.tail.head))
      )

    def max[T: Ordering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Ordering[T]].max(x.head, other))
      )

    def max[T: Ordering](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[Ordering[T]].max(x.head, x.tail.head))
      )

    def min[T: Ordering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Ordering[T]].min(x.head, other))
      )

    def min[T: Ordering](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[Ordering[T]].min(x.head, x.tail.head))
      )

    def compare[T: Ordering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[Int] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Ordering[T]].compare(x.head, other))
      )

    def compare[T: Ordering](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[Int] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[Ordering[T]].compare(x.head, x.tail.head))
      )

    def tryCompare[T: PartialOrdering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[Option[Int]] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[PartialOrdering[T]].tryCompare(x.head, other))
      )

    def tryCompare[T: PartialOrdering](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[Option[Int]] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[PartialOrdering[T]].tryCompare(x.head, x.tail.head))
      )

    def +[T: Numeric](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Numeric[T]].plus(x.head, other))
      )

    def +[T: Numeric](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[Numeric[T]].plus(x.head, x.tail.head))
      )

    def -[T: Numeric](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Numeric[T]].minus(x.head, other))
      )

    def -[T: Numeric](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[Numeric[T]].minus(x.head, x.tail.head))
      )

    def *[T: Numeric](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Numeric[T]].times(x.head, other))
      )

    def *[T: Numeric](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: other :: HNil)(
        (x: T :: T :: HNil) => Some(implicitly[Numeric[T]].times(x.head, x.tail.head))
      )

    def unary_/[T: Numeric]()(implicit ev: Stream[Value] =:= Stream[T]): Stream[T] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Numeric[T]].negate(x.head))
      )

    def /(other: Int)(implicit ev: Stream[Value] =:= Stream[Int]): Stream[Int] =
      lift(ev(this) :: HNil)(
        (x: Int :: HNil) => Some(x.head / other)
      )

    def /(other: Double)(implicit ev: Stream[Value] =:= Stream[Double]): Stream[Double] =
      lift(ev(this) :: HNil)(
        (x: Double :: HNil) => Some(x.head / other)
      )

    def /(other: Stream[Value])(implicit ev: (Stream[Value] =:= Stream[Int]) ||: (Stream[Value] =:= Stream[Double]) ||: CFalse): Stream[Value] = {
      def intCase(intEv: Stream[Value] =:= Stream[Int]) = intEv.inverse(lift(intEv(this) :: intEv(other) :: HNil)(
        (x: Int :: Int :: HNil) => Some(x.head / x.tail.head)
      ))

      def doubleCase(doubleEv: Stream[Value] =:= Stream[Double]) = doubleEv.inverse(lift(doubleEv(this) :: doubleEv(other) :: HNil)(
        (x: Double :: Double :: HNil) => Some(x.head / x.tail.head)
      ))

      ev.switch(intCase _ :: doubleCase _ :: HNil)
    }

    def %(other: Int)(implicit ev: Stream[Value] =:= Stream[Int]): Stream[Int] =
      lift(ev(this) :: HNil)(
        (x: Int :: HNil) => Some(x.head % other)
      )

    def %(other: Double)(implicit ev: Stream[Value] =:= Stream[Double]): Stream[Double] =
      lift(ev(this) :: HNil)(
        (x: Double :: HNil) => Some(x.head % other)
      )

    def toInt[T: Numeric](implicit ev: Stream[Value] =:= Stream[T]): Stream[Int] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Numeric[T]].toInt(x.head))
      )

    def toDouble[T: Numeric](implicit ev: Stream[Value] =:= Stream[T]): Stream[Double] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[Numeric[T]].toDouble(x.head))
      )

    def %(other: Stream[Value])(implicit ev: (Stream[Value] =:= Stream[Int]) ||: (Stream[Value] =:= Stream[Double]) ||: CFalse): Stream[Value] = {
      def intCase(intEv: Stream[Value] =:= Stream[Int]) = intEv.inverse(lift(intEv(this) :: intEv(other) :: HNil)(
        (x: Int :: Int :: HNil) => Some(x.head % x.tail.head)
      ))

      def doubleCase(doubleEv: Stream[Value] =:= Stream[Double]) = doubleEv.inverse(lift(doubleEv(this) :: doubleEv(other) :: HNil)(
        (x: Double :: Double :: HNil) => Some(x.head % x.tail.head)
      ))

      ev.switch(intCase _ :: doubleCase _ :: HNil)
    }

    def ===(other: Stream[Value]): Stream[Boolean] =
      lift(this :: other :: HNil)(
        (x: Value :: Value :: HNil) => Some(x.head == x.tail.head)
      )

    def ifThen[T](other: Stream[T])(implicit ev: Stream[Value] =:= Stream[Boolean]): Stream[T] =
      lift(ev(this) :: other :: HNil)(
        (x: Boolean :: T :: HNil) => if (x.head) Some(x.tail.head) else None
      )

    def ifThenElse[T](other1: Stream[T], other2: Stream[T])(implicit ev: Stream[Value] =:= Stream[Boolean]): Stream[T] =
      lift(ev(this) :: other1 :: other2 :: HNil)(
        (x: Boolean :: T :: T :: HNil) => if (x.head) Some(x.tail.head) else Some(x.tail.tail.head)
      )

    def fold[T](init: T)(f: (Stream[T], Stream[Value]) => Stream[T]) = {
      lazy val result: Stream[T] = f(last(this, result).default(init), this)
      result
    }

    def resetFold[T](init: T, reset: => Stream[Boolean])(f: (Stream[T], Stream[Value]) => Stream[T]) = {
      lazy val state: Stream[T] = f(last(this, result).default(init), this).default(init)
      lazy val result: Stream[T] = this.defined(reset.default(false)).ifThenElse(f(nil[T].default(init), this), state)
      (Lazy(result), Lazy(state))
    }

    def reduce(f: (Stream[Value], Stream[Value]) => Stream[Value]) = {
      lazy val result: Stream[Value] = f(last(this, result), this).default(this)
      result
    }


    def sum[T: Numeric](implicit ev: Stream[Value] =:= Stream[T]) = ev(this).fold(implicitly[Numeric[T]].zero)(_ + _)

    def resetSum[T: Numeric](reset: => Stream[Boolean])(implicit ev: Stream[Value] =:= Stream[T]): (Lazy[Stream[T]], Lazy[Stream[T]]) = ev(this).resetFold(implicitly[Numeric[T]].zero, reset)(_ + _)

    def prod[T: Numeric](implicit ev: Stream[Value] =:= Stream[T]) = ev(this).fold(implicitly[Numeric[T]].one)(_ * _)

    def max[T: Ordering](implicit ev: Stream[Value] =:= Stream[T]): Stream[T] = ev(this).reduce(_ max _)

    def min[T: Ordering](implicit ev: Stream[Value] =:= Stream[T]): Stream[T] = ev(this).reduce(_ min _)

    def count: Stream[Int] = const(1).sum

    def resetCount(reset: => Stream[Boolean]): (Lazy[Stream[Int]], Lazy[Stream[Int]]) = const(1).resetSum(reset)

    def exists(implicit ev: Stream[Value] =:= Stream[Boolean]) = ev(this).fold(false)(_ || _)

    def forall(implicit ev: Stream[Value] =:= Stream[Boolean]) = ev(this).fold(true)(_ && _)

    def time() = Operation[Time, Unit, Option[Value] :: HNil, Stream[Value] :: HNil]((), this :: HNil)((t, s, i) => (s, if (i.head.isDefined) Some(t) else None))

    def default(value: Value) = Operation[Value, Unit, Option[Value] :: HNil, Stream[Value] :: HNil]((), self :: HNil)((t, _, i) => i.head match {
      case Some(value) => ((), Some(value))
      case None => ((), if (t equiv zero) Some(value) else None)
    })

    def default[T](value: Value, when: Stream[T]) = Operation[Value, Boolean, Option[Value] :: Option[T] :: HNil, Stream[Value] :: Stream[T] :: HNil](false, self :: when :: HNil) {
      case (_, false, Some(v) :: _) => (true, Some(v))
      case (_, false, _ :: Some(_) :: _) => (true, Some(value))
      case (_, s, _) => (s, None)
    }

    def default(when: Stream[Value]) = Operation[Value, Boolean, Option[Value] :: Option[Value] :: HNil, Stream[Value] :: Stream[Value] :: HNil](false, self :: when :: HNil) {
      case (_, false, Some(v) :: _) => (true, Some(v))
      case (_, false, _ :: Some(v) :: _) => (true, Some(v))
      case (_, s, _) => (s, None)
    }
  }

  sealed abstract class Triggered[Value]() extends Stream[Value] {
    inputs +:= this

    private[Specification] def step()

  }

  final class Input[Value] private[Specification]() extends Triggered[Value] {
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

  sealed trait StreamConstraint[Complete <: HList, Inputs <: HList, InputStreams <: HList] {
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
      ev.register(streams.tail, () => getState().tail, state => setState(getState().head :: state))
    }

    override def orElse(inputs: Option[Value] :: Inputs, fallback: Option[Value] :: Inputs) = inputs.head.orElse(fallback.head) :: ev.orElse(inputs.tail, fallback.tail)

    override def complete(inputs: Option[Value] :: Inputs) = inputs.head.flatMap(x => ev.complete(inputs.tail).map(y => x :: y))

    override def hasSome(inputs: Option[Value] :: Inputs) = inputs.head.isDefined || ev.hasSome(inputs.tail)

  }

  def Operation[Value, State, Inputs <: HList, InputStreams <: HList]
  (initState: State, inputStreams: InputStreams)
  (op: (Time, State, Inputs) => (State, Option[Value]))
  (implicit constraint: StreamConstraint[_, Inputs, InputStreams]) = new Stream[Value] {
    private var state: State = initState
    private var inputs: Inputs = constraint.init
    private var counter = 0

    override protected def init(): Unit = {
      constraint.register(inputStreams, () => inputs, (newInputs) => {
        counter += 1
        inputs = newInputs
        if (counter == constraint.length) {
          val (newState, output) = op(Specification.this.currentTime, state, inputs)
          counter = 0
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

  class Lazy[A](a: => A) {
    lazy val get = a
  }

  object Lazy {
    def apply[A](a: => A) = new Lazy(a)
  }

  implicit def forceLazy[A](l: Lazy[A]): A = l.get
  
}

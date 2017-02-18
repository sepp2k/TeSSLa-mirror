package de.uni_luebeck.isp.tessla

import shapeless._

import scala.collection.immutable.SortedMap

import scala.language.higherKinds

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
      times.addListener {
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
      lift(other :: this :: HNil)(
        (x: T :: Value :: HNil) => Some(x.tail.head)
      )

    def defined[T](other: Stream[T]): Stream[T] = (time() === time().alsoAt(other)).ifThen(other)

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

    def >[T: PartialOrdering](other: T)(implicit ev: Stream[Value] =:= Stream[T]): Stream[Boolean] =
      lift(ev(this) :: HNil)(
        (x: T :: HNil) => Some(implicitly[PartialOrdering[T]].gt(x.head, other))
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

    def /(other: Stream[Value])(implicit ev: (Stream[Value] =:= Stream[Int]) ||: (Stream[Value] =:= Stream[Double]) ||: CNil): Stream[Value] = {
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

    def %(other: Stream[Value])(implicit ev: (Stream[Value] =:= Stream[Int]) ||: (Stream[Value] =:= Stream[Double]) ||: CNil): Stream[Value] = {
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

    def time() = new Operation[Time, Unit, Option[Value] :: HNil, Stream[Value] :: HNil]((), this :: HNil)((t, s, i) => (s, if (i.head.isDefined) Some(t) else None))

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
      ev.register(streams.tail, () => getState().tail, state => setState(getState().head :: state))
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

  trait ConstraintOr {
    type FS[C] <: HList
  }

  type CNil = ConstraintNil.type

  object ConstraintNil extends ConstraintOr {
    override type FS[C] = HNil
  }

  trait ||:[A, B <: ConstraintOr] extends ConstraintOr {
    override type FS[C] = (A => C) :: B#FS[C]
    def switch[C](fs: FS[C]): C
  }

  implicit val implicitConstraintNil: CNil = ConstraintNil

  implicit def implicitConstraintOrA[A, B <: ConstraintOr](implicit a: A) = new ||:[A, B] {
    override def switch[C](fs: FS[C]): C = fs.head(a)
  }

  implicit def implicitConstraintOrB[A, B, C <: ConstraintOr](implicit b: B ||: C) = new ||:[A, B ||: C] {
    override def switch[C](fs: FS[C]): C = b.switch(fs.tail)
  }

  sealed abstract class =:=[From, To] extends (From => To) with Serializable {
    def inverse(to: To): From
  }

  private[this] final val singleton_=:= = new =:=[Any, Any] {
    def apply(x: Any): Any = x

    def inverse(x: Any): Any = x
  }

  object =:= {
    implicit def tpEquals[A]: A =:= A = singleton_=:=.asInstanceOf[A =:= A]
  }

}

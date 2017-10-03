package de.uni_luebeck.isp.tessla.interpreter

import shapeless._

import scala.collection.immutable.SortedMap
import ImplicitConstraints._
import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.{TesslaCore, UnknownLoc}

import scala.language.implicitConversions

class Specification() {
  private var timeVar: BigInt = 0
  private var trigger: (Map[Any, BigInt], SortedMap[BigInt, Set[Any]]) = (Map(), SortedMap())
  private var acceptInput = true

  def getTime: BigInt = timeVar

  private var inputs: List[Triggered] = Nil

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
  def step(timeDelta: BigInt): Unit = {

    require(timeDelta > 0)
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
  }

  private def updateTrigger(stream: Stream, newTime: BigInt): Unit = {
    require(newTime > getTime)
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
  }

  def lift(
            streams: Seq[Stream]
          )(
            op: Seq[TesslaCore.Value] => Option[TesslaCore.Value]
          ): Stream =
    Operation[Seq[Option[TesslaCore.Value]], TesslaCore.Value](streams.map(_ => None), streams)((_, state, inputs) => {
      val newState = inputs.zip(state).map {
        case (in, st) => in.orElse(st)
      }
      val newOutput =
        if (inputs.exists(_.isDefined)) {
          val tmp = newState.foldLeft(Some(Nil): Option[List[TesslaCore.Value]]) {
            case (Some(list), Some(value)) => Some(value :: list)
            case _ => None
          }
          tmp.flatMap(x => op(x.reverse))
        } else None

      (newState, newOutput)
    })

  def lift[Value, Complete <: HList, Inputs <: HList, Streams <: HList](
                                                                         streams: Streams
                                                                       )(
                                                                         op: Complete => Option[TesslaCore.Value]
                                                                       )(
                                                                         implicit constraint: StreamConstraint[Complete, Inputs, Streams]
                                                                       ): Stream =
    Operation[Value, Inputs, Inputs, Streams](constraint.init, streams)((_, state, inputs) => {
      val newState = constraint.orElse(inputs, state)
      val newOutput =
        if (constraint.hasSome(inputs)) {
          constraint.complete(newState).flatMap(x => op(x))
        } else None

      (newState, newOutput)
    })

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
      private var newDelay: Option[BigInt] = None
      private var targetTime: Option[BigInt] = None
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
                throw new Exception("Not allowed")
              }
            case _ => throw new Exception("Not allowed")
          }: Option[BigInt]
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

  def const(value: TesslaCore.Value): Stream = nil.default(value)

  def period(value: BigInt): Stream = {
    require(value > 0)
    lazy val result: Stream = delayedLast(result, result).default(TesslaCore.IntLiteral(value, UnknownLoc))
    result.const(TesslaCore.Unit(UnknownLoc))
  }

  sealed class Stream {
    self =>

    private var listeners: List[Option[TesslaCore.Value] => Unit] = Nil

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

    def const(other: TesslaCore.Value): Stream =
      lift(this :: HNil)(
        (_: TesslaCore.Value :: HNil) => Some(other)
      )

    def time(): Stream =
      Operation[BigInt, Unit, Option[TesslaCore.Value] :: HNil, Stream :: HNil]((), this :: HNil)(
        (t, s, i) => (s, if (i.head.isDefined) Some(TesslaCore.IntLiteral(t, UnknownLoc)) else None))

    def default(value: TesslaCore.Value): Stream =
      Operation[TesslaCore.Value, Unit, Option[TesslaCore.Value] :: HNil, Stream :: HNil]((), self :: HNil) {
        case (t, _, Some(value) :: _) => ((), Some(value))
        case (t, _, None :: _) => ((), if (t == 0) Some(value) else None)
      }

    def default(when: Stream): Stream =
      Operation[TesslaCore.Value, Boolean, Option[TesslaCore.Value] :: Option[TesslaCore.Value] :: HNil, Stream :: Stream :: HNil](
        false, self :: when :: HNil
      ) {
        case (_, false, Some(v) :: _) => (true, Some(v))
        case (_, false, _ :: Some(v) :: _) => (true, Some(v))
        case (_, s, v :: _) => (s, v)
      }
  }

  sealed abstract class Triggered extends Stream {
    inputs +:= this

    def step(): Unit
  }

  final class Input extends Triggered {
    private var value: Option[TesslaCore.Value] = None

    def provide(value: TesslaCore.Value): Unit = {
      require(acceptInput)
      this.value = Some(value)
    }

    override def step(): Unit = {
      propagate(value)
      value = None
    }

  }

  sealed abstract class StreamConstraint[Complete <: HList, Inputs <: HList, InputStreams <: HList]() {
    def length: Int

    def init: Inputs

    def register(streams: InputStreams, getState: () => Inputs, setState: Inputs => Unit): Unit

    def orElse(inputs: Inputs, fallback: Inputs): Inputs

    def hasSome(inputs: Inputs): Boolean

    def complete(inputs: Inputs): Option[Complete]
  }

  implicit val nilSteamConstraint: StreamConstraint[HNil, HNil, HNil] =
    new StreamConstraint[HNil, HNil, HNil] {
      override val length = 0
      override val init = HNil

      override def register(streams: HNil, getState: () => HNil, setState: HNil => Unit): Unit = {
      }

      override def orElse(inputs: HNil, fallback: HNil) =
        HNil

      override def complete(inputs: HNil) =
        Some(HNil)

      override def hasSome(inputs: HNil) =
        false
    }


  implicit def consStreamConstraint[ Complete <: HList, Inputs <: HList, InputStreams <: HList](
       implicit ev: StreamConstraint[Complete, Inputs, InputStreams]): StreamConstraint[TesslaCore.Value :: Complete, Option[TesslaCore.Value] :: Inputs, Stream :: InputStreams] =
    new StreamConstraint[TesslaCore.Value :: Complete, Option[TesslaCore.Value] :: Inputs, Stream :: InputStreams]() {
      override val length = ev.length + 1
      override val init = None :: ev.init

      override def register(
                             streams: Stream :: InputStreams, getState: () => Option[TesslaCore.Value] :: Inputs,
                             setState: (Option[TesslaCore.Value] :: Inputs) => Unit
                           ): Unit = {
        streams.head.addListener {
          value => setState(value :: getState().tail)
        }
        ev.register(streams.tail, () => getState().tail, state => setState(getState().head :: state))
      }

      override def orElse(inputs: Option[TesslaCore.Value] :: Inputs, fallback: Option[TesslaCore.Value] :: Inputs) =
        inputs.head.orElse(fallback.head) :: ev.orElse(inputs.tail, fallback.tail)

      override def complete(inputs: Option[TesslaCore.Value] :: Inputs) =
        inputs.head.flatMap(x => ev.complete(inputs.tail).map(y => x :: y))

      override def hasSome(inputs: Option[TesslaCore.Value] :: Inputs) =
        inputs.head.isDefined || ev.hasSome(inputs.tail)

    }


  def Operation[Value, State, Inputs <: HList, InputStreams <: HList](
                                                                       initState: State, inputStreams: InputStreams
                                                                     )(
                                                                       op: (BigInt, State, Inputs) => (State, Option[TesslaCore.Value])
                                                                     )(
                                                                       implicit constraint: StreamConstraint[_, Inputs, InputStreams]
                                                                     ): Stream =
    new Stream {
      private var state: State = initState
      private var inputs: Inputs = constraint.init
      private var counter = 0

      override protected def init(): Unit = {
        constraint.register(inputStreams, () => inputs, (newInputs) => {
          counter += 1
          inputs = newInputs
          if (counter == constraint.length) {
            val (newState, output) = op(Specification.this.getTime, state, inputs)
            counter = 0
            inputs = constraint.init
            state = newState
            propagate(output)
          }
        })
      }
    }

  def Operation[State, Value](
                               initState: State, inputStreams: Seq[Stream]
                             )(
                               op: (BigInt, State, Seq[Option[TesslaCore.Value]]) => (State, Option[TesslaCore.Value])
                             ): Stream =
    new Stream {
      private var state: State = initState
      private var inputs: Array[Option[TesslaCore.Value]] = inputStreams.map(_ => None).toArray
      private var counter = 0

      override protected def init(): Unit = {
        for ((stream, i) <- inputStreams.zipWithIndex) {
          stream.addListener { value =>
            counter += 1
            inputs(i) = value
            if (counter == inputStreams.length) {
              val (newState, output) = op(Specification.this.getTime, state, inputs)
              counter = 0
              inputs = inputStreams.map(_ => None).toArray
              state = newState
              propagate(output)
            }
          }
        }
      }
    }

  def Input(): Input = new Input()


  def printStream(stream: Stream, name: String): Unit =
    stream.addListener {
      case Some(value) => println(s"$getTime: $name = $value")
      case None =>
    }

  sealed class RegionMark

  case object RegionOutside extends RegionMark

  case object RegionBegin extends RegionMark

  case object RegionInside extends RegionMark

  final class ResetStream(value: => Stream, proposed_ : => Stream) extends Stream {

    override def init(): Unit = {
      value.addListener(x => propagate(x))
    }

    def proposed: Stream = proposed_
  }

  object ResetStream {
    def apply[A](value: => Stream, proposed: => Stream): ResetStream =
      new ResetStream(value, proposed)
  }

}

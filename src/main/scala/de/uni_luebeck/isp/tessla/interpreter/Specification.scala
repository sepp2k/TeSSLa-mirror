package de.uni_luebeck.isp.tessla.interpreter

import scala.collection.immutable.SortedMap
import de.uni_luebeck.isp.tessla.Errors.TesslaError
import de.uni_luebeck.isp.tessla.{TesslaCore, UnknownLoc}

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

  /*used in update*/
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

  /*used*/
  def lift(streams: Seq[Stream])
          (op: Seq[TesslaCore.Value] => Option[TesslaCore.Value]): Stream =
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

  /*used*/
  def lift(streams: List[Stream])(op: List[TesslaCore.Value] => Option[TesslaCore.Value]): Stream = {
    val constraint = consStreamConstraint
    Operation[List[TesslaCore.Value]](constraint.init, streams)((_, state, inputs) => {
      val newState = constraint.orElse(inputs, state)
      val newOutput =
        op(constraint.complete(newState))
      (newState, newOutput)
    })(consStreamConstraint)
  }

  /*used*/
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

  /*used*/
  def delayedLast(delays: => Stream, values: => Stream): Stream =
    new Triggered {
      private var oldValue: Option[TesslaCore.Value] = None
      private var newValue: Option[TesslaCore.Value] = None
      private var newDelay: Option[BigInt] = None
      private var targetTime: Option[BigInt] = None
      private var counter = 0

      /*used in init and step*/
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

      /*used in addlistener*/
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

  /*nil*/
  def nil: Stream =
    new Triggered {
      override def step() = {
        propagate(None)
      }
    }

  sealed class Stream {
    self =>

    /*List of listeners which get invoked on every single value propagation*/
    private var listeners: List[Option[TesslaCore.Value] => Unit] = Nil

    /*Register a new listener*/
    def addListener(listener: Option[TesslaCore.Value] => Unit): Unit = {
      if (listeners.isEmpty) {
        listeners +:= listener
        init()
      } else {
        listeners +:= listener
      }
    }

    /**/
    protected def init(): Unit = {}

    def propagate(value: Option[TesslaCore.Value]): Unit = {
      for (listener <- listeners) {
        listener(value)
      }
    }

    def time(): Stream =
      Operation[Unit]((), this :: Nil)(
        (t, s, i) => (s, if (i.nonEmpty) Some(TesslaCore.IntLiteral(t, UnknownLoc)) else None))(consStreamConstraint)

    def default(value: TesslaCore.Value): Stream =
      Operation[Unit]((), self :: Nil) {
        case (t, _, v :: _) => ((), Some(v))
        case (t, _, Nil) => ((), if (t == 0) Some(value) else None)
      }(consStreamConstraint)

    def default(when: Stream): Stream =
      Operation[Boolean](
        false, self :: when :: Nil
      ) {
        case (_, false, v :: _) => (true, Some(v))
        case (_, false, _ :: v :: _) => (true, Some(v))
        case (_, s, l) => (s, l.headOption)
      }(consStreamConstraint)
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

  sealed abstract class StreamConstraint() {
    def length: Int

    def init: List[TesslaCore.Value]

    def register(streams: List[Stream], getState: () => List[TesslaCore.Value], setState: List[TesslaCore.Value]=> Unit): Unit

    def orElse(inputs: List[TesslaCore.Value], fallback: List[TesslaCore.Value]): List[TesslaCore.Value]

    def hasSome(inputs: List[TesslaCore.Value]): Boolean

    def complete(inputs: List[TesslaCore.Value]): List[TesslaCore.Value]
  }

  val nilSteamConstraint: StreamConstraint =
    new StreamConstraint {
      override val length = 0
      override val init = Nil

      override def register(streams: List[Stream], getState: () => List[TesslaCore.Value], setState: List[TesslaCore.Value] => Unit): Unit = {
      }

      override def orElse(inputs: List[TesslaCore.Value], fallback: List[TesslaCore.Value]) =
        Nil

      override def complete(inputs: List[TesslaCore.Value]) =
        Nil

      override def hasSome(inputs: List[TesslaCore.Value]) =
        false
    }


  def consStreamConstraint: StreamConstraint =
    new StreamConstraint() {
      val ev = nilSteamConstraint
      override val length = ev.length + 1
      override val init = ev.init

      override def register(
                             streams: List[Stream], getState: () => List[TesslaCore.Value],
                             setState: List[TesslaCore.Value]=> Unit
                           ): Unit = {
        streams.head.addListener {
          value =>setState(value.map(_ :: getState().tail).getOrElse(getState().tail))
        }
        ev.register(streams.tail, () => getState().tail, state => setState(getState().head :: state))
      }

      override def orElse(inputs: List[TesslaCore.Value], fallback: List[TesslaCore.Value]) =
        inputs.headOption.getOrElse(fallback.head) :: ev.orElse(inputs.tail, fallback.tail)

      override def complete(inputs: List[TesslaCore.Value]) =
        inputs.flatMap(x => ev.complete(inputs.tail).flatMap(y => List(x,y)))

      override def hasSome(inputs: List[TesslaCore.Value]) =
        inputs.nonEmpty || ev.hasSome(inputs.tail)

    }


  def Operation[State](
                                                                       initState: State, inputStreams: List[Stream]
                                                                     )(
                                                                       op: (BigInt, State, List[TesslaCore.Value]) => (State, Option[TesslaCore.Value])
                                                                     )(
                                                                        constraint: StreamConstraint
                                                                     ): Stream =
    new Stream {
      private var state: State = initState
      private var inputs: List[TesslaCore.Value]= constraint.init
      private var counter = 0

      override protected def init(): Unit = {
        constraint.register(inputStreams, () => inputs, (newInputStreams) => {
          counter += 1
          inputs = newInputStreams
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

}

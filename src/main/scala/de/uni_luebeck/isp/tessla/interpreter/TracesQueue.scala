package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.DecreasingTimeStampsError
import de.uni_luebeck.isp.tessla.interpreter.Traces.TimeRange

import scala.collection.mutable

class TracesQueue(val threshold: BigInt) {
  /**
    * A PriorityQueue, having a BigInt as timestamp and using the lowest value has highest priority.
    */
  val queue: mutable.PriorityQueue[Traces.Event] =
    new mutable.PriorityQueue[Traces.Event]()(Ordering.by(ev => ev.timeRange.from)).reverse

  /**
    * Dequeues and processes all events with a time stamp lower than the one of the event subtracted by the threshold,
    * and then adds the new event to the queue.
    */
  def enqueue(event: Traces.Event, callback: Traces.Event => Unit): Unit = {
    dequeue(event.timeRange.from).foreach(callback)
    // Note: queue.min actually looks for the highest timestamp (which is the lowest priority, therefore min)
    if (queue.nonEmpty && event.timeRange.from <= queue.min(queue.ord).timeRange.from - threshold){
      // New Input has a too small timestamp
      throw DecreasingTimeStampsError(queue.last.timeRange.from, event.timeRange.from, event.loc)
    }
    queue.enqueue(event)
  }

  /**
    * Dequeue every event which has a timestamp lower than the given timestamp subtracted by the threshold.
    */
  def dequeue(timeStamp: BigInt): List[Traces.Event] = {
    queue.headOption.filter(_.timeRange.from <= timeStamp - threshold) match {
      case None => Nil
      case Some(_) =>
        val generator = queue.dequeue()
        val range = generator.timeRange
        if (range.to.isEmpty || range.to.get - range.from >= range.step){
          queue.enqueue(Traces.Event(generator.loc,
            TimeRange(range.id, range.from + range.step, range.to, range.step),
            generator.stream, generator.value))
        }
        generator +: dequeue(timeStamp)
    }
  }

  /**
    * Dequeue every event from the queue and apply the callback to it.
    */
  def processAll(callback: Traces.Event => Unit): Unit = {
    queue.headOption match {
      case None => Nil
      case Some(_) =>
        val generator = queue.dequeue()
        val range = generator.timeRange
        if (range.to.isEmpty || range.to.get - range.from >= range.step){
          queue.enqueue(Traces.Event(generator.loc,
            TimeRange(range.id, range.from + range.step, range.to, range.step),
            generator.stream, generator.value))
        }
        callback(generator)
        processAll(callback)
    }
  }

  /**
    * Print the whole queue without altering it.
    */
  override def toString: String = {
    queue.clone.dequeueAll.toString
  }
}

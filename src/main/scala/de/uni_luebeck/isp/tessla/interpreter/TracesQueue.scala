package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.Errors.DecreasingTimeStampsError

import scala.collection.mutable

class TracesQueue(val threshold: BigInt) {
  /**
    * A PriorityQueue, having a BigInt as timestamp and using the lowest value has highest priority.
    */
  val queue: mutable.PriorityQueue[Traces.Event] =
    new mutable.PriorityQueue[Traces.Event]()(Ordering.by(ev => ev.timeStamp)).reverse

  /**
    * Dequeues and processes all events with a time stamp lower than the one of the event subtracted by the threshold,
    * and then adds the new event to the queue.
    */
  def enqueue(event: Traces.Event, callback: Traces.Event => Unit): Unit = {
    dequeue(event.timeStamp).foreach(callback)
    // Note: queue.min actually looks for the highest timestamp (which is the lowest priority, therefore min)
    if (queue.nonEmpty && event.timeStamp <= queue.min(queue.ord).timeStamp - threshold){
      // New Input has a too small timestamp
      throw DecreasingTimeStampsError(queue.last.timeStamp, event.timeStamp, event.loc)
    }
    queue.enqueue(event)
  }

  /**
    * Dequeue every event which has a timestamp lower than the given timestamp subtracted by the threshold.
    */
  def dequeue(timeStamp: BigInt): List[Traces.Event] = {
    queue.headOption.filter(_.timeStamp <= timeStamp - threshold) match {
      case None => Nil
      case Some(_) =>
        queue.dequeue() +: dequeue(timeStamp)
    }
  }

  /**
    * Dequeue every event from the queue and apply the callback to it.
    */
  def processAll(callback: Traces.Event => Unit): Unit = {
    queue.dequeueAll.foreach(callback)
  }

  /**
    * Print the whole queue without altering it.
    */
  override def toString: String = {
    queue.clone.dequeueAll.toString
  }
}

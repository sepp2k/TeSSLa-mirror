package de.uni_luebeck.isp.tessla.interpreter

import scala.collection.mutable

class TracesQueue(val threshold: BigInt) {
  /*A PriorityQueue, having a BigInt as timestamp and using the lowest value has highest priority.*/
  val queue: mutable.PriorityQueue[Input.Event] =
    new mutable.PriorityQueue[Input.Event]()(Ordering.by(ev=>ev.timeStamp)).reverse

  def enqueue(event: Input.Event): Unit = {
    queue.enqueue(event)
  }

  def hasNext(timeStamp: BigInt): Boolean = {
    queue.nonEmpty && timeStamp >= queue.head.timeStamp + threshold
  }

  def hasDecreasingTimeStamp(timeStamp: BigInt): Boolean = {
    queue.nonEmpty && timeStamp < queue.head.timeStamp - threshold
  }

  def smallestTimeStamp : BigInt = {
    queue.headOption.map(_.timeStamp).getOrElse(-1)
  }

  /*Dequeues the element with the lowest timestamp, if it is lower than the current timestamp subtracted by the threshold.*/
  def dequeue(timeStamp: BigInt): Option[Input.Event] = {
    queue.headOption.filter(timeStamp >= _.timeStamp + threshold).map(_ => queue.dequeue())
  }

  /*Returns all elements of the queue in a list*/
  def toList: List[Input.Event] = {
    queue.dequeueAll.toList
  }

  override def toString: String = {
    queue.clone.dequeueAll.toString
  }
}

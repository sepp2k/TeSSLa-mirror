package de.uni_luebeck.isp.tessla.interpreter

import scala.collection.mutable
import scala.math.Ordering.BigIntOrdering

class TracesQueue(val threshold: BigInt = BigInt("100000")) {
  /*A PriorityQueue, having a BigInt as timestamp and using the lowest value has highest priority.*/
  val queue: mutable.PriorityQueue[(BigInt, (String, Interpreter.Value))] = new mutable.PriorityQueue[(BigInt, (String, Interpreter.Value))]()(Ordering.by(e=>e._1)).reverse

  def enqueue(timeStamp: BigInt, inStream: String, value: Interpreter.Value = Interpreter.UnitValue): Unit = {
    queue.enqueue((timeStamp, (inStream, value)))
  }


  def hasNext(timeStamp: BigInt): Boolean = {
    queue.nonEmpty && timeStamp >= queue.head._1 + threshold
  }

  /*Dequeues the element with the lowest timestamp, if it is lower than the current timestamp subtracted by the threshold.*/
  def dequeue(timeStamp: BigInt): Option[(BigInt, (String, Interpreter.Value))] = {
    queue.headOption match {
      case Some((t, _)) => if (timeStamp >= t + threshold) {
        Some(queue.dequeue())
      } else {
        None
      }
      case None => None
    }
  }

  /*Returns all elements of the queue in a list*/
  def toList(): List[(BigInt, (String, Interpreter.Value))] = {
    queue.dequeueAll.toList
  }

  override def toString(): String = {
    queue.clone.dequeueAll.toString
  }
}

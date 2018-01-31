package de.uni_luebeck.isp.tessla.builtins

private case class Element[D](time: BigInt, value: D)

class TimeQueue[D] (private[builtins] val list: List[Element[D]]) {
  def removeOlder(time: BigInt): TimeQueue[D] = {
    if (list.isEmpty) {
      this
    } else if (list.length == 1) {
      val first = list(0)
      if (first.time < time) {
        TimeQueue(time, first.value)
      } else {
        this
      }
    } else {
      val first = list(0)
      val second = list(1)
      if (second.time <= time) {
        TimeQueue(list.tail).removeOlder(time)
      } else {
        TimeQueue(first.time.max(time), first.value, list.tail)
      }
    }
  }

  def enqueue(time: BigInt, value: D): TimeQueue[D] = TimeQueue(list, time, value)

  def removeNewer(time: BigInt): TimeQueue[D] = {
    if (list.isEmpty) {
      this
    } else {
      val last = list(list.length - 1)
      if (last.time > time) {
        TimeQueue(list.dropRight(1)).removeNewer(time)
      } else {
        this
      }
    }
  }

  def fold[R](acc: R, until: BigInt)(f: (BigInt, BigInt, D, R) => R): R = list match {
    case Nil => acc
    case Element(t1, value)::Nil => f(t1, until, value, acc)
    case Element(t1, value)::Element(t2, _)::tail => TimeQueue(list.tail).fold(f(t1, t2, value, acc), until)(f)
  }

  override def toString = {
    if (list.nonEmpty) {
      fold(List.empty[String], list.last.time){(t1, t2, d, acc) =>
        if (t1 == t2) {
          acc :+ s"$t1 -> $d"
        } else {
          acc :+ s"[$t1, $t2) -> $d"
        }
      }.mkString(", ")
    } else "empty"
  }

  def dataTimeout = {
    if (list.size >= 2) {
      val second = list(1)
      Some(second.time)
    } else {
      None
    }
  }

  override def equals(obj: scala.Any) = obj match {
    case that: TimeQueue[D] => list == that.list
    case _ => false
  }
}

object TimeQueue {
  def apply[D]() = new TimeQueue[D](List())
  def empty[D] = new TimeQueue[D](List.empty)
  private[builtins] def apply[D](list: List[Element[D]]) = new TimeQueue[D](list)
  private[builtins] def apply[D](time: BigInt, value: D) = new TimeQueue[D](List(Element(time, value)))
  private[builtins] def apply[D](time: BigInt, value: D, list: List[Element[D]]) = new TimeQueue[D](List(Element(time, value)) ++ list)
  private[builtins] def apply[D](list: List[Element[D]], time: BigInt, value: D) = new TimeQueue[D](list ++ List(Element(time, value)))
}
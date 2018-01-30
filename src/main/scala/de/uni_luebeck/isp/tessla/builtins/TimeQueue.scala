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

  def fold[R](acc: R)(f: (BigInt, BigInt, D, R) => R): R = {
    if (list.length <= 1) {
      acc
    } else {
      val first = list(0)
      val second = list(1)
      TimeQueue(list.tail).fold(f(first.time, second.time, first.value, acc))(f)
    }
  }

  override def toString = {
    if (list.nonEmpty) {
      val last = list(list.length - 1)
      val s = fold(List.empty[String]){(t1, t2, d, acc) => acc :+ s"[$t1, $t2) -> $d"} :+
        s"${last.time} -> ${last.value}"
      s.mkString(", ")
    } else "empty"
  }

  def dataTimeout = {
    if (list.size >= 2) {
      Some(list(2).time)
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
  private def apply[D](list: List[Element[D]]) = new TimeQueue[D](list)
  private def apply[D](time: BigInt, value: D) = new TimeQueue[D](List(Element(time, value)))
  private def apply[D](time: BigInt, value: D, list: List[Element[D]]) = new TimeQueue[D](List(Element(time, value)) ++ list)
  private def apply[D](list: List[Element[D]], time: BigInt, value: D) = new TimeQueue[D](list ++ List(Element(time, value)))
}
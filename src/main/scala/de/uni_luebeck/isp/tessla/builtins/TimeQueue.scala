package de.uni_luebeck.isp.tessla.builtins


class BigIntInfinity(val value: Option[BigInt]) extends Ordered[BigIntInfinity]{
  override def compare(that: BigIntInfinity) = (value, that.value) match {
    case (None, None) => throw new RuntimeException("Torben hat gesagt: Das passiert eh nicht.")
    case (None, _) => 1
    case (_, None) => -1
    case (Some(x), Some(y)) => (x-y).signum
  }
  override def equals(other: scala.Any) = other match {
    case that: BigIntInfinity => that.value == value
    case _ => false
  }
  def min(that: BigIntInfinity) = if (this < that) this else that
  def isZero = value == Some(0)
  def isInfinity = value == None
  def limit(a: BigInt, b: BigInt) = {
    val aa = BigIntInfinity(a)
    val bb = BigIntInfinity(b)
    if (this < aa) aa else if (this > bb) bb else this
  }
  def *(that: BigIntInfinity) = (value, that.value) match {
    case (None, _) => new BigIntInfinity(None)
    case (_, None) => new BigIntInfinity(None)
    case (Some(x), Some(y)) => new BigIntInfinity(Some(x * y))
  }
  def +(that: BigIntInfinity) = (value, that.value) match {
    case (None, _) => new BigIntInfinity(None)
    case (_, None) => new BigIntInfinity(None)
    case (Some(x), Some(y)) => new BigIntInfinity(Some(x + y))
  }
  def max(that: BigIntInfinity) = (value, that.value) match {
    case (None, _) => new BigIntInfinity(None)
    case (_, None) => new BigIntInfinity(None)
    case (Some(x), Some(y)) => BigIntInfinity(x.max(y))
  }

  override def toString = value match {
    case None => "∞"
    case Some(x) => x.toString
  }
}

object BigIntInfinity {
  def apply(value: BigInt) = new BigIntInfinity(Some(value))
  def infinity = new BigIntInfinity(None)
  def zero = BigIntInfinity(0)
}

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
      BigIntInfinity(second.time)
    } else {
      BigIntInfinity.infinity
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
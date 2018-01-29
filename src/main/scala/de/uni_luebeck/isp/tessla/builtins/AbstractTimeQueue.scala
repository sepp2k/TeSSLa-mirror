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

case class BigInterval(left: BigIntInfinity, right: BigIntInfinity){
  def limit(a: BigInt, b: BigInt) = BigInterval(left.limit(a,b), right.limit(a,b))
  def *(that: BigInterval) = BigInterval(left * that.left, right * that.right)
  def +(that: BigInterval) = BigInterval(left + that.left, right + that.right)

  override def toString = s"[$left, $right]"
}

object BigInterval {
  def apply(left: BigInt, right: BigInt): BigInterval = BigInterval(BigIntInfinity(left), BigIntInfinity(right))
  def apply(left: BigIntInfinity, right: BigInt): BigInterval = BigInterval(left, BigIntInfinity(right))
  def apply(left: BigInt, right: BigIntInfinity): BigInterval = BigInterval(BigIntInfinity(left), right)
  def apply(single: BigInt): BigInterval = BigInterval(single, single)
  def top: BigInterval = BigInterval(BigInt(0), BigIntInfinity.infinity)
}

object AbstractTimeQueue {
  def empty = new AbstractTimeQueue(BigIntInfinity.zero, TimeQueue.empty)
  def top = new AbstractTimeQueue(BigIntInfinity.infinity, TimeQueue.empty)
}

class AbstractTimeQueue(private[builtins] val unknownBefore: BigIntInfinity, private[builtins] val queue: TimeQueue[BigInterval]) {
  def removeOlder(time: BigInt) = if (BigIntInfinity(time) < unknownBefore)
    new AbstractTimeQueue(unknownBefore, queue.removeOlder(time))
  else
    new AbstractTimeQueue(BigIntInfinity(0), queue.removeOlder(time))

  def enqueue(time: BigInt, value: BigInterval) = new AbstractTimeQueue(unknownBefore, queue.enqueue(time, value))

  def removeNewer(time: BigInt) = new AbstractTimeQueue(unknownBefore.min(BigIntInfinity(time)), queue.removeNewer(time))

  def fold(acc: BigInterval)(f: (BigInt, BigInt, BigInterval, BigInterval) => BigInterval): BigInterval =
    if (unknownBefore.isZero)
      queue.fold(acc)(f)
    else
      queue.fold(BigInterval.top)(f)

  override def toString() = (if (unknownBefore.isZero) "" else s"< $unknownBefore: T, ") + queue.toString
}

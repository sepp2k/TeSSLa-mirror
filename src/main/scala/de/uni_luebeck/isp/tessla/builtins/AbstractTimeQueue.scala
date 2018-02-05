package de.uni_luebeck.isp.tessla.builtins

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

  def enqueueFinite(time: BigInt, value: BigInterval, limit: Int): AbstractTimeQueue = {
    require(limit >= 2)
    if (queue.list.length < limit) {
      new AbstractTimeQueue(unknownBefore, queue.enqueue(time, value))
    } else {
      val first = queue.list(0)
      val second = queue.list(1)
      new AbstractTimeQueue(unknownBefore.max(BigIntInfinity(second.time)), TimeQueue(queue.list.tail)).enqueueFinite(time, value, limit)
    }
  }

  def removeNewer(time: BigInt) = new AbstractTimeQueue(unknownBefore.min(BigIntInfinity(time)), queue.removeNewer(time))

  def fold(acc: BigInterval, until: BigInt)(f: (BigInt, BigInt, BigInterval, BigInterval) => BigInterval): BigInterval =
    if (unknownBefore.isZero)
      queue.fold(acc, until)(f)
    else
      queue.fold(BigInterval.top, until)(f)

  override def toString() = (if (unknownBefore.isZero) "" else s"< $unknownBefore: T, ") + queue.toString

  override def equals(obj: scala.Any) = obj match {
    case that: AbstractTimeQueue => unknownBefore == that.unknownBefore && queue == that.queue
    case _ => false
  }

  def dataTimeout = if (unknownBefore.isZero) Some(queue.dataTimeout) else None
}

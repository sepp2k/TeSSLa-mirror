package de.uni_luebeck.isp.tessla.builtins

import org.scalatest.FunSuite

class TimeQueueTests extends FunSuite {
  test("removeOlder") {
    assert(TimeQueue.empty.removeOlder(36).list.isEmpty)
    val q = TimeQueue.empty.enqueue(4, 12).enqueue(16,17)
    assert(q.removeOlder(2).list == q.list)
    assert(q.removeOlder(14).list == List(Element(14,12), Element(16,17)))
    assert(q.removeOlder(18).list == List(Element(18,17)))
  }

  test("removeNewer") {
    assert(TimeQueue.empty.removeNewer(12).list.isEmpty)
    val q = TimeQueue.empty.enqueue(4, 12).enqueue(16,17)

    assert(q.removeNewer(18).list == q.list)
    assert(q.removeNewer(14).list == List(Element(4,12)))
    assert(q.removeNewer(2).list.isEmpty)
  }

  test("fold") {
    val q = TimeQueue.empty.enqueue(4, 4).enqueue(8,2).enqueue(10,987656940)
    assert(q.fold(0){(t1, t2, d, acc) => acc + (t2.intValue() - t1.intValue()) * d} == 20)
  }
}

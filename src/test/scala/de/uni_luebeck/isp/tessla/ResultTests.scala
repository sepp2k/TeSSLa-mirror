package de.uni_luebeck.isp.tessla

import org.scalatest.funsuite.AnyFunSuite
import TranslationPhase.{Result, Success, Failure, SimpleWarning}
import Errors.{DivideByZero, IndexOutOfRange}

class ResultTests extends AnyFunSuite {
  def w(msgs: String*) = msgs.toList.map(SimpleWarning(Location.unknown, _))
  val dbz = DivideByZero(Location.unknown)
  val ioor = IndexOutOfRange(42, IndexedSeq(), Location.unknown)

  test("combineAll") {
    val result1 = Result.combineAll(Seq(
      Success(1, w("a", "b")),
      Success(2, w()),
      Success(3, w("c"))
    ))
    assert(result1 == Success(Seq(1,2,3), w("a", "b", "c")))

    val result2 = Result.combineAll(Seq(
      Success(1, w("a")),
      Failure(Seq(dbz), w("b")),
      Success(3, w("c"))
    ))
    // Warnings from third result are also included - it does not stop at the first failure
    assert(result2 == Failure(Seq(dbz), w("a", "b", "c")))

    val result3 = Result.combineAll(Seq(
      Success(1, w("a")),
      Failure(Seq(dbz), w("b")),
      Failure(Seq(ioor), w("c"))
    ))
    // Warnings and errors from third result are also included - it does not stop at the first failure
    assert(result3 == Failure(Seq(dbz, ioor), w("a", "b", "c")))
  }

  def f(x: Int): Result[Int] = {
    if (x > 10) Failure(Seq(ioor), w("ioor"))
    else if (x == 0) Failure(Seq(dbz), w("dbz"))
    else Success(x, w(s"item $x"))
  }

  test("runSequentially") {
    val result1 = Result.runSequentially(Seq(1, 2, 3))(f)
    assert(result1 == Success(Seq(1,2,3), w("item 1", "item 2", "item 3")))

    val result2 = Result.runSequentially(Seq(1, 2, 0, 3))(f)
    // Warnings from f(3) are not included because it stops at f(0)
    assert(result2 == Failure(Seq(dbz), w("item 1", "item 2", "dbz")))

    val result3 = Result.runSequentially(Seq(1, 2, 11, 0, 3))(f)
    // Warnings and errors from f(0) or f(3) are not included because it stops at f(11)
    assert(result3 == Failure(Seq(ioor), w("item 1", "item 2", "ioor")))
  }
}

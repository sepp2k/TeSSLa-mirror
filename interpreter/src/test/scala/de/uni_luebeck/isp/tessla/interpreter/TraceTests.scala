package de.uni_luebeck.isp.tessla.interpreter

import org.scalatest.funsuite.AnyFunSuite

class TraceTests extends AnyFunSuite {

  test("testCsvTrace") {
    val csv =
      """ts, x, y
        | 1, 2, 3
        | 2, 5, 6
        | 5, 8, 9""".stripMargin
    val trace = Trace.fromCsvString(csv, "<str>", None).map(_.toString).toSet
    val result = Set(
      "1: x = 2",
      "1: y = 3",
      "2: x = 5",
      "2: y = 6",
      "5: x = 8",
      "5: y = 9"
    )
    assert(trace == result)
  }

  test("testCsvTraceWithEmpty") {
    val csv =
      """ts, x, y
        | 1, 2, 3
        | 2,  , 6
        | 5, 8   """.stripMargin
    val trace = Trace.fromCsvString(csv, "<str>", None).map(_.toString).toSet
    val result = Set(
      "1: x = 2",
      "1: y = 3",
      "2: y = 6",
      "5: x = 8"
    )
    assert(trace == result)
  }

  test("testTrace") {
    val input =
      """
        |1: x = 2
        |1: y = 3
        |2: x = 5
        |
        |
        |2: y = 6
        |5: x = 8
        |5: y = 9""".stripMargin
    val trace = Trace.fromString(input, "<str>", None).map(_.toString).toSet
    val result = Set(
      "1: x = 2",
      "1: y = 3",
      "2: x = 5",
      "2: y = 6",
      "5: x = 8",
      "5: y = 9"
    )
    assert(trace == result)
  }

}

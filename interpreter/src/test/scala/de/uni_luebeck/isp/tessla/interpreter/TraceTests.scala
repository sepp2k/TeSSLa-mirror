/*
 * Copyright 2020 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

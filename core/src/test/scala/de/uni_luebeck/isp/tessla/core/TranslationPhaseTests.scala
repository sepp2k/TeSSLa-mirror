/*
 * Copyright 2022 The TeSSLa Community
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

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Errors.TesslaError
import de.uni_luebeck.isp.tessla.core.TranslationPhase._
import org.scalatest.funsuite.AnyFunSuite

class TranslationPhaseTests extends AnyFunSuite {

  case class TestWarning(message: String, loc: Location = Location.unknown) extends Diagnostic

  case class TestError(message: String, loc: Location = Location.unknown) extends TesslaError

  val w1 = TestWarning("Warn1")
  val w2 = TestWarning("Warn2")
  val e1 = TestError("Err1")
  val e2 = TestError("Err2")

  val phase: TranslationPhase[Int, String] = new TestTranslator(_).translate()

  class TestTranslator(value: Int) extends Translator[String] {
    override protected def translateSpec(): String = value match {
      case 1 => "Foo"
      case 2 =>
        warn(w1)
        "Bar"
      case _ =>
        warn(w2)
        throw e1
    }
  }

  test("testPhaseApplication") {
    assert(phase.translate(1) == Success("Foo", Nil))
    assert(phase.translate(2) == Success("Bar", List(w1)))
    assert(phase.translate(-5) == Failure(List(e1), List(w2)))
  }

  test("testIdentityPhase") {
    val idI = TranslationPhase.IdentityPhase[Int]()
    val idS = TranslationPhase.IdentityPhase[String]()
    assert(idI.translate(1).andThen(phase).andThen(idS) == Success("Foo", Nil))
    assert(idI.andThen(phase).andThen(idS)(1) == Success("Foo", Nil))
  }

  test("testEnableIfPhase") {
    val t: TranslationPhase[Int, Int] = (a: Int) => Success(a * 2, Nil)
    val apply = TranslationPhase.EnableIf(cond = true, t)
    val skip = TranslationPhase.EnableIf(cond = false, t)
    assert(apply(5) == Success(10, Nil))
    assert(skip(5) == Success(5, Nil))
  }

  test("testBypassPhase") {
    val bypass = TranslationPhase.BypassPhase(phase)
    assert(bypass(2) == Success((2, "Bar"), List(w1)))
    assert(bypass(-1) == Failure(List(e1), List(w2)))
  }

  test("testParallelPhase") {
    val t: TranslationPhase[Int, Int] = (a: Int) => Success(a * 2, Nil)
    val parallel = TranslationPhase.ParallelPhase(t, phase)
    assert(parallel(1) == Success((2, "Foo"), Nil))
    assert(parallel(-1) == Failure(List(e1), List(w2)))
  }
}

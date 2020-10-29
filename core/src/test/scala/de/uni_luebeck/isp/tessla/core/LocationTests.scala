package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Location.SourceRange
import org.scalatest.funsuite.AnyFunSuite

class LocationTests extends AnyFunSuite {

  test("testMerge") {
    val loc1 = Location(0, 0, 5, 5, "foo")
    val loc2 = Location(8, 8, 10, 10, "foo")
    assert((loc1 merge loc2) == Location(0, 0, 10, 10, "foo"))
  }

  test("testMergeWithUnknown") {
    val loc1 = Location(0, 0, 5, 5, "foo")
    val loc2 = Location.unknown
    assert((loc1 merge loc2) == loc1)
  }

  test("testMergeDifferentPaths") {
    val loc1 = Location(0, 0, 5, 5, "foo")
    val loc2 = Location(8, 8, 10, 10, "bar")
    assertThrows[IllegalArgumentException](loc1 merge loc2)
  }

  test("testMergeWithInvalid") {
    val loc1 = Location(0, 0, 5, 5, "foo")
    val loc2 = Location.builtIn
    assertThrows[IllegalArgumentException](loc1 merge loc2)
  }

  test("testForWholeFile") {
    val loc1 = Location.forWholeFile(
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
        |Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
        |Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
        |Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
        |""".stripMargin,
      "lipsum"
    )
    assert(loc1 == Location(0, 0, 3, 111, "lipsum"))
  }

  test("testBuiltIn") {
    val loc = Location.builtIn
    assert(loc.toString == "<built-in>")
  }

  test("testOpt") {
    val loc = Location.option("foo")
    assert(loc.toString == "option 'foo'")
  }

}

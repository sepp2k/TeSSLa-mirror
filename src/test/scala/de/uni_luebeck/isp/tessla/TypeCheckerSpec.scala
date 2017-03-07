package de.uni_luebeck.isp.tessla

import org.scalatest.{Matchers, FlatSpec, Inside}
import Inside._

class TypeCheckerSpec extends FlatSpec with Matchers {
  "Type unification" should "not unify distinct conrete types" in {

    val pairs = Seq(
      (SimpleType("foo"), SimpleType("bar")),
      (GenericType("foo", Seq(SimpleType("bar"))), SimpleType("foo")),
      (GenericType("foo", Seq(SimpleType("bar"))),
        GenericType("foo", Seq(SimpleType("baz")))),
      (GenericType("foo", Seq(SimpleType("bar"))),
        GenericType("baz", Seq(SimpleType("bar")))),
      (GenericType("foo", Seq(SimpleType("bar"), SimpleType("bar"))),
        GenericType("foo", Seq(SimpleType("bar"), SimpleType("baz"))))
    )

    for ((a, b) <- pairs) {
      val env = Env()
      env.unify(a, b) should be (None)
    }
  }


  it should "unify identical conrete types" in {
    val types = Seq(
      SimpleType("foo"),
      GenericType("foo", Seq(SimpleType("bar"))),
      GenericType("foo", Seq(SimpleType("bar"), SimpleType("baz")))
    )

    for (t <- types) {
      val env = Env()
      env.unify(t, t) should be (Some(env))
    }
  }

  it should "unify identical quantified types" in {
    val a = new TypeVar
    val b = new TypeVar

    val types = Seq(
      a,
      GenericType("foo", Seq(a)),
      GenericType("foo", Seq(a, SimpleType("baz"))),
      GenericType("foo", Seq(a, b))
    )

    for (t <- types) {
      val env = Env()
      env.unify(t, t) should be (Some(env))
    }
  }

  it should "unify structurally equivalent quantified types" in {
    val a = new TypeVar
    val b = new TypeVar

    val types = Seq(
      a,
      GenericType("foo", Seq(a)),
      GenericType("foo", Seq(a, SimpleType("baz"))),
      GenericType("foo", Seq(a, b))
    )

    for (t <- types) {
      var env = Env()
      val t2 = t.deepCopy
      val envRes = env.unify(t, t2)
      envRes should not be None
      env = envRes.get
      t.substitute(env) should be (t2.substitute(env))
    }
  }

  // TODO Better test coverage
}

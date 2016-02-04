package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.DefExtractor._
import org.scalatest.{Matchers, FlatSpec, Inside}
import Inside._

import scala.util.Success

class DefExtractorSpec extends FlatSpec with Matchers {

  object Big {
    def unapply(n:BigInt) = if (BigInt(n.toLong) == n) Some(n.toLong) else None
  }


  def extract(string: String) = {
    val compiler = new Compiler
    val parsed = Parser(compiler, TesslaSource.fromString(string)).get
    (DefExtractor(compiler, parsed).get, compiler.diagnostics)
  }

  "The definition extractor" should "extract named constants" in {
    inside(extract("define foo := bar")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        macroDefs shouldBe empty
        streamDefs should have size 1
        streamDefs should contain key "foo"
        inside(streamDefs("foo")) {
          case StreamDef("foo", _, ExprTree(NamedFn("bar", _), args, _)) =>
            args shouldBe empty
        }
    }
  }

  it should "extract integer literals" in {
    inside(extract("define foo := 123")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        macroDefs shouldBe empty
        streamDefs should have size 1
        streamDefs should contain key "foo"
        inside(streamDefs("foo")) {
          case StreamDef("foo", _, ExprTree(LiteralFn(IntLiteral(Big(123)), _), args, _)) =>
            args shouldBe empty
        }
    }
  }

  it should "extract string literals" in {
    inside(extract("define foo := \"bar\"")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        macroDefs shouldBe empty
        streamDefs should have size 1
        streamDefs should contain key "foo"
        inside(streamDefs("foo")) {
          case StreamDef("foo", _, ExprTree(LiteralFn(StringLiteral("bar"), _), args, _)) =>
            args shouldBe empty
        }
    }
  }

  it should "extract functions" in {
    inside(extract("define foo := bar(baz, qux)")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        macroDefs shouldBe empty
        streamDefs should have size 1
        streamDefs should contain key "foo"
        inside(streamDefs("foo")) {
          case StreamDef("foo", _, ExprTree(NamedFn("bar", _), args, _)) =>
            args should have size 2
            args should contain key Pos(0)
            args should contain key Pos(1)
            inside(args(Pos(0))) {
              case ExprTree(NamedFn("baz", _), fnArgs, _) =>
                fnArgs shouldBe empty
            }
            inside(args(Pos(1))) {
              case ExprTree(NamedFn("qux", _), fnArgs, _) =>
                fnArgs shouldBe empty
            }
        }
    }
  }

  it should "extract functions with named arguments" in {
    inside(extract("define foo := bar(baz, qux := \"thud\")")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        macroDefs shouldBe empty
        streamDefs should have size 1
        streamDefs should contain key "foo"
        inside(streamDefs("foo")) {
          case StreamDef("foo", _, ExprTree(NamedFn("bar", _), args, _)) =>
            args should have size 2
            args should contain key Pos(0)
            args should contain key Named("qux")
            inside(args(Pos(0))) {
              case ExprTree(NamedFn("baz", _), fnArgs, _) =>
                fnArgs shouldBe empty
            }
            inside(args(Named("qux"))) {
              case ExprTree(LiteralFn(StringLiteral("thud"), _), fnArgs, _) =>
                fnArgs shouldBe empty
            }
        }
    }
  }

  it should "extract type ascriptions" in {
    inside(extract("define foo := bar : Int")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        macroDefs shouldBe empty
        streamDefs should have size 1
        streamDefs should contain key "foo"
        inside(streamDefs("foo")) {
          case StreamDef("foo", _, ExprTree(TypeAscrFn(SimpleType("Int"), _), args, _)) =>
            args should have size 1
            args should contain key Pos(0)
            inside(args(Pos(0))) {
              case  ExprTree(NamedFn("bar", _), fnArgs, _) =>
                fnArgs shouldBe empty
            }
        }

    }
  }

  it should "extract LHS type ascriptions" in {
    inside(extract("define foo: Int := bar")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        macroDefs shouldBe empty
        streamDefs should have size 1
        streamDefs should contain key "foo"
        inside(streamDefs("foo")) {
          case StreamDef("foo", _, ExprTree(TypeAscrFn(SimpleType("Int"), _), args, _)) =>
            args should have size 1
            args should contain key Pos(0)
            inside(args(Pos(0))) {
              case  ExprTree(NamedFn("bar", _), fnArgs, _) =>
                fnArgs shouldBe empty
            }
        }

    }
  }

  it should "extract macros" in {
    inside(extract("define foo(bar) := bar")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        streamDefs shouldBe empty
        macroDefs should have size 1
        macroDefs should contain key "foo"
        inside(macroDefs("foo")) {
          case MacroDef(Seq(("bar", _)),
              StreamDef("foo", _, ExprTree(NamedFn("bar", _), args, _))) =>
            args shouldBe empty
        }
    }
  }

  it should "extract macro argument type ascriptions" in {
    inside(extract("define foo(bar: Int) := bar")) {
      case (Definitions(streamDefs, macroDefs), Seq()) =>
        streamDefs shouldBe empty
        macroDefs should have size 1
        macroDefs should contain key "foo"
        inside(macroDefs("foo")) {
          case MacroDef(Seq(("bar", _)),
              StreamDef("foo", _, ExprTree(TypeAscrFn(SimpleType("Int"), _), args, _))) =>
            args should have size 1
            args should contain key Pos(0)
            inside(args(Pos(0))) {
              case  ExprTree(NamedFn("bar", _), fnArgs, _) =>
                fnArgs shouldBe empty
            }
        }
    }
  }

  "The definition extractor error handling" should "diagnose duplicate definitions (stream/stream)" in {
    extract(
      "define foo := bar\n" +
      "define foo := baz\n"
    ) should matchPattern {
      case (_, Seq(RedefinitionError("foo", _, _))) =>
    }
  }

  it should "diagnose duplicate definitions (stream/macro)" in {
    extract(
      "define foo := bar\n" +
      "define foo(baz) := baz\n"
    ) should matchPattern {
      case (_, Seq(RedefinitionError("foo", _, _))) =>
    }
  }

  it should "diagnose duplicate definitions (macro/macro)" in {
    extract(
      "define foo(bar) := bar\n" +
      "define foo(baz) := baz\n"
    ) should matchPattern {
      case (_, Seq(RedefinitionError("foo", _, _))) =>
    }
  }

  it should "diagnose duplicate macro argument names" in {
    extract("define foo(bar, baz, bar) := bar") should matchPattern {
      case (_, Seq(DuplicateArgumentNameError(("foo", _), "bar", _, _))) =>
    }
  }

  it should "diagnose macro arguments used as functions" in {
    extract("define foo(bar) := bar(123)") should matchPattern {
      case (_, Seq(MacroArgumentUsedAsFunctionError(("foo", _), "bar", _, _))) =>
    }
  }

  it should "diagnose positional after named arguments" in {
    extract("define foo := bar(baz := qux, thud)") should matchPattern {
      case (_, Seq(PositionalFollowsNamedArgumentError(("bar", _), ("baz", _), _, _))) =>
    }
  }

  it should "diagnose duplicated named arguments" in {
    extract("define foo := bar(baz := qux, baz := thud)") should matchPattern {
      case (_, Seq(DuplicateNamedArgumentError(("bar", _), "baz", _, _))) =>
    }
  }
}

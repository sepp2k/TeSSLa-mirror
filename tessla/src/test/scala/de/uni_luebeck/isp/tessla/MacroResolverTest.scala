package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.MacroResolver._
import org.scalactic.Equality
import org.scalatest.Inside._
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util.Success

class MacroResolverTest extends FlatSpec with Matchers {

  def resolve(string: String, debug: Boolean = false) = {
    val compiler = new Compiler(silent = true, debug = debug)
    val parsed = Parser(compiler, TesslaSource.fromString(string)).get
    val extracted = DefExtractor(compiler, parsed).get
    val resolved = MacroResolver(compiler, extracted)
    //print("resolve: compiler.diagnostics = ")
    //println(compiler.diagnostics)
    (resolved, compiler.diagnostics)
  }

  def parse(string: String) = {
    val compiler = new Compiler(silent = true)
    val parsed = Parser(compiler, TesslaSource.fromString(string)).get
    val extracted = DefExtractor(compiler, parsed).get
    (extracted, compiler.diagnostics)
  }

  implicit object exprTreeFnEquality extends Equality[ExprTreeFn] {
    override def areEqual(a: ExprTreeFn, b: Any): Boolean = {
      b.isInstanceOf[ExprTreeFn] && (
        (a, b.asInstanceOf[ExprTreeFn]) match {
          case (NamedFn(n1, loc1), NamedFn(n2, loc2)) => n1 == n2
          case (TypeAscrFn(t1: Type, loc1: NestedLoc), TypeAscrFn(t2: Type, loc2: NestedLoc)) => t1 eq t2
          case (LiteralFn(value1: LiteralValue, loc1: NestedLoc), LiteralFn(value2: LiteralValue, loc2: NestedLoc)) => value1 == value2
          case _ => false
        })
    }
  }

  implicit object streamDefEquality extends Equality[StreamDef] {
    override def areEqual(a: StreamDef, b: Any): Boolean =
      b.isInstanceOf[StreamDef] && exprTreeEquality.areEqual(b.asInstanceOf[StreamDef].expr, a.expr)
  }

  implicit object exprTreeEquality extends Equality[ExprTree] {
    override def areEqual(a: ExprTree, b: Any): Boolean = b.isInstanceOf[ExprTree] && {
      val bT = b.asInstanceOf[ExprTree]
      exprTreeFnEquality.areEqual(a.fn, bT.fn) && {
        (a.args.toSeq.length == bT.args.toSeq.length) &&
          a.args.toSeq.zip(bT.args.toSeq).foldLeft(true) { case (z, ((an1, exprTr1), (an2, exprTr2))) => z && (an1 == an2) && areEqual(exprTr1, exprTr2) }
      }
    }
  }

  implicit object definitionsEquality extends Equality[Definitions] {
    override def areEqual(a: Definitions, that: Any): Boolean = that.isInstanceOf[Definitions] && {
      val b = that.asInstanceOf[Definitions]
      streamDefsEquality.areEqual(a.streamDefs, b.streamDefs) &&
        macroDefsEquality.areEqual(a.macroDefs, b.macroDefs)
    }
  }

  implicit object streamDefsEquality extends Equality[Map[String, StreamDef]] {
    override def areEqual(a: Map[String, StreamDef], that: Any): Boolean = that match {
      case b: Map[String, StreamDef]@unchecked =>
        // Non-variable type argument String is unchecked. However, if the keySets coincide it should be all right anyway.
        (a.keySet == b.keySet) &&
          a.foldLeft(true) {
            case (z, (s, streamDef)) => z && streamDefEquality.areEqual(b(s), streamDef)
          }
      case _ => false
    }
  }

  implicit object macroDefsEquality extends Equality[Map[String, MacroDef]] {
    override def areEqual(a: Map[String, MacroDef], that: Any): Boolean = that match {
      case b: Map[String, MacroDef]@unchecked => {
        // Non-variable type argument String is unchecked. However, if the keySets coincide it should be all right anyway.
        (a.keySet == b.keySet) &&
          a.foldLeft(true) { case (z, (s, macroDef)) => z && macroDefEquality.areEqual(b(s), macroDef) }
      }
      case _ => false
    }
  }

  implicit object macroDefEquality extends Equality[MacroDef] {
    override def areEqual(a: MacroDef, that: Any): Boolean = that.isInstanceOf[MacroDef] && {
      val b = that.asInstanceOf[MacroDef]
      (a.args.map(_._1) == b.args.map(_._1)) && streamDefEquality.areEqual(a.streamDef, b.streamDef)
    }
  }


  "The macro resolution" should "ignore unrelated calls" in {

    val specification =
      """
  define foo := bar
  define bar := foo(arg1, arg2)
      """

    inside(resolve(specification)) {
      case (Success(Definitions(streamDefs, macroDefs)), Seq()) => {
        macroDefs shouldBe empty
        streamDefs should have size 2
        streamDefs should contain key "foo"
        streamDefs should contain key "bar"
        inside(streamDefs("foo")) {
          case StreamDef("foo", _, ExprTree(NamedFn("bar", _), args, _)) =>
            args shouldBe empty
        }
        inside(streamDefs("bar")) {
          case StreamDef("bar", _, ExprTree(NamedFn("foo", _), args, _)) =>
            args should have size 2
            args should contain key Pos(0)
            args should contain key Pos(1)
            inside(args(Pos(0))) {
              case ExprTree(NamedFn("arg1", _), args, _) =>
                args shouldBe empty
            }
            inside(args(Pos(1))) {
              case ExprTree(NamedFn("arg2", _), args, _) =>
                args shouldBe empty
            }
        }
      }
    }
  }

  it should "correctly substitute arguments" in {

    val specification =
      """
        define myMacro(arg1, arg2) := foo(bar(arg1),arg2)
        define mac2(arg1) := foo(arg1,arg2)
        define s := foo(myMacro(1,2),arg2)
      """
    val expected = parse("""define s := foo(foo(bar(1), 2), arg2)""")._1.streamDefs

    inside(resolve(specification)) {
      case (Success(Definitions(streamDefs, macroDefs)), Seq()) => {
        //streamDefs shouldEqual parse("""define s := foo(foo(bar(1), 2), arg2)""")._1.streamDefs
        streamDefs.keySet shouldEqual expected.keySet
        streamDefs.foreach { case (s, streamDef) => expected(s) shouldEqual streamDef }
      }
    }
  }

  it should "correctly substitute macros within a macro definition" in {
    val testSpec2: String =
      """
        define myMacro(arg1, arg2) := foo(bar(arg1),arg2)
        define mac2(arg1) := foo(arg1,myMacro(arg2, arg2))
        define s := foo(myMacro(1,2),arg2)
        define k := mac2(1)
      """
    val expected: Definitions = parse(
      """
        -- define myMacro(arg1, arg2) := foo(bar(arg1),arg2)
        -- define mac2(arg1) := foo(arg1,foo(bar(arg2),arg2))
        define s := foo(foo(bar(1),2),arg2)
        define k := foo(1,foo(bar(arg2),arg2))
      """)._1

    inside(resolve(testSpec2)) {
      case (Success(Definitions(streamDefs, macroDefs)), Seq()) => {
        streamDefs shouldEqual expected.streamDefs
        macroDefs shouldEqual expected.macroDefs
      }
    }
  }

  it should "substitute macros within macro definitions recursively" in {

    val testSpec3: String =
      """
        define myMacro(arg1, arg2) := foo(bar(arg1),arg2)
        define mac2(arg1) := foo(arg1,myMacro(bar1, arg2))
        define mac3(arg1, arg2) := foo(mac2(arg2), myMacro(b, c))
        define s := myMacro(1,2)
        define s2 := mac2(1)
        define s3 := mac3(1,2)
      """

    val expected: Definitions = parse(
      """
        -- define myMacro(arg1, arg2) := foo(bar(arg1),arg2)
        -- define mac2(arg1) := foo(arg1,foo(bar(bar1),arg2))
        -- define mac3(arg1, arg2) := foo(foo(arg2,foo(bar(bar1),arg2)), foo(bar(b),c))
        define s := foo(bar(1),2)
        define s2 := foo(1,foo(bar(bar1),arg2))
        define s3 := foo(foo(2,foo(bar(bar1),2)), foo(bar(b),c))
      """)._1
    inside(resolve(testSpec3)) {
      case (Success(Definitions(streamDefs, macroDefs)), Seq()) => {
        streamDefs shouldEqual expected.streamDefs
        macroDefs shouldBe empty
      }
    }
  }

  it should "be invariant under definition reordering" in {
    val testSpec5: String =
      """
        define mac4(arg1) := mac3(abc)
        define mac3(arg1) := mac2(xyz)
        define mac2(arg1) := ar1
        define mac1(a, b) := bar(mac4(a),mac3(b), mac2(a), b)
        define s1 := mac1(1,2)
      """
    val testSpec5var: String =
      """
        define mac4(arg1) := mac3(abc)
        define s1 := mac1(1,2)
        define mac1(a, b) := bar(mac4(a),mac3(b), mac2(a), b)
        define mac2(arg1) := ar1
        define mac3(arg1) := mac2(xyz)
      """

    inside((resolve(testSpec5), resolve(testSpec5var))) {
      case ((Success(Definitions(streamDefs, macroDefs)), Seq()), (Success(Definitions(streamDefs2, macroDefs2)), Seq())) => {
        streamDefs shouldEqual streamDefs2
      }
    }
  }

  it should "flatten each macro only once" in {

    val testSpec7: String =
      """
        define mac1(arg1, arg2) := a(arg1,b(arg2,bar))
        define mac2(arg1) := c(arg1,mac1(arg1, c2))
        define mac3(arg1) := e(mac2(d1),mac1(d2,d3))
        define mac4(arg1) := e(mac2(d1),mac1(d2,d3))
        define s1 := mac1(1,2)
        define s2 := mac2(1)
        define s3 := mac3(1)
        define s4 := mac4(1)
      """

    val expected: Definitions = parse(
      """
          -- define mac1(arg1, arg2) := a(arg1,b(arg2,bar))
          -- define mac2(arg1) := c(arg1,a(arg1,b(c2,bar)))
          -- define mac3(arg1) := e(c(d1,a(d1,b(c2,bar))),a(d2,b(d3,bar)))
          -- define mac4(arg1) := e(c(d1,a(d1,b(c2,bar))),a(d2,b(d3,bar)))
          define s1 := a(1,b(2,bar))
          define s2 := c(1,a(1,b(c2,bar)))
          define s3 := e(c(d1,a(d1,b(c2,bar))),a(d2,b(d3,bar)))
          define s4 := e(c(d1,a(d1,b(c2,bar))),a(d2,b(d3,bar)))
      """)._1

    inside(resolve(testSpec7)) {
      case (Success(Definitions(streamDefs, macroDefs)), Seq()) => {
        streamDefs shouldEqual expected.streamDefs
        macroDefs shouldBe empty
      }
    }
  }

  it should "resolve nested macro calls" in {
    val testSpec8 =
      """
      -- macros should be resolved recursively,
      -- in macro definitions as well as in stream definitions
      define mac(arg1) := f(arg1)
      define mac2(x) := mac(mac(zzz))
      define b := mac(mac(zzz))
      define a := mac2(b)
      define c := mac2(mac2(x))
      """
    val expected: Definitions = parse(
      """
      -- define mac(arg1) := f(arg1)
      -- define mac2(x) := f(f(zzz))
      define b := f(f(zzz))
      define a := f(f(zzz))
      define c := f(f(zzz))
      """)._1

    inside(resolve(testSpec8)) {
      case (Success(Definitions(streamDefs, macroDefs)), Seq()) => {
        streamDefs shouldEqual expected.streamDefs
        macroDefs shouldBe empty
      }
    }
  }

  "The macro resolution error handling" should "detect cyclic macro definitions" in {
    val testSpec9 =
      """
       -- cyclic definitions
        define mac3(arg1) := mac2(z)
        define mac2(arg1) := mac1(arg1)
        define mac1(arg1) := mac3(x)
        define s := mac1(bla)
      """
    inside(resolve(testSpec9)) {
      case (Success(Definitions(streamDefs, macroDefs)), diagnostics) => {
        streamDefs shouldBe empty
        macroDefs shouldBe empty
        diagnostics should matchPattern {
          case Seq(CyclicMacroDefinitionError(MacroDef(_, StreamDef("mac2", _, _)), _)) =>
        }
      }
    }
  }

  it should "detect multiple cyclic macro definitions" in {
    val testSpec10 =
      """
       -- multiple cyclic definitions
        define mac1(arg1) := f(mac2(arg1))
        define mac2(arg1) := mac1(arg1)

        define mac3(arg1) := baz(mac4(x,arg1), mac5(arg1,y))
        define mac4(a1,a2) := bar(a1, mac3(a2))
        define mac5(a1,a2) := foo(mac3(a1), a2)

        define s1 := mac1(bla)
        define s2 := mac3(blub)
      """
    inside(resolve(testSpec10)) {
      case (_, diagnostics) => {
        exactly(1, diagnostics) should matchPattern {
          case CyclicMacroDefinitionError(MacroDef(_, StreamDef("mac2", _, _)), _) =>
        }
        exactly(1, diagnostics) should matchPattern {
          case CyclicMacroDefinitionError(MacroDef(_, StreamDef("mac4", _, _)), _) =>
        }
        exactly(1, diagnostics) should matchPattern {
          case CyclicMacroDefinitionError(MacroDef(_, StreamDef("mac5", _, _)), _) =>
        }
      }
    }
  }
}
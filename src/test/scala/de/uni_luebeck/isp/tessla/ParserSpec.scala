package de.uni_luebeck.isp.tessla

import org.scalatest.{Matchers, FlatSpec}

import scala.util.{Try, Success}

class ParserSpec extends FlatSpec with Matchers {
  import Ast._
  def parse(string: String): Try[Ast.Spec] = Parser(new Compiler, TesslaSource.fromString(string))

  "A tessla parser" should "parse a simple definition" in {
    parse("define foo := bar") should matchPattern {
      case Success(Spec(List(Def(Identifier("foo",_),Seq(),None,
        ExprName(Identifier("bar",_)),_)))) =>
    }
  }

  it should "parse a definition including an application" in {
    parse("define foo := bar(baz)") should matchPattern {
      case Success(Spec(List(Def(Identifier("foo",_),Seq(),None,
        ExprApp(Identifier("bar",_),List(PosArg(ExprName(Identifier("baz",_)))),_),_)))) =>
    }
  }

  it should "parse a definition including an application with named arguments" in {
    parse("define foo := bar(arg := baz)") should matchPattern {
      case Success(Spec(List(Def(Identifier("foo",_),Seq(),None,
        ExprApp(Identifier("bar", _),List(NamedArg(Identifier("arg", _),ExprName(Identifier("baz", _)))),_),_)))) =>
    }
  }

  it should "parse a a definition including a type ascription" in {
    parse("define foo := bar: Int") should matchPattern {
      case Success(Spec(List(Def(Identifier("foo",_),Seq(),None,
        ExprTypeAscr(ExprName(Identifier("bar",_)),TypeName(Identifier("Int",_))),_)))) =>
    }
  }

  it should "parse a definition with a LHS type ascription" in {
    parse("define foo: Int := bar") should matchPattern {
      case Success(Spec(List(Def(Identifier("foo",_),Seq(),Some(TypeName(Identifier("Int",_))),
        ExprName(Identifier("bar",_)),_)))) =>
    }
  }

  // TODO more parser tests
}

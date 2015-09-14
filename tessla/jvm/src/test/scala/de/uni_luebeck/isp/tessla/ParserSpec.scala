package de.uni_luebeck.isp.tessla

import org.scalatest._
import de.uni_luebeck.isp.tessla.Parser._
import de.uni_luebeck.isp.tessla.AST._
import scala.io.Source

class ParserSpec extends FlatSpec with Matchers {
  "A tessla parser" should "parse a simple definition" in {
    val result = parseAll(spec(), Source.fromString("define foo := bar"))
    result should matchPattern {
      case Success(_, Spec(List(Def("foo", TreeTerm(UnresolvedTerm("bar", ToBeInferred))))), _, _) =>}
  }
  
  it should "parse a definition including an application" in {
    val result = parseAll(spec(), Source.fromString("define foo := bar(baz)"))
    result should matchPattern {
      case Success(_, Spec(List(
          Def("foo", TreeTerm(App(UnresolvedFunction("bar"),
              List(TreeTerm(UnresolvedTerm("baz", ToBeInferred))), List(), ToBeInferred))))), _, _) =>}
  }
  
  it should "parse a definition including an application with named arguments" in {
    val result = parseAll(spec(), Source.fromString("define foo := bar(arg := baz)"))
    result should matchPattern {
      case Success(_, Spec(List(
          Def("foo", TreeTerm(App(UnresolvedFunction("bar"),
              List(), List(Def("arg", TreeTerm(UnresolvedTerm("baz", ToBeInferred)))), ToBeInferred))))), _, _) =>}
  }
  
  it should "parse a definition including a type ascription" in {
    val result = parseAll(spec(), Source.fromString("define foo := bar : int"))
    result should matchPattern {
      case Success(_, Spec(List(
          Def("foo", TreeTerm(TypeAscr(TreeTerm(UnresolvedTerm("bar", ToBeInferred)), UnresolvedPrimitiveType("int")))))), _, _) =>}
  }
  
  it should "parse a definition with a LHS type ascription" in {
    val result = parseAll(spec(), Source.fromString("define foo : int := bar"))
    result should matchPattern {
      case Success(_, Spec(List(
          Def("foo", TreeTerm(TypeAscr(TreeTerm(UnresolvedTerm("bar", ToBeInferred)), UnresolvedPrimitiveType("int")))))), _, _) =>}
  }
  
  // TODO more parser tests
}

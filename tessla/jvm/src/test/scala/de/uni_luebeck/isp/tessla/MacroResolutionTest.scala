package de.uni_luebeck.isp.tessla

import org.scalatest._
import de.uni_luebeck.isp.tessla.Parser._
import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.MacroResolution._
import scala.io.Source

/**
 * @author Normann Decker <decker@isp.uni-luebeck.de>
 */
class MacroResolutionTest extends FlatSpec with Matchers {

  /**
   * Helper methods capturing setup, to make test cases more readable.
   */
  def result(specification: String) = {
    val parseResult = parseAll(spec(), Source.fromString(specification))
    parseResult match {
      case Success(_, spec, _, _) => resolveMacros(spec)
      case _                      => ???
    }
  }
  def parseOnly(specification: String) = 
    parseAll(spec(), Source.fromString(specification)) match {
    case Success(_, spec, _, _) => spec
    case _                      => ???
  }

  /**
   * Test cases go here.
   */

  "The macro resolution" should "ignore unrelated calls" in {

    val specification = """
      define foo := bar
      define bar := foo(arg1, arg2)
    """

    result(specification) should matchPattern {
      case Right(Spec(List(
        Def("foo", TreeTerm(UnresolvedTerm("bar", ToBeInferred))),
        Def("bar", TreeTerm(App(UnresolvedFunction(foo),
          List(TreeTerm(UnresolvedTerm(arg1, ToBeInferred)), TreeTerm(UnresolvedTerm("arg2", ToBeInferred))),
          List(),
          ToBeInferred)))
        ))) =>
    }
  }

  it should "correctly substitute arguments" in {

    val specification = """
        define myMacro(arg1, arg2) := foo(bar(arg1),arg2)
        define mac2(arg1) := foo(arg1,arg2)
        define s := foo(myMacro(1,2),arg2)
    """

    result(specification) should matchPattern {
      case Right(ast) if ast equals parseOnly ("""define s := foo(foo(bar(1), 2), arg2)""") =>
    }
  }

  //TODO: create more test cases based on the following specifications

  val testSpec2: String = """
        define myMacro(arg1, arg2) := foo(bar(arg1),arg2)
        define mac2(arg1) := foo(arg1,myMacro(arg2, arg2))
        define s := foo(myMacro(1,2),arg2)
     """

  val testSpec3: String = """
        define myMacro(arg1, arg2) := foo(bar(arg1),arg2)
        define mac2(arg1) := foo(arg1,myMacro(bar1, arg2))
        define mac3(arg1, arg2) := foo(mac2(arg2), myMacro(b, c))
        define s := foo(myMacro(1,2),arg2)
     """

  val testSpec4: String = """
        define mac2(arg1) := bar1
        define mac3(arg1) := mac2(xyz)
     """

  val testSpec5: String = """
        --define myMacro(arg1, arg2) := arg1
        --define mac2(arg1) := foo(arg1,myMacro(bar1, arg2))
        define mac3(arg1) := mac2(xyz)
        define mac2(arg1) := ar1        
     """
  val testSpec6: String = """
        --define myMacro(arg1, arg2) := arg1
        --define mac2(arg1) := foo(arg1,myMacro(bar1, arg2))        
        define mac2(arg1) := ar1        
        define mac3(arg1) := mac2(xyz)
     """

  val testSpec7: String = """
        define mac1(arg1, arg2) := a(arg1,b(arg2,bar))
        define mac2(arg1) := c(arg1,mac1(arg1, c2))   
        define mac3(arg1) := e(mac2(d1),mac1(d2,d3))     
        define mac3(arg1) := d(mac2(d1),mac1(d2,d3))        
        define mac4(arg1) := e(mac2(d1),mac1(d2,d3))
     """

  val testSpec8: String = """           
        define mac3(arg1) := e(mac2(d1),mac1(d2,d3))     
        define mac3(arg1) := d(mac2(d1),mac1(d2,d3))        
        define mac4(arg1) := e(mac2(d1),mac1(d2,d3))
        define mac1(arg1, arg2) := a(arg1,b(arg2,bar))
        define mac2(arg1) := c(arg1,mac1(arg1, c2))
     """

  val testSpec9 = """        
        define mac2(arg1) := c(arg1,mac1(arg1, c2))
        define mac1(arg1, arg2) := a(arg1,b(arg2,bar))
       """

  val testSpec10 = """
       -- ordering of declaration must not matter (except for redefinitions)
        define mac1(arg1) := a(arg1)
        define mac2(arg1) := mac1(x)
        
        define mac4(arg1) := mac3(x)
        define mac3(arg1) := a(arg1)
       """

  val testSpec11 = """
       -- cyclic definitions
        define mac3(arg1) := mac2(z)
        define mac2(arg1) := mac1(arg1)
        define mac1(arg1) := mac3(x)
       """
  val testSpec12 = """
      -- macros should be resolved recursively, 
      -- in macro definitions as well as in stream definitions
      define mac(arg1) := f(arg1)
      define mac2(x) := mac(mac(zzz))      
      define b := mac(mac(zzz))
      define a := mac2(b)
       """

}

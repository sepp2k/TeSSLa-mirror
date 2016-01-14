package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Parser._
import de.uni_luebeck.isp.tessla.AST._
import scala.util.{ Try, Failure }
import de.uni_luebeck.isp.tessla.Compiler.Tree
import de.uni_luebeck.isp.tessla.Compiler.UnexpectedCompilerState

/**
 * @author Normann Decker <decker@isp.uni-luebeck.de>
 */
object MacroResolution extends Compiler.Pass {

  def applyPass(compiler: Compiler, state: Compiler.State): Try[Compiler.State] = {
    state match {
      case Tree(spec) => resolveMacros(spec) map Tree
      case _          => Failure[Compiler.State](UnexpectedCompilerState)
    }
  }

  case class CyclicDefinitionError(val macroDef: MacroDef) extends Exception

  /**
   * Allows for conveniently instantiating a macro definition.
   */
  implicit class ApplMacroDef(macroDef: MacroDef) {
    /**
     * Substitutes parameter names by terms according to a given binding.
     */
    private def substParam(binding: Map[String, TreeTerm], t: TreeTerm): TreeTerm =
      t match {
        case TreeTerm(UnresolvedTerm(name, typ)) if binding contains name => binding(name)
        case TreeTerm(term) => TreeTerm(term.map { substParam(binding, _) })
      }

    def apply(formalArgs: List[TreeTerm]): TreeTerm =
      substParam(macroDef.args.zip(formalArgs).toMap: Map[String, TreeTerm], macroDef.definition)
  }

  /**
   * Extracts, applies and removes macro definitions from and to a given
   * specification. Returns either a processed specification with all macros
   * applied or a macro error carrying one of the macro definitions causing failure.
   */
  def resolveMacros(in: Spec): Try[Spec] =
    for (m <- extractMacros(in)) yield {
      val statements = in.statements.filter(!_.isInstanceOf[MacroDef])
      substituteMacros(m, Spec(statements))
    }

  /**
   * Substitutes a given set of macro definitions in a given specification. The
   * macro definitions are not processed themselves.
   */
  def substituteMacros(macros: Set[MacroDef], in: Spec): Spec = {
    val macroMap = macros.map { md => ((md.name, md.args.length) -> md) }.toMap
    substituteMacros(macroMap, in)
  }

  /**
   * Substitutes a given set of macro definitions in a given specification. The
   * macro definitions are not processed themselves.
   */
  def substituteMacros(macros: Map[(String, Int), MacroDef], in: Spec): Spec = {
    val substStatements = in.statements.map { s =>
      s match {
        case Def(name, definition) => Def(name, substituteMacros(macros, definition))
        case _                     => s
      }
    }
    Spec(substStatements)
  }

  /**
   * Performs the substitution defined by a set of given macros on a given term.
   * The macro definitions are not processed or applied to themselves.
   */
  def substituteMacros(macros: Map[(String, Int), MacroDef], in: TreeTerm): TreeTerm = in match {
    case TreeTerm(App(UnresolvedFunction(name), args: List[TreeTerm], List(), typ)) if (macros contains (name, args.length)) => {
      val substArgs = args.map { arg => substituteMacros(macros, arg) }
      macros(name, args.length)(substArgs)
    }
    case TreeTerm(t) => TreeTerm(t.map { substituteMacros(macros, _) })
  }

  /**
   * Extracts macro definitions from specification and resolves mutual calls.
   * Throws an exception if any subset of definitions is cyclic.
   */
  def extractMacros(s: Spec): Try[Map[(String, Int), MacroDef]] = {

    type MacroKey = (String, Int)
    type MacroMap = Map[MacroKey, MacroDef]

    /**
     * Encapsulates a stateful macro resolution procedure that resolves nested
     * macro calls and detects cyclic definitions in linear time (in the number
     * of macros). Upon recognising a cyclic definition an Exception is thrown.
     */

    var unprocessed = s.statements.collect {
      case MacroDef(name, args, definition) => ((name, args.length) -> MacroDef(name, args, definition))
    }.toMap

    var processed: MacroMap = Map()

    def updateReduceMacroDef(inuse: List[MacroKey], currentMacro: MacroDef): MacroDef = {
      MacroDef(currentMacro.name, currentMacro.args, updateReduceTerm(inuse, currentMacro, currentMacro.definition))
    }

    def updateReduceTerm(inuse: List[MacroKey], currentMacro: MacroDef, t: TreeTerm): TreeTerm =
      t match {
        case TreeTerm(UnresolvedTerm(name, typ)) if (inuse contains (name, 0)) =>
          throw new CyclicDefinitionError(currentMacro)
        case TreeTerm(loc@UnresolvedTerm(name, typ)) if (!currentMacro.args.contains(name)) => {
          if (processed contains (name, 0)) {
            processed(name, 0).definition
          } else if (unprocessed contains (name, 0)) {
            val m = unprocessed(name, 0)
            val update = updateReduceMacroDef((m.name, m.args.length) :: inuse, m)
            processed += ((m.name, m.args.length) -> update)
            unprocessed -= ((m.name, m.args.length))
            update.definition
          } else t
        }
        case TreeTerm(loc@App(fn, arguments: List[TreeTerm], namedArguments, typ)) => {
          val reducedArgs = arguments.map { arg => updateReduceTerm(inuse, currentMacro, arg) }
          val reducedNArgs = namedArguments.map { case NamedArg(name, definition) => NamedArg(name, updateReduceTerm(inuse, currentMacro, definition)) }

          fn match {
            case UnresolvedFunction(name) => {
              if (inuse contains (name, arguments.length)) {
                throw new CyclicDefinitionError(currentMacro)
              } else if (processed contains (name, arguments.length)) {
                processed(name, arguments.length)(reducedArgs)
              } else if (unprocessed contains (name, arguments.length)) {
                val m = unprocessed(name, arguments.length)
                val update = updateReduceMacroDef((m.name, m.args.length) :: inuse, m)
                processed += ((m.name, m.args.length) -> update)
                unprocessed -= ((m.name, m.args.length))
                update(reducedArgs)
              } else {
                TreeTerm(App(fn, reducedArgs, reducedNArgs, typ))
              }
            }
            case _ => TreeTerm(App(fn, reducedArgs, reducedNArgs, typ))
          }
        }
        case TreeTerm(term) => TreeTerm(term.map { updateReduceTerm(inuse, currentMacro, _) })
      }

    Try {
      while (!unprocessed.isEmpty) {
        val update = updateReduceMacroDef(List(unprocessed.head._1), unprocessed.head._2)
        processed += ((update.name, update.args.length) -> update)
        unprocessed -= ((update.name, update.args.length))
      }
      processed
    }
  }
}
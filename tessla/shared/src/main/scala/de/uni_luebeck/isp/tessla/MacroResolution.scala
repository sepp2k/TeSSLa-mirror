package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Parser._
import de.uni_luebeck.isp.tessla.AST._

/**
 * @author Normann Decker <decker@isp.uni-luebeck.de>
 */
object MacroResolution {
  /**
   * Allows for conveniently instantiating a macro definition.
   */
  implicit class ApplMacroDef(macroDef: MacroDef) {
    /**
     * Substitutes parameter names by terms according to a given binding.
     */
    private def substParam(binding: Map[String, Term], t: Term): Term =
      t match {
        case UnresolvedTerm(name, typ) if binding contains name => binding(name)
        case App(fn: Function, arguments: List[Term], namedArguments: List[Def], typ: Type) => {
          val substNamedArgs = namedArguments.map {
            case Def(name: String, definition: Term) => Def(name, substParam(binding: Map[String, Term], definition))
          }
          val substArgs = arguments.map { arg => substParam(binding, arg) }
          App(fn, substArgs, substNamedArgs, typ)
        }
        case _ => t
      }

    def apply(formalArgs: List[Term]): Term = substParam(macroDef.args.zip(formalArgs).toMap, macroDef.definition)
  }

//  implicit def toApplMacroDef(m: MacroDef): ApplMacroDef =
//    m match { case MacroDef(_, args, definition) => new ApplMacroDef(args, definition) }

  /**
   * Extracts, applies and removes macro definitions from and to a given
   * specification.
   */
  def resolveMacros(in: Spec): Spec = {
    val macros = extractMacros(in)
    val statements = in.statements.filter { s =>
      s match {
        case MacroDef(_, _, _) => false
        case _                 => true
      }
    }
    substituteMacros(macros, Spec(statements))
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
  def substituteMacros(macros: Map[(String, Int), MacroDef], in: Term): Term = in match {
    case App(UnresolvedFunction(name), args, List(), typ) if (macros contains (name, args.length)) => { macros(name, args.length)(args) }
    case App(f, args, nargs, typ) => {
      val substArgs = args.map { arg => substituteMacros(macros, arg) }
      val substNArgs = nargs.map { case Def(name, definition) => Def(name, substituteMacros(macros, definition)) }
      App(f, substArgs, substNArgs, typ)
    }
    case TypeAscr(term: Term, typ: Type) => TypeAscr(substituteMacros(macros, term), typ)
    case _                               => in
  }

  /**
   * Extracts macro definitions from specification and resolves mutual calls.
   * Throws an exception if any subset of definitions is cyclic.
   */
  def extractMacros(s: Spec): Map[(String, Int), MacroDef] = {

    type MacroKey = (String, Int)
    type MacroMap = Map[MacroKey, MacroDef]

    /**
     * Encapsulates a stateful macro resolution procedure that resolves nested
     * macro calls and detects cyclic definitions in linear time (in the number
     * of macros). Upon recognising a cyclic definition an Exception is thrown.
     */
    class MacroProcessor(macros: MacroMap) {

      private var unprocessed = macros
      private var processed: MacroMap = Map()

      val processedMacros: Map[(String, Int), MacroDef] = {
        while (!unprocessed.isEmpty) {
          val update = updateReduce(List(unprocessed.head._1), unprocessed.head._2)
          processed += ((update.name, update.args.length) -> update)
          unprocessed -= ((update.name, update.args.length))
        }
        processed
      }

      private def updateReduce(inuse: List[MacroKey], currentMacro: MacroDef): MacroDef = {
        MacroDef(currentMacro.name, currentMacro.args, updateReduce(inuse, currentMacro, currentMacro.definition))
      }

      private def updateReduce(inuse: List[MacroKey], currentMacro: MacroDef, t: Term): Term =
        t match {
          case UnresolvedTerm(name, typ) if (inuse contains (name, 0)) =>
            throw new Exception("Definition of macro " + currentMacro.name + " is cyclic.")
          case UnresolvedTerm(name, typ) if (!currentMacro.args.contains(name)) => {
            if (processed contains (name, 0)) {
              processed(name, 0).definition
            } else if (unprocessed contains (name, 0)) {
              val m = unprocessed(name, 0)
              val update = updateReduce((m.name, m.args.length) :: inuse, m)
              processed += ((m.name, m.args.length) -> update)
              unprocessed -= ((m.name, m.args.length))
              update.definition
            } else t
          }
          case TypeAscr(term, typ) => TypeAscr(updateReduce(inuse, currentMacro, term), typ)
          case App(fn, arguments, namedArguments, typ) => {
            // TODO: Concurrency/Race Conditions?
            val reducedArgs = arguments.map { arg => updateReduce(inuse, currentMacro, arg) }
            val reducedNArgs = namedArguments.map { case Def(name, definition) => Def(name, updateReduce(inuse, currentMacro, definition)) }

            fn match {
              case UnresolvedFunction(name) => {
                if (inuse contains (name, arguments.length)) {
                  throw new Exception("Definition of macro " + currentMacro.name + " is cyclic.")
                } else if (processed contains (name, arguments.length)) {
                  processed(name, arguments.length)(reducedArgs)
                } else if (unprocessed contains (name, arguments.length)) {
                  val m = unprocessed(name, arguments.length)
                  println("unprocessed m=" + m)
                  val update = updateReduce((m.name, m.args.length) :: inuse, m)
                  println("processed m=" + update)
                  processed += ((m.name, m.args.length) -> update)
                  unprocessed -= ((m.name, m.args.length))
                  update(reducedArgs)
                } else {
                  App(fn, reducedArgs, reducedNArgs, typ)
                }
              }
              case _ => App(fn, reducedArgs, reducedNArgs, typ)
            }
          }
          case _ => t
        }
    }

    val macros = s.statements.collect {
      case MacroDef(name, args, definition) => ((name, args.length) -> MacroDef(name, args, definition))
    }.toMap

    new MacroProcessor(macros).processedMacros
  }
}
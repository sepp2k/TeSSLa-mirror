package de.uni_luebeck.isp.tessla

import scala.util.{Try, Success}
import scala.collection.mutable.{Map => MutableMap}

object MacroResolver extends CompilerPass[Definitions, Definitions] {
//  override def apply(compiler: Compiler, defs: Definitions) = Try {
 //   assert(defs.macroDefs.isEmpty, "macro substitution not implement yet")
 //   defs
  //}

  case class CyclicMacroDefinitionError(inMacro: MacroDef, atLocation: NestedLoc) extends Fatal

  /**
    * Allows for conveniently instantiating a macro definition.
    */
  implicit class ApplMacroDef(macroDef: MacroDef) {
    /**
      * Substitutes parameter names by terms according to a given binding.
      */
    private def substParam(binding: Map[String, ExprTree], expr: ExprTree): ExprTree =
      expr match {
        case ExprTree(NamedFn(name: String, locFn: NestedLoc), args: Map[ArgName, ExprTree], loc: NestedLoc) =>
          if (args.isEmpty && (binding contains name))
            binding(name)
          else
            expr.copy(args = args.mapValues(substParam(binding, _)))
        case ExprTree(_, args, _) => expr.copy(args = args.mapValues(substParam(binding, _)))
      }

    def apply(binding: Map[String, ExprTree]): ExprTree =
      substParam(binding, macroDef.streamDef.expr)
  }


  override def apply(compiler: Compiler, defs: Definitions) = Try {

    var macros: MutableMap[String, (Boolean, MacroDef)] = MutableMap() ++ defs.macroDefs.mapValues(md => (false, md))

    def substituteMacroCalls(substitutionStack: Seq[String], exprTree: ExprTree): ExprTree = exprTree match {
      case ExprTree(NamedFn(name: String, locFn: NestedLoc), args: Map[ArgName, ExprTree], loc: NestedLoc) if (macros contains name) => {
        //print("substituteMacroCalls: substitutionStack = ")
        //println(substitutionStack)

        //println(s"1: ${macros(name)}")
        //val test = macros(name)._2.streamDef.expr.args.keys
        //println(s"TEST1: $name")
        //println(s"TEST2: $test")



        if (substitutionStack contains name) {
          val diag = CyclicMacroDefinitionError(macros(substitutionStack.head)._2, loc)
          compiler.diagnostic(diag)
          //print("substituteMacroCalls: compiler.diagnostics = ")
          //println(compiler.diagnostics)
          //return None
          throw diag
        }
        val calledMacro: MacroDef = macros(name)._2
        val binding: Map[String, ExprTree] = args.toSeq.map {
          case (argName, expr) => {
            val n: String = argName match {
              case Pos(pos) => calledMacro.args(pos)._1
              case Named(argName) => argName
            }
            (n, substituteMacroCalls(substitutionStack,expr))
          }
        }.toMap

        if (!macros(name)._1) {
          //println(s"substituteMacroCalls: !macros(${name}._1)")
          //calledMacro is not yet substituted
          val flatMacroExpr: ExprTree = substituteMacroCalls(name +: substitutionStack, calledMacro.streamDef.expr)
          val flatMacro =  calledMacro.copy(streamDef = calledMacro.streamDef.copy(expr = flatMacroExpr))
          //println(s"substituteMacroCalls: flatMacro = $flatMacro")
          macros += ((name,(true, flatMacro)))
        }
        //println(s"2: $macros")

        macros(name)._2(binding)
      }
      case _ => exprTree.copy(args = exprTree.args.mapValues(substituteMacroCalls(substitutionStack, _)))
    }

    val substitutedStreamDefs = {
      for ((name, streamDef) <- defs.streamDefs) yield {
        //print("MacroResolver: (name, streamDef) = ")
        //println(name)
        //println(streamDef)
        val flatExpr = substituteMacroCalls(Seq(), streamDef.expr)
        (name, streamDef.copy(expr = flatExpr))
      }
    }.toSeq.toMap

    Definitions(substitutedStreamDefs, Map())
  }
}

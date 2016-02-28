package de.uni_luebeck.isp.tessla

import scala.collection.mutable.{Map => MutableMap}
import scala.util.Try

object MacroResolver extends CompilerPass[Definitions, Definitions] {

  case class CyclicMacroDefinitionError(inMacro: MacroDef, atLocation: NestedLoc) extends Fatal

  /**
    * Allows for conveniently instantiating a macro definition.
    */
  implicit class ApplMacroDef(macroDef: MacroDef) {
    def apply(binding: Map[String, ExprTree]): ExprTree = substParam(binding, macroDef.streamDef.expr)

    private def substParam(binding: Map[String, ExprTree], expr: ExprTree): ExprTree =
      expr match {
        case ExprTree(NamedFn(name: String, locFn: NestedLoc), args: Map[ArgName, ExprTree], loc: NestedLoc) =>
          if (args.isEmpty && (binding contains name))
            binding(name)
          else
            expr.copy(args = args.mapValues(substParam(binding, _)))
        case ExprTree(_, args, _) => expr.copy(args = args.mapValues(substParam(binding, _)))
      }
  }

  override def apply(compiler: Compiler, defs: Definitions) = Try {

    // macro definitions to be updated on the fly when processing expressions
    val macroDefs: MutableMap[String, MacroDef] = MutableMap() ++ defs.macroDefs
    val macroFlat: MutableMap[String, Option[MacroDef]] = MutableMap()


    def flattenMacroDef(macroCallStack: List[String], macroDef: MacroDef): Unit = {
      val macroName: String = macroDef.streamDef.name
      val flatMacroDef: Option[MacroDef] = flatten(macroName :: macroCallStack, macroDef.streamDef.expr) map {
        flatExpr => macroDef.copy(streamDef = macroDef.streamDef.copy(expr = flatExpr))
      }
      macroDefs -= macroName
      macroFlat += (macroName -> flatMacroDef)
    }

    def flatten(macroCallStack: List[String], exprTree: ExprTree): Option[ExprTree] = exprTree match {
      case ExprTree(fn: ExprTreeFn, args: Map[ArgName, ExprTree], loc: NestedLoc) =>
        // always process all arguments to catch as many errors as possible
        val flatArgs: Map[ArgName, Option[ExprTree]] = args.mapValues(flatten(macroCallStack, _))
        // process actual expression
        fn match {
          case NamedFn(name: String, locFn: NestedLoc) =>
            if (macroCallStack contains name) {
              compiler.diagnostic(CyclicMacroDefinitionError(macroDefs(macroCallStack.head), loc))
              None
            } else {
              if (macroDefs contains name) flattenMacroDef(macroCallStack, macroDefs(name))
              if (flatArgs.values.toSet contains None) {
                None
              } else if (macroFlat contains name) {
                // instantiate macro if Some(macroDef)
                macroFlat(name).map { macroDef => {
                  val binding: Map[String, ExprTree] = flatArgs.toSeq.map {
                    case (argName, expr) =>
                      val n: String = argName match {
                        case Pos(pos) => macroDef.args(pos)._1
                        case Named(s) => s
                      }
                      (n, expr.get)
                  }.toMap
                  macroDef(binding)
                }
                }
              } else {
                Some(exprTree.copy(args = flatArgs.mapValues(_.get)))
              }
            }
          case _ =>
            if (flatArgs.values.toSet contains None) None
            else Some(exprTree.copy(args = flatArgs.mapValues(_.get)))
        }
    }

    val substitutedStreamDefs: Map[String, StreamDef] = defs.streamDefs.flatMap {
      case (name, streamDef) =>
        flatten(List[String](), streamDef.expr) map { flatExpr => (name, streamDef.copy(expr = flatExpr)) }
    }

    // macro definitions are removed because they should not be of interest afterwards and
    // it makes this pass idempotent
    Definitions(substitutedStreamDefs, Map())

  }
}

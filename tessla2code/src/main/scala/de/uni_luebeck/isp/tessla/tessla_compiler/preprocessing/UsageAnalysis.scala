package de.uni_luebeck.isp.tessla.tessla_compiler.preprocessing

import de.uni_luebeck.isp.tessla.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.ExtendedSpecification

/**
 * Translates a TeSSLa Core AST to an [[ExtendedSpecification]] (AST + usage information)
 */
class UsageAnalysis extends TranslationPhase[Specification, ExtendedSpecification] {

  /**
   * Function triggering the translation from a simple TeSSLa Core Specification to an [[ExtendedSpecification]]
   * with usage information (i.e. which variable is used in which definitions)
   * @param spec The TeSSLa Core specification to be examined
   * @return Core AST plus usage information
   */
  override def translate(spec: Specification): Result[ExtendedSpecification] = {

    var usages: collection.mutable.HashMap[Identifier, Set[Identifier]] = collection.mutable.HashMap()

    def findUsages(ea: ExpressionArg, usedIn: Identifier): Unit = {
      ea match {
        case RecordAccessorExpression(_, target, _, _)   => findUsages(target, usedIn)
        case FunctionExpression(_, _, body, result, _)   => findUsagesInDefs(body); findUsages(result, usedIn)
        case ApplicationExpression(applicable, args, _)  => (args :+ applicable).foreach(findUsages(_, usedIn))
        case TypeApplicationExpression(applicable, _, _) => findUsages(applicable, usedIn)
        case RecordConstructorExpression(entries, _)     => entries.foreach(e => findUsages(e._2._1, usedIn))
        case ExpressionRef(id, _, _)                     => usages += (id -> (usages.getOrElse(id, Set()) + usedIn))
        case _                                           =>
      }
    }

    def findUsagesInDefs(defs: Map[Identifier, DefinitionExpression]): Unit = {
      defs.foreach {
        case (id, d) =>
          if (!usages.contains(id)) {
            usages += (id -> Set())
          }
          findUsages(d, id)
      }
    }

    findUsagesInDefs(spec.definitions)

    Success(ExtendedSpecification(spec, Some(usages.toMap), None, None), Seq())
  }

}

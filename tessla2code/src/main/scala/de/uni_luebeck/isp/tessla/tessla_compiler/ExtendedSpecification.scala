package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core._

/**
 * TeSSLa Core AST with additional information from the preprocessing
 * @param spec The TeSSLa Core AST
 * @param usageInfo Map indication which Identifier is used in the definitions of other identifiers
 * @param lazyVars Set of all variables that shall be translated with lazy assignments
 * @param inlining Set of all variables which are not translated as variables but inlined (must be final variables)
 */
case class ExtendedSpecification(
  spec: Specification,
  usageInfo: Option[Map[Identifier, Set[Identifier]]],
  lazyVars: Option[Set[Identifier]],
  inlining: Option[Set[Identifier]]
)

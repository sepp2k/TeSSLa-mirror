package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.TesslaAST.Core._

case class ExtendedSpecification(
  spec: Specification,
  usageInfo: Option[Map[Identifier, Set[Identifier]]],
  lazyVars: Option[Set[Identifier]],
  inlining: Option[Set[Identifier]]
)

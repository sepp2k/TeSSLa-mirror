/*
 * Copyright 2022 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._

/**
 * TeSSLa Core AST with additional information from the preprocessing
 * @param spec The TeSSLa Core AST
 * @param usageInfo Map indication which Identifier is used in the definitions of other identifiers
 * @param lazyVars Set of all variables that shall be translated with lazy assignments
 * @param inlining Set of all variables which are not translated as variables but inlined (must be final variables)
 * @param traitInfo Set of generic types, that need additional traits in Rust
 */
case class ExtendedSpecification(
  spec: Specification,
  usageInfo: Option[Map[Identifier, Set[Identifier]]],
  lazyVars: Option[Set[Identifier]],
  inlining: Option[Set[Identifier]],
  traitInfo: Option[Map[Identifier, Set[String]]]
)

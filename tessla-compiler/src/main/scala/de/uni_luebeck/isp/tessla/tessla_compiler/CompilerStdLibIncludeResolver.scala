/*
 * Copyright 2021 The TeSSLa Community
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

import de.uni_luebeck.isp.tessla.core.IncludeResolvers
import de.uni_luebeck.isp.tessla.core.IncludeResolvers.fromResource
import org.antlr.v4.runtime.CharStream

/**
 * Object for providing an IncludeResolver (see [[IncludeResolvers]]) that enables overwriting stdlib parts in
 * a compiler-specific way.
 */
object CompilerStdLibIncludeResolver {

  /**
   * Include resolver that looks up all includes in the compiler's resource folder first and then in the
   * stdlib folder from the core project
   * @return Compiler-specific include resolver
   */
  def fromCompilerStdlibResource: String => Option[CharStream] =
    fromResource(
      Seq(
        (this.getClass, "/de/uni_luebeck/isp/tessla/stdlib/compiler"),
        (IncludeResolvers.getClass, "/de/uni_luebeck/isp/tessla/stdlib")
      )
    )

}

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

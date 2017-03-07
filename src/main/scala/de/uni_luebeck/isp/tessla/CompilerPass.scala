package de.uni_luebeck.isp.tessla

import scala.util.Try

trait CompilerPass[I, O] {
  def apply(compiler: Compiler, input: I): Try[O]
  def passDescription: String = this.getClass.getSimpleName.dropRight(1)
}

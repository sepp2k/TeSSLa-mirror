package de.uni_luebeck.isp.tessla

import util.Try

trait CompilerPass[I, O] {
  def apply(input: I): Try[O]
  def passDescription: String = this.getClass.getSimpleName.dropRight(1)
}

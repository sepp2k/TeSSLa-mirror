package de.uni_luebeck.isp.tessla

import util.Try

abstract class CompilerPass[I,O] {
  def applyPass(input: I): Try[O] = ???
  def passDescription: String = this.getClass.getSimpleName.dropRight(1)
}

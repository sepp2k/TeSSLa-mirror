package de.uni_luebeck.isp.tessla

trait CompilerPass[I, O] {
  def apply(compiler: Compiler, input: I): O
  def passDescription: String = this.getClass.getSimpleName.dropRight(1)
}

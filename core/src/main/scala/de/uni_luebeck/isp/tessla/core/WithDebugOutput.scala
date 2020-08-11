package de.uni_luebeck.isp.tessla.core

trait WithDebugOutput {
  def debugOutput: String = this.toString
}
